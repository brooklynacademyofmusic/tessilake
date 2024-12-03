
#' sql_connect
#'
#' Connects and disconnects from the SQL database using the `tessilake.tessitura` connection string in config.yml and
#' updates the Rstudio connection pane
#'
#' @return invisible NULL
#' @importFrom rlang ns_env env_bind
#'
#' @examples
#' \dontrun{
#' tessilake:::db$db
#' sql_connect()
#' tessilake:::db$db
#' sql_disconnect()
#' }
sql_connect <- function() {
  if (is.null(db$db)) {
    tryCatch({
        args <- list(drv = odbc::odbc(),
                     dsn = config::get("tessilake")[["tessitura"]],
                     encoding = config::get("tessilake")[["tessitura.encoding"]] %||% "")

        callbacks <- getTaskCallbackNames()
        db$db <- do.call(DBI::dbConnect,args)
        removeTaskCallback(setdiff(getTaskCallbackNames(), callbacks))

        if ("odbc.version" %in% names(attributes(db$db))) {
          odbc:::on_connection_opened(db$db, deparse(db_expr))
        }
      },
      error = function(e) {
        message(e$message)
        stop("Database connection failed, please set the `tessilake.tessitura` configuration in config.yml to a valid database DSN.")
      }
    )
  }
  invisible(NULL)
}

#' @describeIn sql_connect Tear down the SQL connection
sql_disconnect <- function() {
  DBI::dbDisconnect(db$db)
  db$db <- NULL
}

db <- new.env(parent = emptyenv())

#' read_sql
#'
#' Execute a database query and cache it locally as a Feather file and remotely as a Parquet file.
#' Cache storage locations are managed by `tessilake.depths` options.
#' Database connection defined by an ODBC profile with the name set by the `tessilake.tessitura` option.
#'
#' @param query character query to run on the database.
#' @param name name of the query, defaults to the SHA1 hash of the query.
#' @param select vector of strings indicating columns to select from database
#' @param primary_keys primary keys of the query to be used for incremental updates
#' @param date_column update date column of the query to be used for incremental updates
#' @param freshness the returned data will be at least this fresh
#' @param incremental whether or not to load data incrementally, default is `TRUE`
#'
#' @return an Apache [arrow::Table]
#' @importFrom arrow arrow_table
#' @importFrom checkmate assert_character
#' @importFrom dplyr tbl sql summarise
#' @importFrom dbplyr tbl_sql
#' @importFrom digest sha1
#' @importFrom lubridate tz force_tz is.POSIXct
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom purrr iwalk
#' @export
#'
#' @examples
#' \dontrun{
#' read_sql("select * from T_CUSTOMER", "t_customer",
#'   primary_keys = "customer_no",
#'   date_column = "last_update_dt"
#' )
#' }
read_sql <- function(query, name = digest::sha1(query),
                     select = NULL,
                     primary_keys = NULL, date_column = NULL,
                     freshness = as.difftime(7, units = "days"),
                     incremental = TRUE) {

  assert_character(query, len = 1)
  if (!is.null(date_column)) assert_character(date_column, max.len = 1)
  if (!is.null(primary_keys)) assert_character(primary_keys, min.len = length(date_column))

  sql_connect()
  # build the query with dplyr and sort by primary keys and date column for faster updating
  table <- tbl(db$db, sql(query)) %>% arrange(across(!!c(primary_keys, date_column)))

  test_mtime <- Sys.time() - freshness

  depths <- names(config::get("tessilake")[["depths"]])
  mtimes <- purrr::map_vec(depths, \(depth) cache_get_mtime(name, depth, "tessi"))

  if(all(mtimes < test_mtime))
    # Update caches
    write_cache(table, table_name = name, type = "tessi", incremental = incremental,
                primary_keys = primary_keys, date_column = date_column, partition = FALSE)

  read_cache(table_name = name, type = "tessi", select = select)
}

#' read_sql_table
#'
#' @param schema character, database schema. Default is `dbo`
#' @param table_name character, table name without schema.
#' @param select vector of strings indicating columns to select from database
#' @param primary_keys character vector, primary keys of the table. `read_sql_table` will attempt to identify the primary keys using
#' SQL metadata.
#' @param date_column character, date column of the table showing the last date the row was updated.
#' Defaults to "last_update_dt" if it exists in the table.
#' @param freshness the returned data will be at least this fresh
#' @param incremental whether or not to load data incrementally, default is `TRUE`
#' @describeIn read_sql Reads a table or view from a SQL database and caches it locally using read_sql.
#' @importFrom dplyr filter select collect
#' @importFrom DBI dbListTables
#' @importFrom rlang maybe_missing
#' @importFrom stringr str_split
#' @return an Apache [arrow::Table].
#' @export
#'
#' @examples
#' \dontrun{
#' read_sql_table("T_CUSTOMER")
#' }
read_sql_table <- function(table_name, schema = "dbo",
                           select = NULL,
                           primary_keys = NULL, date_column = NULL,
                           freshness = as.difftime(7, units = "days"),
                           incremental = TRUE) {
  table_schema <- constraint_type <- character_maximum_length <- column_name <- NULL

  assert_character(table_name, len = 1)
  assert_character(schema, len = 1, null.ok = TRUE)
  assert_character(date_column, len = 1, null.ok = TRUE)
  sql_connect()

  available_tables <- list(
    dbo = list(table_name = dbListTables(db$db, schema_name = "dbo")),
    BI = list(table_name = dbListTables(db$db, schema_name = "BI"))
  ) %>%
    rbindlist(idcol = "schema")

  table_name_part <- str_split(table_name, " ", n = 2)[[1]][[1]]

  if (available_tables[
    eval(expr(schema == !!schema & table_name == !!table_name_part)),
    .N
  ] == 0) {
    stop(paste("Table", paste(schema, table_name, collapse = "."), "doesn't exist."))
  }

  available_columns <- read_sql(
    query = "select c.table_schema, c.table_name, c.column_name, cc.constraint_type, c.character_maximum_length from INFORMATION_SCHEMA.COLUMNS c
            left join (select cc.*,CONSTRAINT_TYPE from INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE cc
            join INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc on tc.CONSTRAINT_NAME=cc.CONSTRAINT_name and
            CONSTRAINT_TYPE='PRIMARY KEY') cc on c.TABLE_NAME=cc.TABLE_NAME and c.TABLE_SCHEMA=cc.TABLE_SCHEMA and c.COLUMN_NAME=cc.COLUMN_NAME",
    name = "available_columns",
    freshness = freshness
  ) %>%
    filter(table_schema == schema & table_name == !!table_name_part) %>%
    collect()

  if (!is.null(select)) assert_names(select, subset.of = available_columns$column_name)
  if (!is.null(primary_keys)) assert_names(primary_keys, subset.of = available_columns$column_name)
  if (!is.null(date_column)) assert_names(date_column, subset.of = available_columns$column_name)

  # get primary key info
  if (is.null(primary_keys) && "PRIMARY KEY" %in% available_columns$constraint_type) {
    primary_keys <- filter(available_columns, constraint_type == "PRIMARY KEY")$column_name
  }

  if (is.null(date_column) && !is.null(primary_keys) && "last_update_dt" %in% available_columns$column_name) {
    date_column <- "last_update_dt"
  }

  # arrange columns so that the "long data' is at the end
  # https://stackoverflow.com/questions/60757280/result-fetchresptr-n-nanodbc-nanodbc-cpp2966-07009-microsoftodbc-dri

  max_cols <- available_columns %>% filter(character_maximum_length == -1)
  non_max_cols <- available_columns %>% filter((character_maximum_length != -1 | is.na(character_maximum_length)) &
    !grepl("encrypt", column_name))
  cols <- paste(c(non_max_cols$column_name, max_cols$column_name), collapse = ",")

  # build the table query
  read_sql(
    query = paste("select", cols, "from", paste(schema, table_name, sep = "."), "with (nolock)"),
    name = paste(c(schema, table_name_part), collapse = "."),
    primary_keys = primary_keys,
    date_column = maybe_missing(date_column),
    select = select,
    freshness = freshness,
    incremental = incremental
  )
}
