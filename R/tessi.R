#' tessi_list_tables
#'
#' The list of Tessitura tables is configured in the extdata/tessi_tables.yml file in the package directory
#' supplemented by the tessi_tables dictionary in config.yml
#'
#' ## yml format
#' ```
#' {short_name}:
#'    long_name: {name of table/view to be loaded}
#'    base_table: {the underlying table being queried that has primary_keys}
#'    primary_keys: {the primary key(s) as a value or a list of values}
#' ````
#'
#' @return  data.table of configured Tessitura tables with columns short_name, long_name, base_table and primary_keys
#' @importFrom yaml read_yaml
#' @importFrom purrr map
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#' # customers:
#' #   long_name: BI.VT_CUSTOMER
#' #   base_table: T_CUSTOMER
#' #   primary_keys: customer_no
#'
#' tessi_list_tables()[short_name == "customers"]
#'
tessi_list_tables <- function() {
  config_tessi_tables <- NULL

  try({
      config_default <- config::get()
      config_tessi_tables <- config::get(config = "tessi_tables")
      config_tessi_tables <- config_tessi_tables[setdiff(names(config_tessi_tables),names(config_default))]
    },
    silent = TRUE
  )

  config::merge(
    read_yaml(system.file("extdata", "tessi_tables.yml", package = "tessilake")),
    config_tessi_tables
  ) %>%
    rbindlist(idcol = "short_name", fill = TRUE) %>%
    # get rid of blanks
    map(~str_replace_all(.,c("^$"=NA_character_,"\n"=" "))) %>%
    setDT() -> ret
  ret
}


#' read_tessi
#'
#' Thin wrapper on read_sql_table using the tables configured in `list_tessi_tables` that also :
#' * updates `customer_no` based on merges and adds `group_customer_no` based on [tessi_customer_no_map()]
#'
#' @param table_name character name of the table to read from Tessitura, either one of the available tables (see [tessi_list_tables()]) or the
#' name of a SQL table that exists in Tessitura. The default SQL table schema is `dbo`.
#' @param select vector of strings indicating columns to select from database
#' @param freshness the returned data will be at least this fresh
#' @param incremental whether or not to load data incrementally, default is `TRUE`
#' @param ... further arguments to be passed to read_sql_table
#'
#' @return an Apache [arrow::Table].
#' @importFrom rlang enexpr call_match
#' @importFrom stringr str_split
#' @importFrom dplyr mutate_if
#' @export
#'
#' @examples
#' \dontrun{
#' read_tessi("memberships",
#'   select = c("memb_level", "customer_no")
#' )
#' }
#'
read_tessi <- function(table_name, select = NULL,
                       freshness = as.difftime(7, units = "days"),
                       incremental = TRUE, ...) {
  short_name <- customer_no <- merged_customer_no <- NULL

  select <- enexpr(select)
  assert_character(table_name)
  assert_names(table_name, subset.of = tessi_list_tables()$short_name)

  table_data <- tessi_list_tables()[short_name == table_name] %>% as.list()

  table_data$long_name <- str_split(table_data$long_name, stringr::fixed("."), n = 2)[[1]]

  args <- rlang::list2(...)
  if (any(!is.na(table_data$primary_keys))) {
    args$primary_keys <- table_data$primary_keys
  }
  if (!is.na(table_data$date_column[[1]])) {
    args$date_column <- table_data$date_column[[1]]
  }
  args$select <- expr_get_names(select)
  args$freshness <- freshness
  args$incremental <- incremental

  if(!is.na(table_data$query[[1]])) {
    args$query <- table_data$query[[1]]
    args$name <- paste(table_data$long_name,collapse=".")
    table <- do.call(read_sql, args)
  } else {
    if (length(table_data$long_name) == 1) {
      args$table_name <- table_data$long_name[[1]]
    } else {
      args$schema <- table_data$long_name[[1]]
      args$table_name <- table_data$long_name[[2]]
    }
    table <- do.call(read_sql_table, args)
  }

  if("customer_no" %in% colnames(table))
     table <- table %>% merge_customer_no_map("customer_no",freshness)

  if("creditee_no" %in% colnames(table))
     table <- table %>% merge_customer_no_map("creditee_no",freshness)

  return(collect(table, as_data_frame = FALSE))
}

#' merge_customer_no_map
#'
#' @param table data.table to merge with tessi_customer_no_map
#' @param column string column name to merge with
#' @param freshness difftime
#'
#' @return merged data.table
#' @importFrom dplyr rename_with contains
merge_customer_no_map <- function(table,column,
                                  freshness = as.difftime(7, units = "days")) {

  assert_names(names(table),must.include = column)

  # add group columns
  map <- tessi_customer_no_map(freshness) %>% rename_with(~gsub("customer_no",column,.))

  table <- table %>% left_join(map, by = column, copy = T)
  # only change column to kept (merged) column if it's not a primary key
  table <-
    if (!column %in% cache_get_attributes(table)$primary_keys) {
      table %>%
        select(-all_of(column)) %>%
        rename(!!column := !!sym(paste0("merged_",column)))
    } else {
      table
    }

}

#' tessi_customer_no_map
#'
#' Return an [arrow::Table] of customer numbers `customer_no` mapped to merged `merged_customer_no` and household/primary group
#' customer numbers `group_customer_no`.
#'
#' @param freshness the returned data will be at least this fresh
#' @importFrom dplyr full_join left_join filter select mutate collect rename coalesce
#' @importFrom arrow is_in
#' @return [arrow::Table]
#' @export
tessi_customer_no_map <- function(freshness = as.difftime(7, units = "days")) {
  kept_id <- kept_id_old <- merged_customer_no <- customer_no <- group_customer_no <- NULL

  customers <- read_sql("select customer_no from T_CUSTOMER",
    "customers",
    freshness = freshness
  )

  merges <- read_sql("select kept_id, delete_id from T_MERGED where status='S' and kept_id<>delete_id",
    "merges",
    freshness = freshness
  ) %>%
    distinct() %>%
    collect(as_data_frame = FALSE)

  affiliations <- read_sql("select individual_customer_no, group_customer_no from T_AFFILIATION where primary_ind='Y' and inactive='N'",
    "affiliations",
    freshness = freshness
  )

  merge_recursive <- function(m) {
    if (!any(m$kept_id %>% is_in(m$delete_id))$as_vector()) {
      return(m)
    } else {
      m %>%
        rename(kept_id_old = kept_id) %>%
        left_join(merges, by = c("kept_id_old" = "delete_id")) %>%
        mutate(kept_id = coalesce(kept_id, kept_id_old)) %>%
        select(-kept_id_old) %>%
        collect(as_data_frame = FALSE) %>%
        merge_recursive()
    }
  }

  map <- customers %>%
    full_join(merge_recursive(merges), by = c("customer_no" = "delete_id")) %>%
    rename(merged_customer_no = kept_id) %>%
    mutate(merged_customer_no = coalesce(merged_customer_no, customer_no)) %>%
    left_join(affiliations, by = c("merged_customer_no" = "individual_customer_no")) %>%
    mutate(group_customer_no = coalesce(group_customer_no, merged_customer_no)) %>%
    collect(as_data_frame = FALSE)
}
