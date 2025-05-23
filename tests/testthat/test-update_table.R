withr::local_package("mockery")

# expr_get_names ----------------------------------------------------------

test_that("expr_get_names throws error with anything that doesn't work", {
  expect_error(expr_get_names(expr(mean(x))), "list or vector")
  expect_error(expr_get_names(1), "language")
})

test_that("expr_get_names works with strings and symbols", {
  expect_equal(expr_get_names(expr("a")), expr_get_names(expr(a)))
})

test_that("expr_get_names works with vectors and lists", {
  expect_equal(expr_get_names(expr(c(a, b = "c"))), c("a", b = "c"))
  expect_equal(expr_get_names(expr(list("a", b = c))), c("a", b = "c"))
})

# update_table ------------------------------------------------------------

test_that("update_table requires that from and to are given", {
  from <- data.table(x = c(1, 2, 3))
  to <- data.table(y = c(1, 2, 3))
  expect_error(update_table(to=to), "from")
  expect_error(update_table(from), "to")
})

# test_that("update_table requires that primary_keys and date_column refer to columns in from and to", {
#   from <- data.table(x = c(1, 2, 3))
#   to <- data.table(y = c(1, 2, 3))
#   expect_error(update_table(from, to, primary_keys = x), "colnames")
#   expect_error(update_table(from, to, date_column = y), "colnames")
# })

test_that("update_table allows new columns in to", {
  from_pk <- data.table(x = 1, y = 3)
  from_dt <- data.table(x = 4, y = 3)
  to <- data.table(x = c(3, 2, 1))
  # primary keys
  updated <- update_table(from_pk, copy(to), primary_keys = x)
  expect_equal(colnames(updated),c("x","y"))
  expect_setequal(updated$y, c(3,NA,NA))

  # date only
  expect_warning(updated <- update_table(from_dt, copy(to), date_column = x))
  expect_equal(colnames(updated),c("x","y"))
  expect_setequal(updated$y, c(3,NA,NA,NA))

  # do it again with data.frames
  from <- data.frame(x = 1, y = 3)
  # primary keys
  updated <- update_table(from_pk, copy(to), primary_keys = x)
  expect_equal(colnames(updated),c("x","y"))
  expect_setequal(updated$y, c(3,NA,NA))

  # date only
  expect_warning(updated <- update_table(from_dt, copy(to), date_column = x))
  expect_equal(colnames(updated),c("x","y"))
  expect_setequal(updated$y, c(3,NA,NA,NA))

})

test_that("update_table works with tidy-selected columns in primary_keys and date_column", {
  from <- data.table(x = c(1, 2, 3), y = c(4, 5, 6))
  to <- copy(from)
  expect_equal(update_table(from, to, primary_keys = x), from)
  expect_equal(update_table(from, to, primary_keys = c(x)), from)
  expect_equal(update_table(from, to, primary_keys = "x"), from)
  expect_equal(update_table(from, to, primary_keys = c("x")), from)
  expect_equal(update_table(from, to, primary_keys = c(x, y)), from)
  expect_equal(update_table(from, to, primary_keys = c("x", "y")), from)
  expect_equal(update_table(from, to, primary_keys = c(x, "y")), from)
  expect_equal(update_table(from, to, primary_keys = c("x", y)), from)
})

test_that("update_table simply copies from into to when date_column and primary_keys are missing", {
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)]
  # divide the data
  from <- copy(expect)[1:9000]
  to <- copy(expect)[1000:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  expect_equal(update_table(from, to), from)
})


# update_table.data.table -------------------------------------------------

test_that("update_table.data.table updates data.tables incrementally when given primary_keys", {
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)] %>% setorderv(c("x", "y"))
  # divide the data
  from <- copy(expect)[1:9000]
  to <- copy(expect)[1000:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  expect_equal(update_table(from, to, primary_keys = c("x", "y")), expect)
  expect_equal(update_table(from, to, primary_keys = c(x, y)), expect)
  expect_equal(update_table(from, to, primary_keys = c(x, y), delete = TRUE), from)
})

test_that("update_table.data.table updates data.tables incrementally when given primary_keys unless incremental is FALSE", {
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)] %>% setorderv(c("x", "y"))
  # divide the data
  from <- copy(expect)[1:9000]
  to <- copy(expect)[1000:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  expect_equal(update_table(from, to, primary_keys = c("x", "y"), incremental = FALSE), from)
  expect_equal(update_table(from, to, primary_keys = c(x, y), incremental = FALSE), from)
})

test_that("update_table.data.table updates data.tables incrementally when given date_column and primary_keys", {
  withr::local_package("lubridate")
  withr::local_timezone("America/New_York")

  from <- data.table(date = seq(today() - dyears(10), now(), by = "days"))
  from[, `:=`(I = .I, data = runif(.N))]
  to <- copy(from)
  to <- to[runif(.N) > .1]
  to[runif(.N) < .1, `:=`(
    date = date - ddays(1),
    data = runif(.N)
  )]

  expect_equal(update_table(from, to, date_column = date, primary_keys = c(I)), from)
  expect_equal(update_table(from[1:1000], to, date_column = date, primary_keys = c(I), delete = TRUE), from[1:1000])
})

test_that("update_table.data.table updates by date when date_column given and primary_keys are missing", {
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)]
  # divide the data
  from <- copy(expect)[x>50]
  to <- copy(expect)[x<90]

  expect_warning(result <- update_table(from, to, date_column = "x"), "primary_keys not given.+date_column given")
  setkey(result,x,y)
  setkey(expect,x,y)
  expect_equal(result, expect)
})

test_that("update_table.data.table doesn't copy from when from is a data.table", {
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)]
  # divide the data
  from <- copy(expect)[1:9000]
  to <- copy(expect)[1000:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  tracemem(from)

  expect_silent(update_table(from, to))
  expect_silent(update_table(from, to, primary_keys = c(x, y)))
  expect_silent(update_table(from, to, primary_keys = c(x, y), delete = TRUE))
  expect_silent(expect_warning(update_table(from, to, date_column = c(x))))
})

test_that("update_table.data.table doesn't copy to when to is a data.table", {
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)]
  # divide the data
  from <- copy(expect)[1:9000]
  to <- copy(expect)[1001:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  tracemem(to)

  expect_silent(update_table(from, to))
  expect_silent(update_table(from, to, primary_keys = c(x, y)))
  expect_silent(update_table(from, to, primary_keys = c(x, y), delete = TRUE))
  expect_silent(expect_warning(update_table(from, to, date_column = c(x))))

})

# update_table.default ----------------------------------------------------

test_that("update_table.default updates from db incrementally when given primary_keys", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)] %>% setorderv(c("x", "y"))
  # divide the data
  from <- copy(expect)[1:9000] %>% copy_to(dest = con, "from")
  to <- copy(expect)[1000:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  expect_equal(update_table(from, to, primary_keys = c("x", "y")), expect)
  expect_equal(update_table(from, to, primary_keys = c(x, y)), expect)
  expect_equal(update_table(from, to, primary_keys = c(x, y), delete = TRUE), setDT(collect(from)))
})

test_that("update_table.default updates from db incrementally when given primary_keys unless incremental is FALSE", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)] %>% setorderv(c("x", "y"))
  # divide the data
  from <- copy(expect)[1:9000] %>% copy_to(dest = con, "from")
  to <- copy(expect)[1000:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  expect_equal(update_table(from, to, primary_keys = c("x", "y"), incremental = FALSE), setDT(collect(from)))
  expect_equal(update_table(from, to, primary_keys = c(x, y), incremental = FALSE), setDT(collect(from)))
})


test_that("update_table.default updates from db incrementally when given date_column and primary_keys", {
  withr::local_package("lubridate")
  withr::local_timezone("America/New_York")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  from <- data.table(date = seq(today() - dyears(10), now(), by = "days"))
  from <- from[, `:=`(I = .I, data = runif(.N), date = as.integer(date))]
  to <- copy(from)
  from <- copy_to(con, from)
  to <- to[runif(.N) > .1]
  to[runif(.N) < .1, `:=`(
    date = date - ddays(1),
    data = runif(.N)
  )]

  expect_equal(update_table(from, to, date_column = date, primary_keys = c(I)), setDT(collect(from)))
  expect_equal(
    update_table(head(from, 1000), to, date_column = date, primary_keys = c(I), delete = TRUE),
    setDT(collect(head(from, 1000)))
  )
})

test_that("update_table loads from DB incrementally by date when date_column given and primary_keys are missing", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)]
  # divide the data
  from <- copy_to(con,expect[x>50]) # 5000 rows
  to <- copy(expect)[x<=90] # 9000 rows

  stub(update_table_date_only.default, "collect", function(.) {
    print(dplyr::collect(dplyr::summarize(., dplyr::n()))[[1]])
    dplyr::collect(.)
  })

  # this writes out 1000 because 10*100 rows are updated
  expect_output(result <- update_table_date_only(from, to, date_column = "x"), "\\[1\\] 1000$")
  setkey(collect(result),x,y)
  setkey(expect,x,y)
  expect_equal(result, expect)
})

test_that("update_table loads from DB incrementally by date when date_column given and primary_keys are missing and prefer='from'", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)]
  # divide the data
  from <- copy_to(con,expect[x>50]) # 5000 rows
  to <- copy(expect)[x<=90] # 9000 rows

  stub(update_table_date_only.default, "collect", function(.) {
    print(dplyr::collect(dplyr::summarize(., dplyr::n()))[[1]])
    dplyr::collect(.)
  })

  # this writes out 1000 because 10*100 rows are updated
  expect_output(result <- update_table_date_only(from, to, date_column = "x",
                                                 prefer = "from"), "\\[1\\] 5000$")
  setkey(collect(result),x,y)
  setkey(expect,x,y)
  expect_equal(result, expect)
})

test_that("update_table.default loads from DB incrementally", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  seasons <- readRDS(test_path("seasons.Rds"))
  seasons <- dplyr::mutate_if(seasons, ~ lubridate::is.POSIXct(.), as.numeric)
  seasons_tbl <- dplyr::copy_to(con, seasons)
  seasons <- setDT(seasons)[-1, ]
  seasons[1:2, "last_update_dt"] <- lubridate::ymd("1900-01-01")

  stub(update_table.default, "collect", function(.) {
    print(dplyr::collect(dplyr::summarize(., n()))[[1]])
    dplyr::collect(.)
  })
  # this writes out 3 because 2 rows are updated and 1 row is added
  expect_output(update_table(seasons_tbl, seasons, primary_keys = "id", date_column = "last_update_dt"), "\\[1\\] 3$")
})



test_that("update_table.default loads from DB incrementally even if it's a very large update", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  seasons <- readRDS(test_path("seasons.Rds"))
  seasons <- dplyr::mutate_if(seasons, ~ lubridate::is.POSIXct(.), as.numeric)
  seasons_tbl <- dplyr::copy_to(con, seasons)
  seasons <- setDT(seasons)[-1, ]
  seasons[1:2, "last_update_dt"] <- lubridate::ymd("1900-01-01")

  stub(update_table.default, "collect", function(.) {
    print(dplyr::collect(dplyr::summarize(., dplyr::n()))[[1]])
    dplyr::collect(.)
  })
  stub(update_table.default, "object.size", 2^20 + 1)

  # this writes out 3 because 2 rows are updated and 1 row is added
  expect_output(update_table(seasons_tbl, seasons, primary_keys = "id", date_column = "last_update_dt"), "\\[1\\] 3$")
})

test_that("update_table.default loads from arrow table incrementally", {
  seasons <- readRDS(test_path("seasons.Rds"))
  seasons_arrow <- arrow::arrow_table(seasons)
  seasons <- setDT(seasons)[-c(1, 2), ]
  seasons[1:3, "last_update_dt"] <- lubridate::ymd("1900-01-01")
  stub(update_table.default, "collect", function(.) {
    print(dplyr::collect(dplyr::summarize(., n()))[[1]])
    dplyr::collect(.)
  })
  # this writes out 3 + 2 because 3 rows are updated and 2 rows are added
  expect_output(update_table(seasons_arrow, seasons, primary_keys = "id", date_column = "last_update_dt"), "\\[1\\] 5$")
})

test_that("update_table.default loads from arrow query incrementally", {
  seasons <- readRDS(test_path("seasons.Rds"))
  seasons_arrow <- arrow::arrow_table(seasons) %>%
    filter(id %in% seasons[c(1,3,5),"id"])
  seasons <- setDT(seasons)[-c(1, 2), ]
  seasons[1:3, "last_update_dt"] <- lubridate::ymd("1900-01-01")
  stub(update_table.default, "collect", function(.) {
    print(dplyr::collect(dplyr::summarize(., n()))[[1]])
    dplyr::collect(.)
  })
  # this writes out 2 + 1 because 2 rows are updated and 1 row is added
  expect_output(update_table(seasons_arrow, seasons, primary_keys = "id", date_column = "last_update_dt"), "\\[1\\] 3$")
})

test_that("update_table.default doesn't copy to when to is a data.table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect <- data.table(expand.grid(x = 1:100, y = 1:100))[, data := runif(.N)]
  # divide the data
  from <- copy(expect)[1:9000] %>% dplyr::copy_to(dest=con, "from")
  to <- copy(expect)[1001:10000]
  # and mung it up
  to[1:5000, data := runif(.N)]

  tracemem(to)

  expect_silent(update_table(from, to))
  expect_silent(update_table(from, to, primary_keys = c(x, y)))
  expect_silent(update_table(from, to, primary_keys = c(x, y), delete = TRUE))
  expect_silent(expect_warning(update_table(from, to, date_column = c(x))))

})
