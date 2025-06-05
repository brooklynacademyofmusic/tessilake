withr::local_package("purrr")
withr::local_package("checkmate")

# as.ram2 -----------------------------------------------------------------

# if (FALSE) {
test_that("as.ram2 returns ram objects", {
  test.ff <- ff(1:5)
  expect_equal(as.ram2(test.ff), 1:5)

  test.ffdf <- as.ffdf(data.frame(a = 1:5, b = 6:10))
  expect_equal(as.ram2(test.ffdf), data.table(a = 1:5, b = 6:10))

  test.ffdf <- as.ffdf(data.frame(a = 1:5, b = 6:10))
  expect_true(is.data.table(as.ram2(test.ffdf)))
})

test_that("as.ram2 leaves nothing behind", {
  test.ff <- ff(1:5)
  as.ram2(test.ff)
  expect_equal(file.exists(ff::filename(test.ff)), FALSE)

  test.ffdf <- as.ffdf(data.frame(a = 1:5, b = 6:10))
  as.ram2(test.ffdf)
  expect_mapequal(sapply(ff::filename(test.ffdf), file.exists), c(a = FALSE, b = FALSE))
})

# fix_vmode ---------------------------------------------------------------

fix_vmode <- function(x) {
  suppressMessages(tessilake::fix_vmode(x))
}

test_that("fix_vmode throws an error when given an ff", {
  test.ff <- ff(1:5)
  expect_error(fix_vmode(test.ff))
})

test_that("fix_vmode converts character to factor and leaves factors alone", {
  vec.char <- c("a", "b")
  expect_equal(fix_vmode(vec.char), as.factor(vec.char))
  vec.fact <- as.factor(vec.char)
  expect_equal(fix_vmode(vec.fact), vec.fact)
})


local_create_vec <- function() {
  vecs <- purrr::map(as.list(2^(0:63)), ~ c(0, .))
  rlang::env_bind(parent.frame(),
    vecs = vecs,
    vecs_na = map(vecs, ~ c(NA, .)),
    vecs_1 = map(vecs, ~ c(-1, .)),
    vecs_e = copy(vecs),
    vecs_e_na = map(vecs, ~ c(NA, .)),
    vecs_e_1 = map(vecs, ~ c(-1, .))
  )
}

test_that("fix_vmode sets vmode on integer values based on the number of bits required", {
  local_create_vec()
  expect_equal(map_chr(vecs, ~ vmode(fix_vmode(.))), c(
    "boolean",
    "quad",
    rep("nibble", 2),
    rep("ubyte", 4),
    rep("ushort", 8),
    rep("integer", 15),
    rep("double", 33)
  ))
})

test_that("fix_vmode sets vmode on integer values even when they're out of range", {
  vec_oor <- c(-.Machine$integer.max*2,.Machine$integer.max*2)
  expect_equal(map_chr(vec_oor, ~ vmode(fix_vmode(.))), c("double","double"))
})

test_that("fix_vmode sets vmode on decimal values", {
  local_create_vec()
  vecs <- map(vecs,~{.[1]=.[1]+.01; .})
  expect_equal(map_chr(vecs,~ vmode(fix_vmode(.))),rep("double",64))
})


test_that("fix_vmode upgrades vmode on integer values when there are NA or signed values", {
  local_create_vec()
  expect_equal(map_chr(vecs_na, ~ vmode(fix_vmode(.))), c(
    "logical",
    rep("byte", 6),
    rep("short", 8),
    rep("integer", 16),
    rep("double", 33)
  ))
  expect_equal(map_chr(vecs_1, ~ vmode(fix_vmode(.))), c(
    rep("byte", 7),
    rep("short", 8),
    rep("integer", 16),
    rep("double", 33)
  ))
})

test_that("fix_vmode doesn't destroy any values when passing through ff", {
  local_create_vec()
  expect_equal(map(vecs, ~ as.ram2(ff(fix_vmode(.))) + 0), vecs_e)
  expect_equal(map(vecs_na, ~ as.ram2(ff(fix_vmode(.))) + 0), vecs_e_na)
  expect_equal(map(vecs_1, ~ as.ram2(ff(fix_vmode(.))) + 0), vecs_e_1)
})

as.Date.numeric <- function(x, ...) {
  base::as.Date.numeric(x, origin = as.Date("1970-01-01"))
}
test_that("fix_vmode doesn't destroy dates when passing through ff", {
  local_create_vec()
  expect_equal(map(vecs, ~ as.ram2(ff(fix_vmode(as.Date.numeric(.))))), map(vecs_e, as.Date.numeric))
  expect_equal(map(vecs_na, ~ as.ram2(ff(fix_vmode(as.Date.numeric(.))))), map(vecs_e_na, as.Date.numeric))
  expect_equal(map(vecs_1, ~ as.ram2(ff(fix_vmode(as.Date.numeric(.))))), map(vecs_e_1, as.Date.numeric))
})

as.POSIXct.numeric <- function(x, ...) {
  base::as.POSIXct.numeric(x, origin = as.POSIXct("1970-01-01 00:00:00"))
}
test_that("fix_vmode doesn't destroy POSIXct when passing through ff", {
  local_create_vec()
  expect_equal(map(vecs, ~ as.ram2(ff(fix_vmode(as.POSIXct(. + .1))))), map(vecs_e, ~ as.POSIXct(. + .1)))
  expect_equal(map(vecs_na, ~ as.ram2(ff(fix_vmode(as.POSIXct(. + .1))))), map(vecs_e_na, ~ as.POSIXct(. + .1)))
  expect_equal(map(vecs_1, ~ as.ram2(ff(fix_vmode(as.POSIXct(. + .1))))), map(vecs_e_1, ~ as.POSIXct(. + .1)))
})

test_that("fix_vmode doesn't destroy doubles when passing through ff", {
  local_create_vec()
  expect_equal(map(vecs, ~ as.ram2(ff(fix_vmode(. + .1)))), map(vecs_e, ~ (. + .1)))
  expect_equal(map(vecs_na, ~ as.ram2(ff(fix_vmode(. + .1)))), map(vecs_e_na, ~ (. + .1)))
  expect_equal(map(vecs_1, ~ as.ram2(ff(fix_vmode(. + .1)))), map(vecs_e_1, ~ (. + .1)))
})

test_that("fix_vmode works with things that already have vmode set", {
  local_create_vec()
  expect_equal(map(vecs, ~ vmode(fix_vmode(as.ram(ff(.))))), map(vecs_e, ~ vmode(fix_vmode(.))))
  expect_equal(map(vecs_na, ~ vmode(fix_vmode(as.ram(ff(.))))), map(vecs_e_na, ~ vmode(fix_vmode(.))))
  expect_equal(map(vecs_1, ~ vmode(fix_vmode(as.ram(ff(.))))), map(vecs_e_1, ~ vmode(fix_vmode(.))))
})

test_that("fix_vmode works with things that are in the right storage mode but wrong vmode", {
  local_create_vec()
  expect_equal(map(vecs, ~ vmode(fix_vmode(setattr(., "vmode", "integer")))), map(vecs_e, ~ vmode(fix_vmode(.))))
  expect_equal(map(vecs, ~ vmode(fix_vmode(setattr(., "vmode", "double")))), map(vecs_e, ~ vmode(fix_vmode(.))))
})


test_that("fix_vmode assigns in-place things that don't need conversion", {
  local_create_vec()
  expect_equal(map(vecs, ~ rlang::is_reference(fix_vmode(.), .)), as.list(c(
    rep(F, 31),
    rep(T, 33)
  )))
  vecs[1:31] <- map(vecs[1:31], as.integer)
  expect_equal(map(vecs[1:31], ~ rlang::is_reference(fix_vmode(.), .)), as.list(c(F, rep(T, 30))))
})

# write_ffdf --------------------------------------------------------------
test_ffdf_data <- data.table(a = sample(seq(1000),1e3,replace = T),
                   b = sample(letters,1e3,replace=T),
                   c = runif(1e3))

test_that("write_ffdf works with data.tables", {
  withr::defer(file.remove(file.path(tempdir(),"testFfdf.gz")))
  expect_failure(expect_file_exists(file.path(tempdir(),"testFfdf.gz")))
  suppressMessages(write_ffdf(test_ffdf_data, "test_ffdf", tempdir()))
  expect_file_exists(file.path(tempdir(),"testFfdf.gz"))
})

test_that("write_ffdf outputs a readable ffdf", {
  withr::defer(file.remove(file.path(tempdir(),"testFfdf.gz")))
  suppressMessages(write_ffdf(test_ffdf_data, "test_ffdf", tempdir()))
  ffbase::unpack.ffdf(file.path(tempdir(),"testFfdf.gz"))
  expect_true(exists("testFfdf"))
  test_ffdf <- suppressMessages(as.data.table(testFfdf)) %>%
    purrr::map_if(is.factor,as.character) %>% setDT
  expect_equal(test_ffdf, test_ffdf_data,
               ignore_attr = TRUE)
})

test_that("write_ffdf works with arrow tables", {
  withr::defer(file.remove(file.path(tempdir(),"testFfdf.gz")))
  arrow <- arrow::as_arrow_table(test_ffdf_data)
  suppressMessages(write_ffdf(test_ffdf_data, "test_ffdf", tempdir()))

  ffbase::unpack.ffdf(file.path(tempdir(),"testFfdf.gz"))
  expect_true(exists("testFfdf"))
  test_ffdf <- suppressMessages(as.data.table(testFfdf)) %>%
    purrr::map_if(is.factor,as.character) %>% setDT
  expect_equal(test_ffdf, test_ffdf_data,
               ignore_attr = TRUE)
})

test_that("write_ffdf works with arrow dataset", {
  withr::defer(file.remove(file.path(tempdir(),"testFfdf.gz")))
  arrow::write_dataset(test_ffdf_data,file.path(tempdir(),"dataset"))
  arrow <- arrow::open_dataset(file.path(tempdir(),"dataset"))
  suppressMessages(write_ffdf(test_ffdf_data, "test_ffdf", tempdir()))

  ffbase::unpack.ffdf(file.path(tempdir(),"testFfdf.gz"))
  expect_true(exists("testFfdf"))
  test_ffdf <- suppressMessages(as.data.table(testFfdf)) %>%
    purrr::map_if(is.factor,as.character) %>% setDT
  setkey(test_ffdf,a,b)
  setkey(test_ffdf_data,a,b)
  expect_equal(test_ffdf, test_ffdf_data, ignore_attr = TRUE)
})
