withr::local_package("withr")

# setleftjoin -------------------------------------------------------------

setleftjoin <- function(...) {
  suppressMessages(tessilake::setleftjoin(...))
}

l <- data.table(a = seq(1:10), b = sample(1:100, 10))
r <- data.table(b = seq(1:100), c = sample(1:1000, 100))
r1 <- r

merge.data.table <- function(...) {
  setkey(data.table::merge.data.table(...), NULL)
}
expect_no_side_effects_on_r <- function() {
  expect_true(rlang::is_reference(r, r1))
  expect_equal(r, r1)
}

test_that("setleftjoin merges in place", {
  expect_true(rlang::is_reference(setleftjoin(l, r), l))
  expect_no_side_effects_on_r()
})

l <- data.table(a = seq(1:10), b = sample(1:100, 10))
r <- data.table(b = seq(1:100), c = sample(1:1000, 100))
r1 <- r

test_that("setleftjoin handles missing 'by' argument", {
  expect_mapequal(setleftjoin(copy(l), r) %>% setkeyv("b"), merge(l, r, all.x = T))
  expect_no_side_effects_on_r()
})

test_that("setleftjoin handles character 'by' argument", {
  expect_mapequal(setleftjoin(copy(l), r, by = "b") %>% setkeyv("b"), merge(l, r, by = "b", all.x = T))
  expect_no_side_effects_on_r()
})

test_that("setleftjoin avoids column collision", {
  colnames(l) <- c("b", "c")
  expect_mapequal(setleftjoin(copy(l), r, by = "b"), merge(l, r, by = "b", all.x = T))
  expect_no_side_effects_on_r()
})

test_that("setleftjoin handles named vector 'by' argument", {
  colnames(l) <- c("f", "g")
  expect_mapequal(setleftjoin(copy(l), r, by = c("f" = "b")), merge(l, r, by.x = "f", by.y = "b", all.x = T))
  expect_no_side_effects_on_r()
})

test_that("setleftjoin fails when there's more than one matching row in r", {
  r2 <- r[c(1, 1:.N)]
  expect_error(setleftjoin(copy(l), r2))
})

test_that("setleftjoin succeeds when there's more or less than one matching row in l", {
  l0 <- l[-1, ]
  l2 <- l[c(1, 1:.N)]

  expect_mapequal(setleftjoin(copy(l0), r) %>% setkeyv("b"), merge(l0, r, all.x = T))
  expect_mapequal(setleftjoin(copy(l2), r) %>% setkeyv("b"), merge(l2, r, all.x = T))
})

test_that("setleftjoin succeeds when there's NAs in l or r", {
  l1 <- copy(l)[1, "b" := NA]
  r1 <- copy(r)[1, "b" := NA]

  expect_mapequal(setleftjoin(copy(l1), r) %>% setkeyv("b"), merge(l1, r, all.x = T))
  expect_mapequal(setleftjoin(copy(l), r1) %>% setkeyv("b"), merge(l, r1, all.x = T))
})
