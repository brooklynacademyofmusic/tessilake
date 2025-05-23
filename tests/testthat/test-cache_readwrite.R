withr::local_package("checkmate")
withr::local_package("mockery")
local_cache_dirs()

# cache_write -------------------------------------------------------------

test_read_write <- setattr(data.table(x = 1:100000, y = runif(1000)), "key", "value")
test_that("cache_write with partitioning=FALSE creates parquet and feather files", {
  cache_write(test_read_write, "test_read_write", "deep", "tessi", partition = FALSE)
  expect_true(file.exists(paste0(cache_path("test_read_write", "deep", "tessi"), ".parquet")))
  cache_write(test_read_write, "test_read_write", "shallow", "tessi", partition = FALSE)
  expect_true(file.exists(paste0(cache_path("test_read_write", "shallow", "tessi"), ".feather")))
})

test_that("cache_write with primary keys or partitioning spec creates datasets", {
  test_partitioning <- copy(test_read_write)
  data.table::setattr(test_partitioning, "primary_keys", "x")

  cache_write(test_partitioning, "test_partitioning", "deep", "tessi")
  expect_equal(length(dir(cache_path("test_partitioning", "deep", "tessi"))), 11)
  cache_write(test_partitioning, "test_partitioning", "shallow", "tessi")
  expect_equal(length(dir(cache_path("test_partitioning", "shallow", "tessi"))), 11)

  data.table::setattr(test_partitioning, "primary_keys", c("x","y"))
  cache_write(test_partitioning, "test_partitioning_2keys", "deep", "tessi")
  expect_equal(length(dir(cache_path("test_partitioning_2keys", "deep", "tessi"))), 11)
  cache_write(test_partitioning, "test_partitioning_2keys", "shallow", "tessi")
  expect_equal(length(dir(cache_path("test_partitioning_2keys", "shallow", "tessi"))), 11)

  data.table::setattr(test_partitioning, "primary_keys", NULL)
  test_partitioning[,partition_x:=floor(x/10000)]
  cache_write(test_partitioning, "test_partitioning_0keys", "deep", "tessi", partition = "partition_x")
  expect_equal(length(dir(cache_path("test_partitioning_0keys", "deep", "tessi"))), 11)
  cache_write(test_partitioning, "test_partitioning_0keys", "shallow", "tessi", partition = "partition_x")
  expect_equal(length(dir(cache_path("test_partitioning_0keys", "shallow", "tessi"))), 11)
})

test_that("cache_write with primary keys doesn't have side effects on x", {
  cache_write(test_read_write, "test_read_write2", "deep", "tessi", primary_keys = "x")
  expect_false("primary_keys" %in% names(attributes(test_read_write)))
  expect_true("primary_keys" %in% names(cache_get_attributes(cache_read("test_read_write2", "deep", "tessi"))))
})

test_that("cache_write refuses to overwrite an existing cache unless overwrite==TRUE", {
  expect_error(cache_write(test_read_write, "test_read_write", "deep", "tessi", partition = FALSE), "overwrite")
  expect_error(cache_write(test_read_write, "test_read_write", "deep", "tessi"), "overwrite")
  expect_silent(cache_write(test_read_write, "test_read_write", "deep", "tessi", partition = FALSE, overwrite = TRUE))
})

test_that("cache_write with works with anything data.frameish", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  test_read_write_table <- dplyr::copy_to(con, test_read_write)

  cache_write(arrow_table(test_read_write), "test_read_write_arrow", "deep", "tessi")
  expect_true(file.exists(paste0(cache_path("test_read_write_arrow", "deep", "tessi"), ".parquet")))

  cache_write(arrow_table(x=1), "test_read_write_arrow", "shallow", "tessi", primary_keys = "x", partition = F)
  expect_true(file.exists(paste0(cache_path("test_read_write_arrow", "shallow", "tessi"), ".feather")))
  expect_true("primary_keys" %in% names(cache_get_attributes(cache_read("test_read_write_arrow", "shallow", "tessi"))))

  cache_write(test_read_write_table, "test_read_write_tbl", "deep", "tessi", primary_keys = "x", partition = F)
  expect_true(file.exists(paste0(cache_path("test_read_write_tbl", "deep", "tessi"), ".parquet")))
  expect_true("primary_keys" %in% names(cache_get_attributes(cache_read("test_read_write_tbl", "deep", "tessi"))))

  cache_write(test_read_write_table, "test_read_write_tbl", "shallow", "tessi", primary_keys = "x", partition = F)
  expect_true(file.exists(paste0(cache_path("test_read_write_tbl", "shallow", "tessi"), ".feather")))
  expect_true("primary_keys" %in% names(cache_get_attributes(cache_read("test_read_write_tbl", "shallow", "tessi"))))


  DBI::dbDisconnect(con)
})

test_that("cache_write doesn't copy x", {
  tracemem(test_read_write)
  expect_silent(cache_write(test_read_write, "test_copy", "deep", "tessi"))
  untracemem(test_read_write)
})

test_that("cache_write returns nothing, invisibly", {
  expect_invisible(cache_write(test_read_write, "test_silent", "deep", "tessi"))
  expect_equal(cache_write(test_read_write, "test_silent2", "deep", "tessi"), NULL)
})

test_that("cache_write is failure resistant", {
  test_write_failure <- data.table(x = runif(1000000))
  path <- cache_path("test_write_failure","shallow","tessi")
  cache_write(test_write_failure, "test_write_failure", "shallow", "tessi")
  # make cache read-only
  system2("chmod",c("400",paste0(path,".feather")))
  # and try to write it...
  expect_error(
    cache_write(test_write_failure,"test_write_failure","shallow","tessi",overwrite=T,num_tries = 1),
    "IOError")
  # set up a deferred job to make it writeable again
  r <- callr::r_bg(function() {
    Sys.sleep(1)
    system2("chmod",c("600",paste0(path,".feather")))
  }, package = TRUE)
  # now try to write again with some error recovery
  error_count <- mock(cycle = T)
  stub(cache_write, "force", error_count)
  expect_silent(
    cache_write(test_write_failure,"test_write_failure","shallow","tessi",overwrite=T))
  expect_gte(length(mock_args(error_count)),1)
})

test_that("cache_write works after a read (see issue #22: Error with memory-mapped files on Windows during cache_write after a cache_read)", {
  depths <- names(config::get("tessilake")$depths)
  cache_write(test_read_write, "test_mmap_write", depths[1], "stream")
  cache_read("test_mmap_write", depths[1], "stream")
  expect_silent(cache_write(test_read_write, "test_mmap_write", depths[1], "stream", overwrite = TRUE, num_tries = 2))
})

# cache_read --------------------------------------------------------------

test_that("cache_read returns FALSE and warns for non-existent caches", {
  expect_error(expect_false(cache_read("blah", "deep", "tessi", num_tries = 1)),
                              "Cache does not exist")
})

test_that("cache_read returns data to the original form including attributes", {
  expect_equal(cache_read("test_read_write", "deep", "tessi") %>% collect(), test_read_write)
  expect_equal(cache_read("test_read_write", "shallow", "tessi") %>% collect(), test_read_write)
  expect_equal(cache_read("test_read_write_arrow", "deep", "tessi") %>% collect(), test_read_write)

  ignore_attributes <- c("partitioning",
                       "partition_key",
                       "primary_keys")

  expect_equal(cache_read("test_partitioning", "deep", "tessi") %>% collect() %>% setorderv("x"),
               test_read_write, ignore_attr = ignore_attributes)
  expect_equal(cache_read("test_partitioning", "shallow", "tessi") %>% collect() %>% setorderv("x"),
               test_read_write, ignore_attr = ignore_attributes)

  expect_equal(cache_read("test_partitioning_2keys", "deep", "tessi") %>% collect() %>% setorderv("x"),
               test_read_write, ignore_attr = ignore_attributes)
  expect_equal(cache_read("test_partitioning_2keys", "shallow", "tessi") %>% collect() %>% setorderv("x"),
               test_read_write, ignore_attr = ignore_attributes)
})

test_that("cache_read with include_partition = TRUE returns the hidden partition column", {
  expect_equal(cache_read("test_partitioning", "deep", "tessi", include_partition = TRUE) %>% names(), c("x", "y", "partition_x"))
  expect_equal(cache_read("test_partitioning", "shallow", "tessi", include_partition = TRUE) %>% names(), c("x", "y", "partition_x"))

  expect_equal(cache_read("test_partitioning_2keys", "deep", "tessi", include_partition = TRUE) %>% names(), c("x", "y", "partition_x"))
  expect_equal(cache_read("test_partitioning_2keys", "shallow", "tessi", include_partition = TRUE) %>% names(), c("x", "y", "partition_x"))

})

test_that("cache_read can select particular columns", {
  expect_equal(cache_read("test_read_write", "deep", "tessi", select = "y") %>% collect() %>% .[[1]], test_read_write[, y])
  expect_equal(cache_read("test_read_write", "shallow", "tessi", select = "y") %>% collect() %>% .[[1]], test_read_write[, y])
  expect_equal(cache_read("test_read_write_arrow", "deep", "tessi", select = "y") %>% collect() %>% .[[1]], test_read_write[, y])
  expect_equal(cache_read("test_read_write_tbl", "deep", "tessi", select = "y") %>% collect() %>% .[[1]], test_read_write[, y])
  expect_equal(cache_read("test_partitioning", "deep", "tessi", select = "y") %>% collect() %>% .[[1]] %>% sort(), test_read_write[, y] %>% sort())
  expect_equal(cache_read("test_partitioning", "shallow", "tessi", select = "y") %>% collect() %>% .[[1]] %>% sort(), test_read_write[, y] %>% sort())
})

test_that("cache_read is failure resistant", {
  test_read_write <- data.table(x = runif(1000000))
  path <- cache_path("test_read_write","deep","tessi")
  cache_write(test_read_write,"test_read_write","deep","tessi",overwrite=T)
  # mung up the file
  system2("truncate",c("-s","-1k",shQuote(paste0(path,".parquet"))))
  # and try to read it -- should throw warning
  expect_error(cache_read("test_read_write","deep","tessi",num_tries = 1),"Couldn't read cache.+corrupted")
  # point child process to parent tempdir
  mockery::stub(cache_read,"cache_path",path)
  # now try to read it again
  r <- callr::r_bg(function(){
    dplyr::collect(cache_read("test_read_write", "deep", "tessi"))
  }, package = T)
  # and restore file in the meantime
  cache_write(test_read_write,"test_read_write","deep","tessi",overwrite=T)
  r$wait()
  ret <- r$get_result()
  expect_equal(ret,test_read_write)
})

# read_cache --------------------------------------------------------------

test_that("read_cache reads from the most recently updated cache", {
  withr::local_package("lubridate")
  cache_read <- mock(cycle = TRUE)
  cache_get_mtime <- mock(now(), now() + 3600, now() + 3600, now())
  stub(read_cache, "cache_read", cache_read)
  stub(read_cache, "cache_get_mtime", cache_get_mtime)

  read_cache("test_read_write", "tessi")
  expect_length(mock_args(cache_read), 1)
  expect_equal(mock_args(cache_read)[[1]][["depth"]], names(config::get("tessilake")$depths)[2])

  read_cache("test_read_write", "tessi")
  expect_length(mock_args(cache_read), 2)
  expect_equal(mock_args(cache_read)[[2]][["depth"]], names(config::get("tessilake")$depths)[1])

})

# write_cache -------------------------------------------------------------

test_that("write_cache writes to the primary (first listed) cache using either `cache_write` or `cache_update` and then syncs", {

  cache_write <- mock()
  cache_update <- mock()
  sync_cache <- mock()
  stub(write_cache, "cache_write", cache_write)
  stub(write_cache, "cache_update", cache_update)
  stub(write_cache, "sync_cache", sync_cache)

  write_cache(test_read_write, "test_write_cache", "tessi")
  expect_length(mock_args(cache_write), 1)
  expect_equal(mock_args(cache_write)[[1]][["depth"]], names(config::get("tessilake")$depths)[1])
  expect_length(mock_args(sync_cache), 1)
  expect_equal(mock_args(sync_cache)[[1]][["table_name"]], "test_write_cache")
  expect_equal(mock_args(sync_cache)[[1]][["incremental"]], FALSE)

  write_cache(test_read_write, "test_write_cache", "tessi", incremental = TRUE)
  expect_length(mock_args(cache_update), 1)
  expect_equal(mock_args(cache_update)[[1]][["depth"]], names(config::get("tessilake")$depths)[1])
  expect_length(mock_args(sync_cache), 2)
  expect_equal(mock_args(sync_cache)[[2]][["table_name"]], "test_write_cache")
  expect_equal(mock_args(sync_cache)[[2]][["incremental"]], FALSE)
})

test_that("write_cache makes the primary file the most recently updated after a sync for better performance", {
  depths <- names(config::get("tessilake")$depths)
  write_cache(test_read_write, "test_write_cache", "tessi")

  mtimes <- purrr::map_vec(depths, \(depth) cache_get_mtime("test_write_cache", depth, "tessi"))
  expect_length(mtimes,length(depths))
  expect_true(all(mtimes[1] == mtimes[-1]))

  write_cache(test_read_write, "test name with spaces", "tessi")

  mtimes <- purrr::map_vec(depths, \(depth) cache_get_mtime("test name with spaces", depth, "tessi"))
  expect_length(mtimes,length(depths))
  expect_true(all(mtimes[1] == mtimes[-1]))
})

# sync_cache --------------------------------------------------------------

test_that("sync_cache does nothing when there's only one depth", {
  tessilake <- config::get("tessilake")
  tessilake$depths <- tessilake$depths[1]
  stub(sync_cache, "config::get", tessilake)
  cache_write(test_read_write, "test_sync_cache", names(tessilake$depths)[1], "stream")
  .cache_write <- mock(cycle = TRUE)
  stub(sync_cache, "cache_write", .cache_write)

  sync_cache("test_sync_cache","stream")

  expect_length(mock_args(.cache_write),0)

})

test_that("sync_cache updates arrow caches non-incrementally across all storages", {
  depths <- names(config::get("tessilake")$depths)
  cache_write(test_read_write, "test_sync_cache", depths[1], "stream", overwrite = TRUE)
  .cache_write <- mock(cycle = TRUE)
  stub(sync_cache, "cache_write", .cache_write)

  # sync cache will warn becasue it can't sync the timestamp on a non-existent cache
  expect_warning(sync_cache("test_sync_cache", "stream"),
                 paste0("Timestamp sync failed.+",depths[2],".+test_sync_cache"))

  expect_length(mock_args(.cache_write),1)
  expect_equal(mock_args(.cache_write)[[1]][["depth"]], depths[2])

  cache_write(test_read_write, "test_sync_cache", depths[2], "stream")
  sync_cache("test_sync_cache", "stream")
  expect_length(mock_args(.cache_write),2)
  expect_equal(mock_args(.cache_write)[[2]][["depth"]], depths[1])

})

test_that("sync_cache updates arrow caches incrementally across all storages", {
  depths <- names(config::get("tessilake")$depths)
  cache_write(test_read_write, "test_sync_cache", depths[1], "stream", overwrite = TRUE)
  cache_update <- mock(cycle = TRUE)
  stub(sync_cache, "cache_update", cache_update)

  sync_cache("test_sync_cache", "stream", incremental = TRUE)
  expect_length(mock_args(cache_update),1)
  expect_equal(mock_args(cache_update)[[1]][["depth"]], depths[2])

  cache_write(test_read_write, "test_sync_cache", depths[2], "stream", overwrite = TRUE)
  sync_cache("test_sync_cache", "stream", incremental = TRUE)
  expect_length(mock_args(cache_update),2)
  expect_equal(mock_args(cache_update)[[2]][["depth"]], depths[1])
})

test_that("sync_cache copies non-arrow files across all storages", {
  depths <- names(config::get("tessilake")$depths)

  other_file <- write.csv(letters, cache_path("other_file.csv", depths[1], "stream"))
  sync_cache("other_file.csv", "stream")
  expect_file_exists(cache_path("other_file.csv", depths[2], "stream"))

  expect_equal(read.csv(cache_path("other_file.csv", depths[1], "stream")),
               read.csv(cache_path("other_file.csv", depths[2], "stream")))

})

test_that("sync_cache copies directories across all storages", {
  depths <- names(config::get("tessilake")$depths)

  dir.create(cache_path("other_file", depths[1], "stream"))
  other_file <- write.csv(letters, cache_path("other_file/other_file.csv", depths[1], "stream"))
  sync_cache("other_file", "stream", whole_file = T)
  expect_file_exists(cache_path("other_file/other_file.csv", depths[2], "stream"))

  expect_equal(read.csv(cache_path("other_file/other_file.csv", depths[1], "stream")),
               read.csv(cache_path("other_file/other_file.csv", depths[2], "stream")))

})

test_that("sync_cache syncs arrow timestamps across all storages", {
  depths <- names(config::get("tessilake")$depths)

  cache_write(test_read_write, "test_sync_cache", depths[1], "stream", overwrite = TRUE)
  cache_write(test_read_write, "test_sync_cache_partitioned", depths[1], "stream", overwrite = TRUE, primary_keys = "x")
  Sys.sleep(1)
  cache_write(test_read_write, "test_sync_cache", depths[2], "stream", overwrite = TRUE)
  cache_write(test_read_write, "test_sync_cache_partitioned", depths[2], "stream", overwrite = TRUE, primary_keys = "x")

  cache_files <- c(cache_files("test_sync_cache", depths[1], "stream"),
                   cache_files("test_sync_cache", depths[2], "stream"))

  cache_files_partitioned <- c(cache_files("test_sync_cache_partitioned", depths[1], "stream"),
                               cache_files("test_sync_cache_partitioned", depths[2], "stream"))

  expect_false(all(file.mtime(cache_files) == max(file.mtime(cache_files))))
  expect_false(all(file.mtime(cache_files_partitioned) == max(file.mtime(cache_files_partitioned))))

  sync_cache("test_sync_cache", "stream", incremental = TRUE)
  sync_cache("test_sync_cache_partitioned", "stream", incremental = TRUE)

  expect_true(all(file.mtime(cache_files) == max(file.mtime(cache_files))))
  expect_true(all(file.mtime(cache_files_partitioned) == max(file.mtime(cache_files_partitioned))))
})

test_that("sync_cache syncs non-arrow timestamps across all storages", {
  depths <- names(config::get("tessilake")$depths)

  file.create(cache_path("other_file.txt", depths[1], "stream"))
  Sys.sleep(1)
  file.create(cache_path("other_file.txt", depths[2], "stream"))

  cache_files <- c(cache_files("other_file.txt", depths[1], "stream"),
                   cache_files("other_file.txt", depths[2], "stream"))

  expect_false(all(file.mtime(cache_files) == max(file.mtime(cache_files))))

  sync_cache("other_file.txt", "stream")

  expect_true(all(file.mtime(cache_files) == max(file.mtime(cache_files))))
})
