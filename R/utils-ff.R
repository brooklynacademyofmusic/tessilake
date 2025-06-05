# as.ram2 -----------------------------------------------------------------

#' as.ram2
#' wrapper for ff:as.ram that also deletes the underlying ff/ffdf to reduce temp space
#' @param obj ff/ffdf object to be loaded to ram and deleted
#'
#' @return a data.table
#' @export
#' @importFrom ff as.ram delete is.ffdf ff as.ffdf
#' @importFrom ffbase as.ram.ffdf
#' @importFrom data.table setDT setattr
#' @examples
#' library(ff)
#' library(tessilake)
#'
#' test <- ff(1:5)
#' file.exists(filename(test)) # TRUE
#' test.ram <- as.ram2(test)
#' file.exists(filename(test)) # FALSE
#'
as.ram2 <- function(obj) {
  if (is.ffdf(obj)) {
    r <- setDT(obj[, ])
  } else {
    r <- obj[]
  }
  delete(obj)
  # fix_vmode sets vmode in a way that persists after loading from ff
  setattr(r, "vmode", NULL)
  r
}

# fix_vmode ---------------------------------------------------------------

#' fix_vmode
#' adjust the vmode of a vector to the lowest necessary to save it as ff
#'
#' @param vec the vector to be adapted
#'
#' @return a vector with the vmode attribute set appropriately
#'
#' @export
#' @importFrom glue glue
#' @importFrom data.table setattr
#' @importFrom ff is.ff vmode vmode<- .rammode
#'
#' @examples
#'
#' library(ff)
#' test <- 1:5
#' test <- fix_vmode(test)
#' vmode(test) # "nibble"
#' test <- fix_vmode(test[1:3])
#' vmode(test) # "quad"
#'
fix_vmode <- function(vec) {
  if (is.ff(vec)) {
    stop("ff objects can't be coerced")
  }

  if (!is.atomic(vec)) vec <- as.vector(vec)
  if (is.list(vec)) vec <- vec[[1]]

  if (is.character(vec)) {
    message("Converting character to factor...")
    return(as.factor(vec))
  } else if (is.factor(vec)) {
    message("Already a factor")
    return(vec)
  }

  minVec <- min(vec, na.rm = TRUE)
  maxVec <- max(vec, na.rm = TRUE)
  isDate <- inherits(vec, c("Date","POSIXct"))
  hasDec <- isDate || maxVec > .Machine$integer.max ||
    any(as.integer(vec[which(!is.na(vec))])!=vec[which(!is.na(vec))], na.rm = T)
  hasNA <- any(is.na(vec))
  signed <- minVec < 0 || isDate
  bits <- ceiling(log2(abs(as.numeric(maxVec)) + .01))

  vmode <-
    if (hasDec) {
      "double" # 64 bit float
    } else if (!signed & !hasNA) {
      if (!isDate & bits == 1) {
        "boolean"
      } # 1 bit logical without NA
      else if (bits <= 2) {
        "quad"
      } # 2 bit unsigned integer without NA
      else if (bits <= 4) {
        "nibble"
      } # 4 bit unsigned integer without NA
      else if (bits <= 8) {
        "ubyte"
      } # 8 bit unsigned integer without NA
      else if (bits <= 16) {
        "ushort"
      } # 16 bit unsigned integer without NA
      else if (bits <= 32) {
        "integer"
      } # 32 bit unsigned integer without NA
      else {
        "double"
      }
    } else {
      if (!isDate & bits == 1 & !signed) {
        "logical"
      } # 2 bit logical with NA
      else if (bits <= 7) {
        "byte"
      } # 8 bit signed integer with NA
      else if (bits <= 15) {
        "short"
      } # 16 bit signed integer with NA
      else if (bits <= 31) {
        "integer"
      } # 32 bit signed integer with NA
      else {
        "double"
      }
    }


  setattr(vec, "vmode", NULL)
  if (.rammode[vmode] != storage.mode(vec)) {
    message(glue("Converting to {vmode}..."))
    vmode(vec) <- vmode
  } else {
    message(glue("Assigning vmode attribute to {vmode}..."))
    setattr(vec, "vmode", vmode)
  }
  vec
}

#' write_ffdf
#'
#' Write out a gzip compressed ffdf. `table_name` is converted to camel-case for consistency with
#' legacy use
#'
#' @param x data.frame or [dplyr] table or [arrow::Table]/[arrow::Dataset] to be written
#' @param table_name string name of the file to be written, will be converted to camel-case
#' @param out_dir string directory where the compressed ffdf should be written
#' @importFrom ffbase save.ffdf
#' @importFrom ff ffdf as.ff
#' @importFrom utils tar
#' @importFrom dplyr collect
#' @return invisibly
#' @export
write_ffdf <- function(x, table_name, out_dir) {
  table_name_camelcase <- gsub("[^a-zA-Z](\\w)", "\\U\\1", table_name, perl = TRUE)

  as_ff <- function(c) {
    vec <- x[[c]] %||% x[c]
    tryCatch(
      vec <- collect(vec),
      error = \(e){}
    )
    as.ff(fix_vmode(vec))
  }

  assign(table_name_camelcase,
         lapply(names(x), as_ff) %>%
           setNames(names(x)) %>%
           do.call(what = ffdf))

  temp_dir <- tempfile(table_name_camelcase)
  dir.create(temp_dir)
  eval(rlang::expr(save.ffdf(!!table_name_camelcase,dir=temp_dir,overwrite=T)))

  message("packing ffdf...")
  tar(file.path(out_dir,paste0(table_name_camelcase, ".gz")),
      files=".",compression="gzip",tar="tar",extra_flags=paste0("-C ",temp_dir))
  invisible()
}
