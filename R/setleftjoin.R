#' setleftjoin
#' Fast left join for data.table that simply adds columns from `r` to `l` by reference.
#'  Requires that `r` has at most one item of each join group in order to be able to simply add to `l`.
#' @param l left side of the join (data.table)
#' @param r right side of the join (data.table)
#' @param by character vector for matching columns. Names, if they exist, refer to columns in the
#' left table, values to columns in the right table
#'
#' @return a joined data.table, invisibly
#' @importFrom glue glue
#' @importFrom  rlang %||%
#' @importFrom stats setNames
#' @export
#'
#' @examples
#'
#' library(data.table)
#'
#' l <- data.table(a = c(1, 2, 3), b = c(3, 4, NA))
#' r <- data.table(b = c(3, 7, NA), c = c(1, 2, 4))
#'
#' setleftjoin(l, r, by = "b")
#' setleftjoin(l, r, by = c("b" = "c"))
#'
setleftjoin <- function(l, r, by = NULL) {
  N <- NULL
  stopifnot(is.data.frame(l), is.data.frame(r))

  setDT(r)
  setDT(l)
  if (is.null(by)) {
    by <- intersect(colnames(l), colnames(r))
    message(glue("Joining on {paste(by,collapse=',')}"))
  }

  # build by.l and by.r from by
  by_l <- names(by) %||% by
  by_r <- unname(by)
  by_l[by_l == ""] <- by_r[by_l == ""]

  # fail if there's too many matching rows in r
  if (r[, .N, by = by_r][N > 1, .N] > 0) stop(glue("{deparse(substitute(r))} must have no more than one row per join column."))

  # build out column names
  on <- setNames(by_l, by_r)
  collisions <- intersect(
    setdiff(colnames(l), by_l),
    setdiff(colnames(r), by_r)
  )
  cols_r <- setdiff(colnames(r), by_r)
  cols_l <- colnames(l)
  cols_r[cols_r %in% collisions] <- paste0(cols_r[cols_r %in% collisions], ".y")
  cols_l[cols_l %in% collisions] <- paste0(cols_l[cols_l %in% collisions], ".x")

  if (length(collisions)) {
    # rename columns in l
    setnames(l,colnames(l),cols_l)
    l[, (setdiff(colnames(l), cols_l)) := NULL]
  }
  # and set columns from r
  l[, (cols_r) := r[l, setdiff(colnames(r), by_r), on = on, with = F]]

  l
}
