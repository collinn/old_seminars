## Function to replace head()
.startup$peek <- function(x, n = 6L, r = 6L, ...) {
  UseMethod("peek")
}

## In case it's not called on  data frame or matrix
peek.default <- function(x, n = 6L, ...) {
  head(x, n, ...)
}


## Methods for peek
peek.matrix <- function(x, n = 6L, r = 6L, ...) {
  stopifnot(length(n) == 1L)
  n <- if (n < 0L)
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  r <- if(r < 0L)
    max(ncol(x) + n, 0L)
  else min(r, ncol(x))
  x[seq_len(n), seq_len(r), drop = FALSE]
}

peek.data.frame <- function(x, n = 6L, r = 6L, ...) {
  stopifnot(length(n) == 1L)
  n <- if (n < 0L)
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  r <- if(r < 0L)
    max(ncol(x) + r, 0L)
  else min(r, ncol(x))
  x[seq_len(n), seq_len(r), drop = FALSE]
}

size_of <- function(x, units = "Mb") {
  format(object.size(x), units = units)
}
