### Configure .Rprofile for startup ###
## Thanks Dirk and others

## This is bad. Why?
options(stringsAsFactors=FALSE) 

## This is OK, scientific notation is hard
options(scipen=50)

## Format printing output
options(width=80, digits = 5)

## Especially nice when quitting from command line
q <- function (save="no", ...) {
  quit(save=save, ...)
}

## Source any functions used in analysis (directory specfic)
sourceDir <- function(path, trace = FALSE, ...) {
  for(nm in list.files(path, pattern = "\\.[Rr]")) {
    if(trace) cat(nm, ":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

## Startup environment
.startup <- new.env()

.startup$sshhh <- function(a.package) {
  suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only = TRUE)))
}

.startup$auto_loads <- c("data.table", "magrittr")

## Only run if interaction
if(interactive()) {
  invisible(sapply(.startup$auto_loads, .startup$sshhh))
}

## Function to replace head()
.startup$peek <- function(x, n = 6L, r = 6L, ...) {
  UseMethod("peek")
}

## In case it's not called on  data frame or matrix
.startup$peek.default <- function(x, n = 6L, ...) {
  head(x, n, ...)
}


## Methods for peek
.startup$peek.matrix <- function(x, n = 6L, r = 6L, ...) {
  stopifnot(length(n) == 1L)
  n <- if (n < 0L)
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  r <- if(r < 0L)
    max(ncol(x) + n, 0L)
  else min(r, ncol(x))
  x[seq_len(n), seq_len(r), drop = FALSE]
}

.startup$peek.data.frame <- function(x, n = 6L, r = 6L, ...) {
  stopifnot(length(n) == 1L)
  n <- if (n < 0L)
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  r <- if(r < 0L)
    max(ncol(x) + r, 0L)
  else min(r, ncol(x))
  x[seq_len(n), seq_len(r), drop = FALSE]
}

.startup$size_of <- function(x, units = "Mb") {
  format(object.size(x), units = units)
}

## First function ran
.First <- function() {
  if(interactive()){
    library(utils)
    timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))
  }
  sourceDir("./functions")
  cat("\n  Hey Collin! You are so cool and have so many friends!\n\n")
  attach(.startup)
}

.Last <- function() {
  if(interactive()) {
    hist_file <- Sys.getenv("R_HISTFILE")
    if(hist_file=="") hist_file <- ".RHistory"
    savehistory(hist_file)
  }
}

## Alternatively, if you have an R file of startup functions
#sys.source("myfuns.R", envir = attach(NULL, name = ".startup"))


