
#### Methods ####
x <- matrix(rnorm(200), ncol = 2)
y <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

## Demonstrating plot method with different classes
plot(x)
plot(y, which = 1)

class(x); class(y)
plot
methods(plot)
?plot
?plot.lm


## The generic, only calls to method
donothing <- function(x) {
  UseMethod("donothing")
}

## In case a particular method isn't found, use default
donothing.default <- function(x) {
  print("this is default, you should usually include this")
}

## Class specific methods
donothing.matrix <- function(x) {
  print("this is a matrix")
}

donothing.data.frame <- function(x) {
  print("maybe consider using data.table instead")
}

donothing.newclass <- function(x) {
  print("helpful if you want to extend previous methods with your own class")
}

z <- list(1:10)
donothing(z)

x <- matrix(rnorm(10))
donothing(x)

y <- as.data.frame(x)
donothing(y)

## Say w 'inherits' from y
w <- structure(y, class = c("newclass", class(y)))
donothing(w)


#### Be Careful! ####
x <- 1:10

## No
for(i in 1:10) {
  x[i] <- x[i]^2
}

x <- 1:10

## Better, I guess
for(i in 1:length(x)) {
  x[i] <- x[i]^2
}

## Vectorize instead
x <- rnorm(10)
y <- rnorm(10)
z <- 1

x*y + z*y - exp(x) + x/y

# ## !!!
# x*x != t(x) %*% x

## Dealing with numeric 0
x <- numeric(0L)
x
x + 10

identical(x, 0L)


## Incorrect
for(i in 1:length(x)) {
  print(i)
}

## Correct
for(i in seq_along(x)) {
  print(i)
}

## Initialize vectors
## Neat!
x <- numeric(5)
x
length(x)

## Less neat
x <- list(5)
x
length(x)

## Be more explicit
x <- vector("numeric", length = 5L)
y <- vector("list", length = 5L)
x
y


## Choose better initial value than R
## Initialized with 0
y <- vector("numeric", length = 1000L)

set.seed(69)
for(i in seq_along(y)) {
  u <- runif(1)
  if(u < 0.8) {
    y[i] <- rbinom(1, 1, 0.75)
  }
}

table(y, useNA = "always")

## Initialized with NA
y <- NA*vector("numeric", length = 1000L)

set.seed(69)
for(i in seq_along(y)) {
  u <- runif(1)
  if(u < 0.8) {
    y[i] <- rbinom(1, 1, 0.75)
  }
}

table(y, useNA = "always")


#### Don't be slow ####

library(rbenchmark)

## Growing vectors
grow_vec <- function(n) {
  x <- c()
  for(i in seq_len(n)) {
    x <- cbind(x, rnorm(1))
  }
}

init_vec <- function(n) {
  x <- vector("numeric", n)
  for(i in seq_len(n)) {
    x[i] <- rnorm(1)
  }
}


benchmark(
  grow_vec(n = 10000),
  init_vec(n = 10000),
  replications = 25
)

## Using apply
x <- matrix(rnorm(100000*100), ncol = 100)

## Using apply
f_apply <- function(x) {
  res <- apply(x, 2, mean)
}

## Loops
f_loop <- function(x) {
  res <- vector("numeric", ncol(x))
  for(i in seq_along(ncol(x))) {
    res[i] <- mean(x[, i])
  }
}

## And lapply
f_lapply <- function(x) {
  res <- lapply(as.data.frame(x), mean, numeric(1))
  unlist(res, use.names = FALSE)
}

benchmark(f_apply(x), 
          f_loop(x),
          f_lapply(x),
          replications = 50)

apply

x <- rpois(n = 500000, lambda = 20)

benchmark(
  lapply(x, function(y) sqrt(y) < 5),
  vapply(x, function(y) (sqrt(y) < 5), logical(1)),
  replications = 25
)

## But wait, there's more

## Unlist AND do type-checking
f_lapply2 <- function(x) {
  res <- lapply(x, function(y) {
    tt <- (sqrt(y) > 5)
    ## Type checking
    if(!is.logical(tt) | length(tt) != 1) stop("stop")
  })
  ## Unlisting
  unlist(res, use.names = FALSE)
}

benchmark(
  vapply(x, function(y) sqrt(y) > 5, logical(1)),
  f_lapply2(x),
  replications = 25
)


## Squish example
## Squish numeric vector x into interval [a, b]
squish_ife <- function(x, a, b) {
  ifelse(x <= a, a, ifelse(x >= b, b, x))
}

squish_p <- function(x, a, b) {
  pmax(pmin(x, b), a)
}

squish_in_place <- function(x, a, b) {
  x[x <= a] <- a
  x[x >= b] <- b
  x
}

x <- rpois(n = 50000, lambda = 20)
benchmark(squish_ife(x, 15, 25),
          squish_p(x, 15, 25),
          squish_in_place(x, 15, 25))


## Unlist
x <- matrix(rnorm(1e6*2), nrow = 2)
res <- lapply(as.data.frame(x), mean)

## We often don't need the names of the listed elements
benchmark(
  tt <- unlist(res),
  tt2 <- unlist(res, use.names = FALSE)
)

## Rbindlist
library(data.table)

datasets <- lapply(1:1000, function(x) {
  dat <- data.frame(ID = rep(x, 15), 
                    A = rnorm(15),
                    B = rnorm(15),
                    C = rnorm(15))
})

f_loop <- function(x) {
  res <- data.frame()
  for(i in seq_along(x)) {
    res <- rbind(res, x[[i]])
  }
}

benchmark(
  f_loop(datasets),
  Reduce(rbind, datasets),
  rbindlist(datasets),
  replications = 10
)

# ## My typical pattern
# result <- lapply(datasets, function(x) {
#   # do something
# }) %>% rbindlist()


#### Useful functions ####
x <- matrix(rnorm(1e6*2), nrow = 2)
object.size(x)
?object.size

size_of <- function(x, units = "Mb") {
  format(object.size(x), units = units)
}

size_of(x)

## Function to remove null values from list
compact <- function(x) {
  Filter(Negate(is.null), x)
}

x <- vector("list", 5)
for(i in c(1, 3, 5)) {
  x[[i]] <- i
}

x
(x <- compact(x))

## Simulate model failure
dummy_fit <- function() {
  u <- runif(1)
  if(u < 0.8) {
    res <- lm(Sepal.Width ~ Sepal.Length + Petal.Length, data = iris)
  } else {
    res <- NULL
  }
  return(res)
}

set.seed(69)
model_fits <- replicate(20, dummy_fit())
length(model_fits)
length(compact(model_fits))

## First which
x <- 1:10
idx <- which(x < 5)
x[idx]


## Then where
where <- function(x, f, only.true = FALSE) {
  res <- vapply(x, f, logical(1))
  if(only.true) return(names(res[res == TRUE]))
  res
}

## Subsetting by logicals equivalent to subsetting by position
idx <- where(x, function(y) (y < 5))
x[idx]
x[which(idx)]


## Where is the iris dataset numeric?
(vars <- where(iris, is.numeric))

## Maybe we only want where it's true
(num_vars <- where(iris, is.numeric, only.true = TRUE))

## Only vars that are numeric
(num_vars <- where(iris, is.numeric, only.true = TRUE))
mean_val <- vector("numeric", length(num_vars))
names(mean_val) <- num_vars

for(var in num_vars) {
  mean_val[var] <- mean(iris[[var]])
}

#### Bonus ####
library(data.table)

## Vector recycle
x <- 1:20
x

## Recycle 2, get evens
x[c(FALSE, TRUE)]

## Recycle 3, get multiples of 3
x[c(FALSE, FALSE, TRUE)]

## Make column names
(col_names <- paste0("col", seq_len(10)))


## Vectorization
x <- seq_len(10)
y <- NA*x
z <- NA*x

## Not vectorized :(
if(x < 5) {
  y <- x
} else {
  y <- 5
}
print(y)

## Vectorized!
ifelse(x < 5, z <- x, z <- 5)

x <- c(TRUE, TRUE, FALSE)
y <- c(TRUE, FALSE, FALSE)
x; y

## Only inspects first element
x && y
x || y

## Inspects all elements
x & y
x | y


# USArrests["Murder" > 13.2, ]

usa <- as.data.table(USArrests, keep.rownames = TRUE)
head(usa)

## Here we are subsetting by logical vectors
usa[Murder > 13.1 & Assault > 230, ]

## And again, but with a different logical vector
usa[Murder > 13.1 && Assault > 230, ]


## What do those vectors look like?
attach(usa) # <- adds var names of usa to global environment

## Returns appropriate length
Murder > 13.1 & Assault > 230

## Recycles length one vector, selecting all columns
Murder > 13.1 && Assault > 230












