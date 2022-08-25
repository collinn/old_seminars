library(microbenchmark)
library(rbenchmark)
### Startup ###

## Let's test .Rprofilel startup
x <- matrix(rnorm(100*100), ncol = 10)
peek(x)

## And loaded  packages
x <- as.data.table(x)
colMeans(x) %>% sum


### Be Cautious! ###

## Making assignment
x <- numeric(5)
x
length(x)

x <- list(5)
x
length(x)

## Instead
x <- NA*vector("numeric", length = 5)
y <- vector("list", length = 5)
x; y
rm(x); rm(y)

## What about loops?
x <- numeric(0L)
class(x); length(x)
x+10

## Incorrect
for(i in 1:length(x)) {
  print(i)
}

## Correct
for(i in seq_along(x)) {
  print(i)
}

## One more error (R Inferno) 
name_val <- c(4, 7)
vec <- rep(0, length(name_val))
names(vec) <- name_val
for(i in name_val) {
  vec[i] <- 31
}
vec

### Let's not be unnecessarily slow ###

## Initialize vs growing vectors
grow_vec <- function(n) {
  x <- c()
  for(i in seq_len(n)) {
    x[i] <- rnorm(1)
  }
}

init_vec <- function(n) {
  x <- vector("numeric", n)
  for(i in seq_len(n)) {
    x[i] <- rnorm(1)
  }
}

grow_list <- function(n) {
  x <- list()
  for(i in seq_len(n)) {
    x[[i]] <- matrix(rnorm(100), ncol = 10)
  }
}

init_list <- function(n) {
  x <- vector("list", n)
  for(i in seq_len(n)) {
    x[[i]] <- matrix(rnorm(100), ncol = 10)
  }
}

library(microbenchmark)
library(rbenchmark)
benchmark(grow_list(1e5), 
               init_list(1e5),
          replications = 10)

n <- c(10000, 50000, 100000, 1000000)
t1 <- numeric(length(n))
t2 <- numeric(length(n))
t3 <- numeric(length(n))
t4 <- numeric(length(n))
for(i in seq_along(n)) {
  t1[i] <- system.time(grow_vec(n[i]))[3]
  t2[i] <- system.time(init_vec(n[i]))[3]
  
  t3[i] <- system.time(grow_list(n[i]))[3]
  t4[i] <- system.time(init_list(n[i]))[3]
}

plot(1:4, t1, ylab = "time", col = 'red', type = 'b')
lines(1:4, t2, col = 'blue', type = 'b')
plot(1:4, t3, ylab = "time", col = 'red', type = 'b')
lines(1:4, t4, col = 'blue', type = 'b')


## Loops are bad, apply is good
x <- matrix(rnorm(100000*100), ncol = 100)

f_apply <- function(x) {
  res <- apply(x, 2, mean)
}

f_loop <- function(x) {
  res <- vector("numeric", ncol(x))
  for(i in seq_len(ncol(x))) {
    res[i] <- mean(x[, i])
  }
  res
}

f_lapply <- function(x) {
  res <- lapply(as.data.frame(x), mean, numeric(1))
  unlist(res, use.names = FALSE)
}
library(rbenchmark)
benchmark(f_apply(x), 
          f_loop(x),
          f_lapply(x),
          replications = 25)

### Other times when base R is too much ###

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


## We often don't need names from a list when 
## they are computational output of lapply
x <- matrix(rnorm(1e6*2), nrow = 2)
res <- lapply(as.data.frame(x), mean)


benchmark(
tt <- unlist(res),
tt2 <- unlist(res, use.names = FALSE)
)

### Some other useful functions ###

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


## Function to remove null values from list
compact <- function(x) {
  Filter(Negate(is.null), x)
}

## Remove NULL models
model_fits <- compact(model_fits)
length(model_fits)


### Determine where an expression is TRUE (similar to which)
where <- function(x, f, names = FALSE) {
  res <- vapply(x, f, logical(1))
  if(names) return(names(res[res == TRUE]))
  res
}

## Only get names of TRUE values
(num_vars <- where(iris, is.numeric, names = TRUE))

## Get pairlist names/logical
(nn_vars <- where(iris, is.numeric))

## Keeps indices of TRUE
which(nn_vars)

## Loop through names to modify in place (especially helpful w/ data.table)
for(var in num_vars) {
  iris[[var]] <- sqrt(iris[[var]])
}

### Bonus Small Stuff ###

## Vector recycle
x <- 1:20

## Recycle 2, get evens
x[c(FALSE, TRUE)]

## Recycle 3, get multiples of 3
x[c(FALSE, FALSE, TRUE)]

## Make some column names
x <- matrix(rnorm(100*10), ncol = 10)
head(x)
(col_names <- paste0("col", seq_len(ncol(x))))
colnames(x) <- col_names

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

x <- rep(c(TRUE, FALSE), times = 5)
y <- rep(c(TRUE, FALSE), each = 5)

## Only inspects first element
x && y
x || y

## Inspects all elements
x & y
x | y

head(USArrests)
USArrests["Murder" > 13.2, ]

usa <- as.data.table(USArrests, keep.rownames = TRUE)
usa
usa[Murder > 13.1 & Assault > 230, ]
usa[Murder > 13.1 && Assault > 230, ]

## Put usa vars in environment
attach(usa)

## Recycles lenght one vector
Murder > 13.1 && Assault > 230
## Returns appropriate length
Murder > 13.1 & Assault > 230



