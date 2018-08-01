randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(mean)
randomise(sum)

l <- replicate(20, runif(sample(1:10, 1)), simplify = FALSE)

# With a for loop
out <- vector("list", length(l))
for (i in seq_along(l)) {
  out[[i]] <- length(l[[i]])
}
unlist(out)

# With lapply
unlist(lapply(l, length))
unlist(lapply(mtcars, class))
mtcars[] <- lapply(mtcars, function(x) x / mean(x))

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
unlist(lapply(trims, function(trim) mean(x, trim = trim)))

scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
sapply(mtcars, scale)

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
x <- lapply(formulas, function(x) lm(x, data = mtcars))

bootstraps <- lapply(1:10, function(x) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

fit <- lapply(bootstraps, function(i) lm(formulas[[3]], data = i))
rsq <- function(mod) summary(mod)$r.squared
lapply(fit, rsq)
mean(unlist(lapply(fit, rsq)))

# Generate some sample data
xs <- replicate(5, runif(10), simplify = FALSE)
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE)

unlist(lapply(xs, mean))
unlist(Map(weighted.mean, xs, ws))

rollmean <- function(x, n) {
  out <- rep(NA, length(x))
  
  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- mean(x[(i - offset):(i + offset - 1)])
  }
  out
}
x <- seq(1, 3, length = 1e2) + runif(1e2)
plot(x)
lines(rollmean(x, 5), col = "blue", lwd = 2)
lines(rollmean(x, 10), col = "red", lwd = 2)

x <- seq(1, 3, length = 1e2) + rt(1e2, df = 2) / 3
plot(x)
lines(rollmean(x, 5), col = "red", lwd = 2)

rollapply <- function(x, n, f, ...) {
  out <- rep(NA, length(x))
  
  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- f(x[(i - offset):(i + offset - 1)], ...)
  }
  out
}
plot(x)
lines(rollapply(x, 5, median), col = "red", lwd = 2)

lapply3 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in sample(seq_along(x))) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}
unlist(lapply(1:10, sqrt))

vapply(mtcars[ ,vapply(mtcars, is.numeric, logical(1))], sd, numeric(1))

trials <- replicate(
  100, 
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

sapply(1:length(trials), function(x) trials[[x]]$p.value)
lapply(trials, '[')

Reduce(`+`, 1:3)
Reduce(sum, 1:3)

Reduce2 <- function(f, x) {
  out <- x[[1]]
  for(i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}
l <- replicate(5, sample(1:10, 15, replace = T), simplify = FALSE)
str(l)
Reduce(intersect, l)

vapply(Filter(is.numeric, mtcars), summary, numeric(6))

x <- matrix(rnorm(20, 0, 10), nrow = 4)
x1 <- sweep(x, 1, apply(x, 1, min), `-`)
x2 <- sweep(x1, 1, apply(x1, 1, max), `/`)

power1 <- function(x){
  force(x)
  function(exp){
    exp ^ x
  }
}
cube <- power1(3)
cube(2)

lprob_poisson <- function(lambda, x) {
  n <- length(x)
  log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
}
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poisson(10, x1)
#> [1] -184
lprob_poisson(20, x1)
#> [1] -61.1
lprob_poisson(30, x1)
#> [1] -31
ll_poisson <- function(x) {
  n <- length(x)
  
  function(lambda) {
    log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
  }
}
ll_poisson <- function(x){
  n <- length(x)
  sum_x <- sum(x)
  c <- sum(lfactorial(x))
  
  function(lambda){
    log(lambda) * sum_x - n * lambda - c
  }
}

ll_poisson(x1)(3)

boot_permute <- function(df, var) {
  n <- nrow(df)
  
  function() {
    df[[var]][sample(n, n, replace = TRUE)]
  }
}

boot_mtcars1 <- boot_permute(mtcars, "mpg")
head(boot_mtcars1())
#> [1] 22.8 26.0 33.9 18.1 21.0 10.4
head(boot_mtcars1())

boot_model <- function(df, formula) {
  mod <- lm(formula, data = df)
  fitted <- unname(fitted(mod))
  resid <- unname(resid(mod))
  rm(mod)
  
  function() {
    fitted + sample(resid)
  }
} 

boot_mtcars2 <- boot_model(mtcars, mpg ~ wt)
head(boot_mtcars2())
#> [1] 29.7 21.2 20.3 22.2 16.1 23.0
head(boot_mtcars2())

names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)
funs <- purrr::map(names, power1)
funs$root(64)