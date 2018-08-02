library(pryr)
library(sloop)
otype(1:10)
#> [1] "base"

otype(mtcars)
#> [1] "S3"

mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
otype(mle_obj)

# A base object:
is.object(1:10)

# An OO object
is.object(mtcars)
S3Class(1L)

f <- factor("a")
typeof(f)
attributes(f)

new_Date <- function(x) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}

new_Date(c(-1, 0, 1))

new_Date <- function(x){
  sloop::new_s3_dbl(x, class = 'Date')
}
new_Date(1)

new_POSIXct <- function(x, tzone = "") {
  stopifnot(is.double(x))
  stopifnot(is.character(tzone), length(tzone) == 1)
  
  structure(x, 
    class = c("POSIXct", "POSIXt"),
    tzone = tzone
  )
}

new_POSIXct(1)
new_POSIXct(1, tzone = "UTC")

fit <- lm(mpg ~ wt, data = mtcars)
class(fit) <- 'vector'
class(fit) <- 'scalar'
attributes(difftime(Date(2018, 05, 18), Date(2018, 04, 14)))

attributes(difftime(Date(2018, 05, 18), Date(2018, 04, 14)))
date_new <- function(x, attr = c('days', 'months')){
  stopifnot(is.double(x))
  stopifnot(is.character(attr))
  attr <- match.arg(attr)
  structure(x, units = attr, class = 'difftime')
}

x <- replicate(5, runif(5), simplify = F)
attributes(new_data.frame(x))