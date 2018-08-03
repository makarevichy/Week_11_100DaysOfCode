y <- x * 10
z <- quote(y <-  x * 10)
x <- 4
eval(z)
y

lobstr::ast(f(x, "y", 1))
lobstr::ast(y <- x * 10)
lobstr::ast(names(x) <- y)
lobstr::ast(for(i in seq_along(1:3)){
  x <- i + i
})

lobstr::ast(1 + 2 * 3)
lobstr::ast(f((1)))
lobstr::ast(`(`(1 + 1))
lobstr::ast(sqtr(sum(mean(x) / x)))
lobstr::ast(x1 <- x2 <- x3 <- 0)

x <- expr(read.table("important.csv", row = FALSE))
lobstr::ast(!!x)

#pairlist
f <- function(x = 10) x + 1
typeof(formals(f))
pl <- pairlist(x = 1, y = 2)
length(pl)
pl$x

l1 <- as.list(1:100)
l2 <- as.pairlist(l1)

microbenchmark::microbenchmark(
  l1[[1]],
  l1[[100]],
  l2[[1]],
  l2[[100]]
)

x1 <- "y <- x + 10"
lobstr::ast(!!x1)
x2 <- rlang::parse_expr(x1)
x2
lobstr::ast(!!x2)
expr_text(parse_expr("!!x"))
expr <- expr(g(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z))
expr_text(expr)
expr_name(expr)
expr_label(expr)

cement <- function(...) {
  dots <- exprs(...)
  paste(purrr::map(dots, expr_name), collapse = " ")
}
cement('good', day)

library(dplyr)
f <- function(df, var1, var2, f) {
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  nam <- quo(f)
  df %>% 
    group_by(!!var1) %>% 
    summarise(nam = f(!!var2))
}

mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(mpg))

f1 <- function(x, y) {
  exprs(x = x, y = y)
}
f2 <- function(x, y) {
  enexprs(x = x, y = y)
}
f1(a + b, c + d)
f2(a + b, c + d)


x <- 10
y <- 20
f <- eval_bare(expr(function(x, y) !!x + !!y))
f

q2 <- quo(x + !!x)
q2

y <- quo(long_function_name(
  argument_1 = long_argument_value,
  argument_2 = long_argument_value,
  argument_3 = long_argument_value,
  argument_4 = long_argument_value
))
quo_name(y)   # e.g. for data frames
#> [1] "long_function_name(...)"
quo_label(y)  # e.g. for error messages
#> [1] "`long_function_name(...)`"
quo_text(y)   # for longer messages  

n <- 1000
x1 <- expr(runif(n))
e1 <- globalenv()
q1 <- quo(runif(n))

microbenchmark::microbenchmark(
  runif(n),
  eval_bare(x1, e1),
  eval_tidy(q1),
  eval_tidy(q1, mtcars)
)  