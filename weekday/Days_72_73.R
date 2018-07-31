library(rlang)

search_envs <- function() {
  rlang:::new_environments(c(
    list(global_env()),
    head(env_parents(global_env()), -1)
  ))
}

e1 <- env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3,
)
e1
env_print(e1)
identical(current_env(), global_env())
global_env() == current_env()

e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)
env_print(e2b)
env_print(e2a)
e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)
env_print(e2c)
env_print(e2d)
parent.env()

e3 <- env(x = 1, y = 2)
e3$x
e3$z <- 3
e3[["z"]]
env_poke(e3, "a", 100)
e3$a
env_bind(e3, a = 10, b = 20)
env_names(e3)
env_has(e3, "a")

e3$a <- NULL
env_has(e3, "a")
env_unbind(e3, "a")
env_has(e3, "a")

env_bind_exprs(current_env(), b = {Sys.sleep(1); 1})
system.time(print(b))
system.time(print(b))

env_bind_fns(current_env(), z1 = function(val) runif(1))
z1
z1
env_bind_fns(current_env(), z2 = function(val) {
  if (missing(val)) {
    2
  } else {
    stop("Don't touch z2!", call. = FALSE)
  }
})
z2
z2 <- 3

x <- env()
x$e <- x
x <- env()
y <- env()
x$y <- y
y$x <- x
env_print(x)
env_print(y)

my_poke <- function(name, value, env = globalenv()){
  assign(name, value, env = env)
}

where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}
where('yyy')
where('a')
where("mean")
env_parent(global_env())

my_str <- function(x){
  str(x)
  where(x)
}

my_ls <- function(){
  sort(names(globalenv()))
}
my_ls()

h <- function() abort("This is an error!")
f()

errorR <- function(x){
  if(!exists(x)){
    rlang::abort('file "x" does not exist!!!', call = F)
  } else {
    file.remove(x)
  }
}
errorR('data/123.txt')

cnd <- catch_cnd(abort("An error"))
str(cnd)

fail_with <- function(expr, value = NULL) {
  tryCatch(
    error = function(cnd) value,
    expr
  )
}

fail_with(log(10), NA_real_)
fail_with(log("x"), NA_real_)

safety <- function(expr) {
  tryCatch(
    error = function(cnd) {
      list(result = NULL, error = c)
    },
    list(result = expr, error = NULL)
  )
}
str(safety(10))
str(safety(above('Stop')))