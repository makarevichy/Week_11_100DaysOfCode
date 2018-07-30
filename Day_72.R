library(mice)
mtcars2 <- mtcars
for(i in seq_along(mtcars2)){
  mtcars2[sample(nrow(mtcars2), size = 2, replace = F), i] <- NA
}

val_na <- lapply(mtcars2, function(x) which(is.na(x)))

vec <- replicate(11, NULL)
for (i in seq_along(val_na)) {
  x <- mtcars[[i]][val_na[[i]]]
  vec[[i]] <- x
}

eval_na <- mice(mtcars2, m = 5, method = 'cart', printFlag = T)
fit_na <- complete(eval_na, action = 3)

vec2 <- replicate(11, NULL)
for (i in seq_along(val_na)) {
  x <- fit_na[[i]][val_na[[i]]]
  vec2[[i]] <- x
}

f <- function(x, y){
  mean(abs(x - y))
}

map2(vec, vec2, f)