library(mice)
library(purrr)
mtcars2 <- mtcars
for(i in seq_along(mtcars2)){
  mtcars2[sample(nrow(mtcars2), size = 3, replace = F), i] <- NA
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
  sum(abs(x - y))
}

map2(vec, vec2, f)

df_binary <- as.data.frame(abs(is.na(mtcars2)))
y <- names(df_binary)[apply(df_binary, 2, sum) > 0]
df_binary <- df_binary[ ,y]
cor(df_binary)
cor(mtcars2, df_binary, use = 'pairwise.complete.obs')
