# Solution exercises Basic Generalized Linear Modeling – Part 1:
# from https://www.r-exercises.com/2018/07/19/basic-generalised-linear-modelling-part-1-exercises/
library(car)
library(MuMIn)

# Exercise 1
df <- read.csv(file.choose())
head(df)
scatterplotMatrix(~Srich * Habitat * Latitude * Elevation, data = df)

# exercise 2
model_glm <- glm(Srich ~ Habitat * Latitude * Elevation, family = poisson, data  = df)
vif(model_glm)

# Exercise 3
df$lat <- scale(df$Latitude, scale = T)
df$el <- scale(df$Elevation, scale = T)

# Exercise 4
model_glm_scale <- glm(Srich ~ Habitat * lat * el, family = poisson, data  = df)
vif(model_glm_scale)

# Exercise 5
influence.measures(model_glm_scale)
plot(model_glm_scale, which = 4)

# Exercise 6
model_glm_scale$deviance/model_glm_scale$df.resid

# Exercise 7
summary(model_glm_scale)

# Exercise 8
options(na.action = na.fail)
summary(model.avg(dredge(model_glm_scale), fit = TRUE, subset = TRUE))
options(na.action = "na.omit")

model_glm_2 <- glm(Srich ~ Habitat + Latitude + Elevation, family=poisson, data=df)
summary(model_glm_2)

# Exercise 9
plot(model_glm_2)

# Exercise10
xs <- seq(40, 45, l = 1000)
plot(Srich ~ Latitude, data = df, xlab = "Latitude", ylab = "Ant Species Richness")

points(Srich ~ Latitude, data = df, subset = Habitat == "Forest", pch = 16)
pred <- predict(model_glm_2, type = "response", se = T, newdata = data.frame(Latitude = xs, Habitat = "Forest", Elevation = mean(df$Elevation)))
lines(pred$fit ~ xs)
points(Srich ~ Latitude, data = df, subset = Habitat == "Bog", pch = 21)
pred <- predict(model_glm_2, type = "response", se = T, newdata = data.frame(Latitude = xs, Habitat = "Bog", Elevation = mean(df$Elevation)))
lines(pred$fit ~ xs)
legend("topright", legend = c("Forest", "Bog"), pch = c(16, 21), title = "Habitat", bty = "n")
box(bty = "l")