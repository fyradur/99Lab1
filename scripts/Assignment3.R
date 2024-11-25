set.seed(12345)
library(ggplot2)
df <- read.csv('data/pima-indians-diabetes.csv')

colnames(df) <- c('pregnant',
               'plasma',
               'blood_pressure',
               'triceps',
               'insulin',
               'bmi',
               'diabetes_pedigree',
               'age',
               'diabetes')

df$diabetes <- df$diabetes == 1

df |> ggplot() +
      geom_point(aes(x = age, y = plasma, color = diabetes)) +
      labs(x = "Age", y = "Plasma glucose concentration", color = "Has diabetes?")

df$diabetes

m = glm(diabetes ~ age + plasma, data=train, family="binomial")

df$predicted_prob <- predict(m, newdata = df, type = "response")
classification_threshold <- 0.5
df$prediction <- df$predicted_prob > classification_threshold

missclassification_rate <- sum(df$diabetes != df$prediction) / nrow(df)

# 0 = k1 + k2 * age + k3 * plasma
k1 <- summary(m)$coefficients[1]
k2 <- summary(m)$coefficients[2]
k3 <- summary(m)$coefficients[3]

# plasma = (-k1 -k2 * age) / k3
plasma_function <- function(x)(-k1 -k2 * x) / k3

df |> ggplot() +
      geom_point(aes(x = age, y = plasma, color = prediction)) +
      geom_function(fun = plasma_function) +
      labs(x = "Age", y = "Plasma glucose concentration", color = "Predicted to have diabetes?")

p <- 0.2
df$prediction02 <- df$predicted_prob > p
k4 <- log (p / (1-p))
plasma_function <- function(x)( k4 -k1 -k2 * x) / k3

df |> ggplot() +
      geom_point(aes(x = age, y = plasma, color = prediction02)) +
      geom_function(fun = plasma_function) +
      labs(x = "Age", y = "Plasma glucose concentration", color = "Predicted to have diabetes?")

p <- 0.8
df$prediction08 <- df$predicted_prob > p
k4 <- log (p / (1-p))
plasma_function <- function(x)( k4 -k1 -k2 * x) / k3


df |> ggplot() +
      geom_point(aes(x = age, y = plasma, color = prediction08)) +
      geom_function(fun = plasma_function) +
      labs(x = "Age", y = "Plasma glucose concentration", color = "Predicted to have diabetes?")


df$z1 <- df$plasma ^ 4
df$z2 <- df$plasma ^ 3 * df$age
df$z3 <- df$plasma ^ 2 * df$age ^ 2
df$z4 <- df$plasma  * df$age ^ 3
df$z5 <- df$age ^ 4

m2 = glm(diabetes ~ age + plasma + z1 + z2 + z3 + z4 + z5,
         data=df, family="binomial")

df$predicted_prob2 <- predict(m2, newdata = df, type = "response")
classification_threshold <- 0.5
df$prediction2 <- df$predicted_prob2 > classification_threshold

# 0 = k1 + k2 * age + k3 * plasma
k1 <- summary(m)$coefficients[1]
k2 <- summary(m)$coefficients[2]
k3 <- summary(m)$coefficients[3]

# plasma = (-k1 -k2 * age) / k3
plasma_function <- function(x)(-k1 -k2 * x) / k3

df |> ggplot() +
      geom_point(aes(x = age, y = plasma, color = prediction)) +
      geom_function(fun = plasma_function) +
      labs(x = "Age", y = "Plasma glucose concentration", color = "Predicted to have diabetes?")

