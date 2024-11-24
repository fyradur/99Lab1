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

df |> ggplot() +
      geom_point(aes(x = age, y = plasma, color = prediction)) +
      labs(x = "Age", y = "Plasma glucose concentration", color = "Predicted to have diabetes?")
