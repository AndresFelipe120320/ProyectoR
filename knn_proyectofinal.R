library(tidyverse)
library(caret)

Dataset1 <- Copia_de_Rfinal_Dataset_3_

Dataset1$Escenario <- as.factor(Dataset1$Escenario)
sample.index <- sample(1:nrow(Dataset1)
                       ,nrow(Dataset1)*0.7
                       ,replace = F)

predictors <- 
  c("Proximidad", "Gases", "Luz", "Temperatura")

train.data <- Dataset1[sample.index
                       ,c(predictors, "Escenario")
                          ,drop=F]
test.data <- Dataset1[-sample.index
                      ,c(predictors, "Escenario")
                      ,drop=F]

ctrl <- trainControl(method = "cv", p=0.7)
knnFit <- train(Escenario ~ Proximidad+Gases+Luz+Temperatura
                ,data=train.data
                ,method = "knn", trControl = ctrl
                , preProcess= c("range")# c("scale)for z-score
                , tuneLength = 20)


knnFit
plot(knnFit)

knnPredict <- predict(knnFit, newdata = test.data)

confusionMatrix(knnPredict
                , test.data$Escenario)
