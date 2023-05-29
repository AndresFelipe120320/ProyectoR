library(tidyverse)
library(psych)
library(caret)
library(ggplot2)
library(caret)
DataSet1 <- Rfinal_Dataset_RR
colnames(DataSet1)
summary(DataSet1)
hist(DataSet1$Proximidad)
hist(DataSet1$Gases)
hist(DataSet1$Luz)
hist(DataSet1$Temperatura)
pairs(
  DataSet1[c("Proximidad","Gases","Luz","Temperatura")]
  ,pch=21, bg=c("black","blue","red","yellow","orange","purple","white")[unclass(DataSet1$Escenario)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Sensor 3 = Rojo Sensor 4 = Amarillo"
)
pairs.panels(
  DataSet1[c("Proximidad","Gases","Luz","Temperatura")]
  ,pch=21, bg=c("black","blue","red","yellow","orange","purple","white")[unclass(DataSet1$Escenario)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Sensor 3 = Rojo Sensor 4 = Amarillo"
)

predictors <- colnames(DataSet1)[-5]
sample.index <- sample(1:nrow(DataSet1)
                       ,nrow(DataSet1)*0.7
                       ,replace = F)
train.data <-DataSet1[sample.index,c(predictors,"Escenario"),drop=F]

test.data <-DataSet1[-sample.index,c(predictors,"Escenario"),drop=F]

head(DataSet1)
modelom <- lm(Escenario ~ Proximidad + Gases + Luz + Temperatura, data = train.data)
summary(modelom)
predictionModelo1 <- predict(modelom,test.data)

RMSEmodelo1 = data.frame(prediccion = predictionModelo1
                         ,actual = test.data
                         ,RSE = sqrt((predictionModelo1-test.data$Escenario)^2)
)
View(RMSEmodelo1)
##cross validacion
set.seed(1)
train.control <- trainControl(method = "cv", p=0.7)
# Train the model
model <- train(Escenario ~ Proximidad + Gases + Luz + Temperatura, data = train.data, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

## Arbol de decision

fit.rf1 <- randomForest(Escenario ~ Proximidad + Gases + Luz + Temperatura, data = train.data)

prediction.rf1 <- predict(fit.rf1, test.data)
output <- data.frame(test.data$Escenario, prediction.rf1)
RMSE = sqrt(sum((output$test.data.Escenario -output$prediction.rf1)^2)/
              nrow(output))

RMSE
RMSEmodelo3 = data.frame(prediccion = prediction.rf1
                         ,actual = test.data$Escenario
                         ,RSE = sqrt((prediction.rf1-test.data$Escenario)^2)
)
View(RMSEmodelo3)

##******************** GLM ******************
ModeloGlm <- glm(Escenario ~ Proximidad + Gases + Luz + Temperatura, data = train.data)
summary(ModeloGlm)
predictionModelo2 <- predict(ModeloGlm,test.data)

RMSEmodelo2 = data.frame(prediccion = predictionModelo2
                         ,actual = test.data
                         ,RSE = sqrt((predictionModelo2-test.data$Escenario)^2)
)
View(RMSEmodelo2)


## modelo de clasificacion arbol de decision
library(rpart)
library(rpart.plot)
library(randomForest)
Clasificacion1 <- Copia_de_Rfinal_Dataset
summary(Clasificacion1)
data.samples <- sample(1:nrow(Clasificacion1),
                              nrow(Clasificacion1) *0.7, replace = FALSE)
                    
    
training.data1Ad <- Clasificacion1[data.samples, ]
test.data1Ad <- Clasificacion1[-data.samples, ]

fit <- rpart(Escenario ~ Proximidad + Gases + Luz + Temperatura,
             method = "class",
             data = Clasificacion1)
plot(fit, uniform = T, margin = 0.1)
text(fit, use.n = T, all = T, cex=0.8)
rpart.plot::rpart.plot(fit, shadow.col = "blue")
fit$cptable 
fit.pruned <- prune(fit,
                    cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot::rpart.plot(fit.pruned, shadow.col = "yellow")
table(Clasificacion1$Escenario)

Clasificacion1$Escenario <- as.factor(Clasificacion1$Escenario)
fit.rf <- randomForest(Clasificacion1$Escenario ~ Clasificacion1$Proximidad + Clasificacion1$Gases + Clasificacion1$Luz + Clasificacion1$Temperatura,
                       method = "class",
                       data = Clasificacion1)

prediction.rf <- predict(fit.rf, Clasificacion1)
table(prediction.rf)

