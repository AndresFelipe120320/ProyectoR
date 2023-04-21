library(tidyverse)
library(psych)
library(caret)
library(ggplot2)
DataSetSensores <- D2_Hoja_1
colnames(DataSetSensores)
summary(DataSetSensores)
hist(DataSetSensores$`Sensor1`)
hist(DataSetSensores$`Sensor2`)

plot(DataSetSensores$`Sensor1`,DataSetSensores$`Sensor2`,DataSetSensores$Manual
     ,bg=c("black","blue","red"))
pairs(
  DataSetSensores[c("Sensor1","Sensor2","Manual")]
  ,pch=21, bg=c("black","blue","red")[unclass(DataSetSensores$Manual)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Manual=Rojo"
)
pairs.panels(
  DataSetSensores[c("Sensor1","Sensor2","Manual")]
  ,pch=21, bg=c("black","blue","red")[unclass(DataSetSensores$Manual)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Manual=Rojo"
)
predictors <- colnames(DataSetSensores)[-3]
sample.index <- sample(1:nrow(DataSetSensores)
                       ,nrow(DataSetSensores)*0.7
                       ,replace = F)
train.data <- DataSetSensores[sample.index,c(predictors,"Manual"),drop=F]
test.data <- DataSetSensores[sample.index,c(predictors,"Manual"),drop=F]

## modelo lineal para el sensor 1 y 2 entrenamiento de datos
Sensor1_model <- lm(Manual ~ Sensor1, data=train.data)
summary(Sensor1_model)
lm(formula = Manual ~ Sensor1+Sensor2, data=train.data)

Sensor2_model <- lm(Manual ~ Sensor2, data=train.data)
summary(Sensor2_model)
#entrenar el modelo de regresión multilineal
modelo_multilineal <- lm(Manual ~ Sensor1 + Sensor2, data=train.data)

summary(Sensor1_model)

summary(Sensor2_model)

##Validación

ValidacionModelo1 <- predict(Sensor1_model,DataSetSensores)
view(ValidacionModelo1)

ValidacionModelo2 <- predict(Sensor2_model,DataSetSensores)
view(ValidacionModelo2)

ValidacionModelo3 <- predict(modelo_multilineal,DataSetSensores)
view(ValidacionModelo3)

#calculate RMSE !!!1
RMSEmodelo1 = data.frame(prediccion = ValidacionModelo1
                         ,actual = DataSetSensores$Manual
                         ,RSE = sqrt((ValidacionModelo1-DataSetSensores$Manual)^2)
)
view(RMSEmodelo1)
sum(RMSEmodelo1$RSE)/nrow(RMSEmodelo1)
##Error cuadratico modelo 2
RMSEmodelo2 = data.frame(prediccion1 = ValidacionModelo2
                         ,actual1 = DataSetSensores$Manual
                         ,RSE1 = sqrt((ValidacionModelo2-DataSetSensores$Manual)^2)
)
view(RMSEmodelo2)
sum(RMSEmodelo2$RSE)/nrow(RMSEmodelo1)
##Error cuadratico modelo multilineal
RMSEmodelo3 = data.frame(prediccion2 = ValidacionModelo3
                         ,actual2 = DataSetSensores$Manual
                         ,RSE2 = sqrt((ValidacionModelo3-DataSetSensores$Manual)^2)
)
view(RMSEmodelo3)
sum(RMSEmodelo2$RSE)/nrow(RMSEmodelo3)

#Validación cross
# Validación cruzada para el modelo del sensor 1
set.seed(15)
train.control1 <- trainControl(method = "cv", number = 10)
modelo1 <- train(Manual ~ Sensor1 + Sensor2 , data = train.data, method = "lm"
                 ,trControl = train.control1)

print(modelo1)

# Validación cruzada para el modelo del sensor 1
