library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)
library(e1071)

#leo mi datframe
dataframe<- read_delim("C:/Users/santi/OneDrive/Documentos/UNIVERSIDAD ECCI/OCTAVO SEMESTRE/ELECTIVA AREA ELETRONICA/CORTE 2//Trabajo final2/dataset.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#--Funcion de normalizacion---#
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#----Cambio a factor mi clase----#
dataframe$ Pertenece<-
  as.factor(dataframe$Pertenece)
#---miro las proporciones de mi datframe-#
prop.table(table(dataframe$Pertenece))
#-----#
hist(dataframe$`sensor1`, breaks = 50)
hist(dataframe$`sensor2`, breaks = 50)

#--normalizo las variables--#
normData <- dataframe
standardData <- dataframe
#--minmax--#
### min-max
normData$sensor1 <-
  normalise(dataframe2$sensor1)
normData$sensor2 <-
  normalise(dataframe2$`sensor2`)
normData$sensor3 <-
  normalise(dataframe2$`sensor3`)
#-- z- score-#
standardData$sensor1 <-
  scale(dataframe2$`sensor1`)
standardData$sensor2 <-
  scale(dataframe2$`sensor2`)
standardData$sensor3 <-
  scale(dataframe2$`sensor3`)


#----Parto mi dataframe----#
sample.index <- sample(1:nrow(dataframe)
                       ,nrow(dataframe)*0.7
                       ,replace = F)

#--entrenamiento y test---#
k <- 5
predictors <- c("sensor1","sensor2")

# original data
train.data <-dataframe[sample.index,c(predictors,"Pertenece"),drop=F]
test.data <-dataframe[-sample.index,c(predictors,"Pertenece"),drop=F]
#-- cargo mi clase library---#
prediction <- knn(train = train.data[predictors], test = test.data[predictors],cl = train.data$Pertenece, k=k)
#---Gmoldes---#

CrossTable(x = test.data$Pertenece, y = prediction
           
           , prop.chisq = F)


