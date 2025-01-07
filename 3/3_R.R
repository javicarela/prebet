# Redes neuronales
load("~/prebet/df.RData")
library(data.table)
library(keras)
library(tensorflow)
library(nnet)
library(caret)
setDT(df)
df_Team <- df[Team == levels(df$Team)[1] , c("V","hour","month","local","Opponent_Zone","avg_V_1")]
df_Team <- na.omit(df_Team)
df_Team$hour <- as.factor(df_Team$hour)
df_Team$local <- as.factor(df_Team$local)
ecuacion <-  V ~ hour + month + local + Opponent_Zone + avg_V_1
df_Team$hour = as.numeric(as.character(df_Team$hour))
df_Team$month = as.numeric(df_Team$month)
df_Team$local = as.numeric(df_Team$local)
df_Team$Opponent_Zone = as.numeric(df_Team$Opponent_Zone)

df_Team <- scale(df_Team)
df_Team
trainIndex <- createDataPartition(df_Team[,1], p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
trainData <- df_Team[trainIndex, ]
testData  <- df_Team[-trainIndex, ]
trainLabels <- trainData[,1]
#install.packages("RSNNS")
library(RSNNS)
model <- mlp(trainData, trainLabels, size = 10, learnFuncParams = c(0.1), maxit = 100)

# Definir el conjunto de entrenamiento (sin la variable dependiente)
train_matrix <- as.matrix(trainData[, -1])
train_matrix
# Crear y entrenar la red neuronal
model <- mlp(train_matrix, trainLabels, size = 10, learnFuncParams = c(0.1), maxit = 1000)
# Predecir usando el conjunto de prueba
predictions <- predict(model, as.matrix(testData[, -1]))

conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(testData[,1]))



