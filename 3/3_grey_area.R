# Carga de las liberrías y datos
library(data.table)
library(sjPlot)
library(caret)
library(party)
library(randomForest)
library(e1071)
load("~prebet/data_frames/NBA_games_info.RData")
# Análisis de la zona gris #
#1. Fijamos un delta
delta <- 0.15
# Seleccionamos nuestra df de trabajo
setDT(df)
ec_optima <- V ~ hour + weekday    + Opponent_Zone + temporada +  local + avg_V_1 + 
  avg_V_4 + avg_V_8 + avg_V_12 + avg_V_16 + avg_V_32 + avg_P_1 + avg_P_4    +  
  avg_V_o_4 + avg_V_o_8 + avg_V_o_12  + avg_V_o_32 + n_victorias +
  n_victorias_o + days_since_last + days_since_last_o + odds + odds.opponent

df_Team <- df[df$Team == levels(df$Team)[1],]
trainData <- df_Team[!temporada==2024, ]
testData  <- df_Team[temporada==2024, ]

model_team <- glm( ec_optima ,
                   data = trainData, 
                   family = binomial(link = "logit"))
predictions <- predict(model_team, newdata = testData, type = "response")
predictions <- ifelse(predictions > 0.35 , ifelse(predictions > 0.65, 1, "Z_G"), 0)

tabla <- table(predictions, as.factor(testData$V) )
tabla <- tabla[rownames(tabla) != "Z_G", ]
confusionMatrix(tabla)


check_modelo <- function(df){
  n_predicciones <- c()
  n_total <- c()
  accuracies_d <- c()
  accuracies_v <- c()
  accuracies <- c()
  for (i in 1:length(levels(df$Team))){
    df_Team <- df[Team == levels(df$Team)[i] ,]
    set.seed(123)
    trainIndex <- createDataPartition(df_Team$V, p = 0.7, 
                                      list = FALSE, 
                                      times = 1)
    trainData <- df_Team[!temporada==2024, ]
    testData  <- df_Team[temporada==2024, ]
    model_team <- glm( ec_optima,
                       data = trainData, 
                       family = binomial(link = "logit"))
    try(predictions <- predict(model_team, newdata = testData, type = "response"))
    try(predictions <- ifelse(predictions > 0.35, ifelse(predictions > 0.65, 1, "Z_G"), 0))
    try(tabla <- table(predictions, as.factor(testData$V) ))
    try(tabla <- tabla[rownames(tabla) != "Z_G", ])
    try(conf_matrix <- confusionMatrix(tabla))
    try(n_predicciones_e <- sum(tabla))
    try(n_total <- nrow(testData))
    try(acc_derrotas <- conf_matrix$byClass[3])
    try(acc_victorias <- conf_matrix$byClass[4])
    try(valor_prediccion <- conf_matrix$overall[1])
    try(cat(" \n El número de predicciones es ", n_predicciones_e , "sobre ", n_total))
    #cat(levels(df$Team)[i], "|Acc derrotas ", "del ", round(acc_derrotas*100,2), "%| /n")
    #cat("                    |Acc victorias " , "del ", round(acc_victorias*100,2), "%| /n")
    #cat("                    |Acc media ", "del ", round(valor_prediccion*100,2), "%| /n")  
    try(n_predicciones <-c(n_predicciones, n_predicciones_e))
    try(accuracies_d <-c(accuracies_d, acc_derrotas))
    try(accuracies_v <-c(accuracies_v, acc_victorias))
    try(accuracies <-c(accuracies, valor_prediccion))
  }
  cat("===================================== \n
     |  El número total de predicciones es  " , sum(n_predicciones), "que representa un " , sum(n_predicciones)/nrow(df[temporada==2024, ])*100,"%" )
  cat("===================================== \n 
     |   La predicción media de las derrotas es " , mean(accuracies_d)*100 , "%    " )
  cat("===================================== \n 
     |   La predicción media de las victorias es " , mean(accuracies_v)*100 , "%    " )
  cat("===================================== \n 
     |   La predicción media del modelo es" , mean(accuracies)*100 , "%    " )
}

try(check_modelo(df))

#    El número total de predicciones es   1361 que representa un  51.59212 %===================================== 
#    La predicción media del modelo es 71.38146 %    



## Análisis de bayesian models
# Selección para 2025 


df_bayes <- df[,c("V","hour","weekday","month","Opponent_Zone","temporada","local",
                  "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
                  "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
                  "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
                  "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
                  "avg_P_o_32" ,"days_since_last","days_since_last_o","games_played",
                  "odds","odds.opponent")]
df_bayes <- na.omit(df_bayes)
df_bayes <- na.omit(df_bayes)

factor_columns <- sapply(df_bayes, is.factor)
factor_columns_names <- names(df_bayes)[factor_columns]

# Convertir las columnas de tipo factor a numérico
for (col in factor_columns_names) {
  df_bayes[[col]] <- as.numeric(as.factor(df_bayes[[col]]))
}

data_train <- df_bayes[!temporada<2024, ]
data_test  <- df_bayes[temporada==2024, ]  
#data_test <- data_test[(odds) <= 1.5 | (odds) >=2.5 , ]



topredict_set<-data_test[,c(2:33)] 
model_bayes <- naiveBayes(formula = V ~. - odds - odds.opponent,
                          data = data_train)
model_bayes
preds_bayes <- predict(model_bayes, newdata = topredict_set, type = "raw") 
preds_bayes <- ifelse(preds_bayes[,2] > 0.3 , ifelse(preds_bayes[,2] > 0.7, 1, "Z_G"), 0)


(conf_matrix_bayes <- table(preds_bayes, data_test$V))   

confusionMatrix(conf_matrix_bayes[1:2,]) 

#                Accuracy : 0.6646          
#                Accuracy : 0.6778


## En ctree

control <- ctree_control(
  minsplit = 100,    # Número mínimo de observaciones en un nodo para intentar una partición
  minbucket = 25,   # Número mínimo de observaciones en cualquier nodo terminal
  maxdepth = 3,     # Profundidad máxima del árbol
  mincriterion = 0.95  # Valor mínimo del criterio de información (mayor valor es más restrictivo)
)
maintenance_tree <- ctree(formula = V~.  , data = data_train)
#control =  control)

pred_tree <- predict(maintenance_tree, newdata = data_test, type="response")

pred_tree <- ifelse(pred_tree > 0.3 , ifelse(pred_tree > 0.7, 1, "Z_G"), 0)


(conf_matrix_bayes <- table(pred_tree, data_test$V))   

confusionMatrix(conf_matrix_bayes[1:2,]) 
sum(conf_matrix_bayes[1:2,])
#  Accuracy : 0.7771     n = 803     

# with odds        Accuracy : 0.8206      # 1187  

library("e1071")
library(caret)

svm_model1 <- svm(ec_optima,kernel = "radial", data=data_train)
pred1 <- predict(svm_model1, newdata = data_test)
pred_svm <- ifelse(pred1 > 0.3 , ifelse(pred1 > 0.7, 1, "Z_G"), 0)
pred_svm <- as.factor(pred_svm)
data_test$V <- as.factor(data_test$V)
confusion_matrix <- table(pred_svm,data_test$V)
confusionMatrix(confusion_matrix[1:2,] )
# Accuracy : 0.7262          


# Realizamos un greed para encontrar el óptimo en los valores de rondas, max_dephts y deltas
 
 # Cargar las librerías necesarias
 library(xgboost)
 library(caret) # Para confusionMatrix
 
 # Definir los parámetros a probar
 n_rounds_s <- c( 1000,1250,1500)
 max_depths <- c(2, 3)
 deltas <- c(0.05 , 0.1)
 etas <- c(0.05,0.01 ,0.005,0,001)
 # Crear un data.frame para guardar los resultados
 results <- data.frame(n_rounds = integer(), 
                       max_depth = integer(), 
                       delta = numeric(),
                       eta = numeric(),
                       accuracy = numeric(),
                       n = integer())  # Agregar columna para 'n'
 
 # Generar todas las combinaciones de n_rounds, max_depth y delta
 param_combinations <- expand.grid(n_rounds = n_rounds_s, max_depth = max_depths, delta = deltas, eta = etas)
 
 # Bucle a través de cada combinación de parámetros
 for (i in 1:nrow(param_combinations)) {
   n <- NULL
   accuracy <- NULL
   n_rounds <- param_combinations$n_rounds[i]
   max_depth <- param_combinations$max_depth[i]
   delta <- param_combinations$delta[i]
   eta <- param_combinations$eta[i]
   cat("Ejecutando combinación:", i, "/", nrow(param_combinations), 
       "- n_rounds:", n_rounds, ", max_depth:", max_depth, ", delta:", delta, "\n", 
       eta, "\n")
   
   # Entrenar el modelo XGBoost
   bstSparse <- xgboost(data = as.matrix(data_train[,-1]), 
                        label = as.matrix(data_train[,1]), 
                        gamma = 1,  
                        verbose  = FALSE, 
                        max.depth = max_depth, 
                        eta = eta, 
                        nthread = 14, 
                        nrounds = n_rounds, 
                        objective = "binary:logistic")
   
   # Realizar predicciones
   pred <- predict(bstSparse, as.matrix(data_test[,-1]))
   pred_xgboost <- ifelse(pred > (0.5 - delta), ifelse(pred > (0.5 + delta), 1, "Z_G"), 0)
   pred_xgboost <- as.factor(pred_xgboost)
   
   # Crear la matriz de confusión y calcular la precisión
   confusion_matrix <- table(pred_xgboost, data_test$V)
   try(conf <- confusionMatrix(confusion_matrix[1:2,]))
   # Calcular n
   try(n <- sum(confusion_matrix[rownames(confusion_matrix) != "Z_G", ]))
   
   # Guardar los resultados en el data.frame, incluyendo delta
   try(results <- rbind(results, data.frame(n_rounds = n_rounds, 
                                        max_depth = max_depth, 
                                        delta = delta,
                                        eta = eta,
                                        accuracy = conf$overall[1], 
                                        n = n))  )
 }
 data_test[900,]
library(beepr)
beep()
beep(2) 
library(scales)
results[7:8] <- apply(results[, 5:6], 2, rescale)
max(results[,7]*results[,8])
results[,7]*results[,8]
results[19,]
results[71,]
#            n_rounds    max_depth delta   eta  accuracy   n        V7        V8
#Accuracy32      100         3      0.2    0.1  0.8100962 832 0.7950962 0.4402116



max(results[,6]*results[,7])
#  results2[, 6:7] <- apply(results2[, 4:5], 2, rescale)
# Máximo results2[27,] eta = 0.05
# Con la  muestra de 2024
#n_rounds max_depth delta  accuracy    n        V6        V7
#Accuracy26      100         2  0.15 0.7600989 1213 0.5180039 0.5788449
#results3[, 6:7] <- apply(results3[, 4:5], 2, rescale)
#results3[22,] eta = 0.05
#n_rounds max_depth delta  accuracy   n        V6        V7
#Accuracy21      100         1  0.15 0.8024883 643 0.7255209 0.3402116

# Con la muestra aleatoria 
#n_rounds max_depth delta  eta  accuracy    n        V7        V8
#Accuracy118      500         5  0.15 0.01 0.7550148 8425 0.4392467 0.6228745
## Modelo óptimo
bstSparse <- xgboost(data = as.matrix(data_train[,-c(1,34)]), 
                     label = as.matrix(data_train[,1]), 
                     gamma = 1,  
                     verbose  = TRUE, 
                     max.depth = 2, 
                     eta = 0.005, 
                     nthread = 14, 
                     nrounds = 1000, 
                     objective = "binary:logistic")

# Realizar predicciones
pred <- predict(bstSparse, as.matrix(data_test[,-c(1,34)]))
pred_xgboost <- ifelse(pred > (0.5 - 0.15), ifelse(pred > (0.5 + 0.15), 1, "Z_G"), 0)

pred_xgboost <- as.factor(pred_xgboost)

# Crear la matriz de confusión y calcular la precisión
confusion_matrix <- table(pred_xgboost, data_test$V)
try(conf <- confusionMatrix(confusion_matrix[1:2,]))
# Calcular n
try(n <- sum(confusion_matrix[rownames(confusion_matrix) != "Z_G", ]))



results(pred,data_test,0.15,0)

predictions_v <- ifelse(pred > (0.5-0.15), ifelse(pred > (0.5+0.15), 1, "Z_G"), 0)
data_test2 <- cbind(data_test,predictions_v)
data_test2$result <- ifelse(data_test2$predictions_v== "Z_G" , 0,
                          ifelse(data_test2$predictions_v == 1 & data_test2$V == 1, (data_test2$odds - 1), 
                                 ifelse(data_test2$predictions_v == 0 & data_test2$V == 0, (data_test2$odds.opponent - 1), -1)))
#data_test$result <- ifelse(data_test$odds < (1+0)| data_test$odds.opponent < (1+0), 0, data_test$result)
#View(testData[,c("Team","Opponent","Points","OpponentPoints","Odds.x","odds.opponent","predictions","result")])
results <- data_test2[ , .(result = result[1]), by = game_id]
cumulative_sum <- na.omit(cumsum(results$result))
plot(cumulative_sum)
nrow(results[results$result!=0,])
pred_xgboost <- ifelse(pred > (0.5 - 0.15), ifelse(pred > (0.5 + 0.15), 1, "Z_G"), 0)
pred_xgboost <- as.factor(pred_xgboost)

# Crear la matriz de confusión y calcular la precisión
confusion_matrix <- table(pred_xgboost, data_test$V)
confusion_matrix
confusionMatrix(confusion_matrix[1:2,])



bstSparse <- xgboost(data = as.matrix(data_train[,-1]), 
                     label = as.matrix(data_train[,1]), 
                     gamma = 1,  
                     verbose  = FALSE, 
                     max.depth = 3, 
                     eta = 0.1, 
                     nthread = 14, 
                     nrounds = 100, 
                     objective = "binary:logistic")

# Realizar predicciones
pred <- predict(bstSparse, as.matrix(data_test[,-1]))
pred_xgboost <- ifelse(pred > (0.5 - 0.2), ifelse(pred > (0.5 + 0.2), 1, "Z_G"), 0)
pred_xgboost <- pred_xgboost


# Experimento para 2023

# Crear la matriz de confusión y calcular la precisión
confusion_matrix <- table(pred_xgboost, data_test$V)
confusion_matrix
confusionMatrix(confusion_matrix[1:2,])


# Xgboost with trees
bstSparse <- xgboost(data = as.matrix(data_train[,-1]), 
                     label = as.matrix(data_train[,1]), 
                     gamma = 1,  
                     verbose  = FALSE, 
                     max.depth = 3, 
                     eta = 0.1, 
                     nthread = 14, 
                     nrounds = 100, 
                     objective = "binary:logistic")
bstSparse$feature_names
importance(bstSparse)
importance_matrix = xgb.importance(bstSparse$feature_names, model = bstSparse)
importance_matrix
xgb.plot.importance(importance_matrix[1:5,])



# Random forest 
temp.model <- randomForest(V~ ., data = data_train, importance=TRUE, 
                           ntree = 500, mtry = 5 , verbose = 1)
preds_rf <- predict(temp.model, data_test) 

preds_rf <- ifelse(preds_rf > 0.3 , ifelse(preds_rf > 0.7, 1, "Z_G"), 0)


(conf_matrix_bayes <- table(preds_rf, data_test$V))   

confusionMatrix(conf_matrix_bayes[1:2,]) 
sum(conf_matrix_bayes[1:2,])

