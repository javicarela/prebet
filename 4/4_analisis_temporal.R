#Area verde
# El area everde de la predicción son aquellos valores en los que las casas de apuestas
# valoran entre 1.6 y 2.4 de cuota. ¿Porque ? porque son los que peores se predicen
load("~prebet/data_frames/NBA_games_info.RData")
library(data.table)
library(knitr)
library(caret)
library(dplyr)
setDT(df)

df <- df %>%
  arrange(Date)
df <- df[-17364,]
df_bayes <- df[,c("V","hour","weekday","month","Opponent_Zone","temporada","local",
                  "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
                  "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
                  "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
                  "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
                  "avg_P_o_32" ,"days_since_last","days_since_last_o","games_played",
                  "odds","odds.opponent" , "avg_diff_P_1","avg_diff_P_4","avg_diff_P_8" ,
                  "avg_diff_P_12","avg_diff_P_16","avg_diff_P_32", "avg_diff_P_o_1","avg_diff_P_o_4","avg_diff_P_o_8",
                 "avg_diff_P_o_12","avg_diff_P_o_16","avg_diff_P_o_32", "diff_victorias","score_last_10_between", "score" ,
                 "score_last_5_between","game_id" )]
df_bayes <- na.omit(df_bayes)
df_bayes <- na.omit(df_bayes)
# Como puedo obtener mejores cuotas apostando en diferentes sitios,
#obtengo un 0.06 - 0.07 más en cada cuota de media 
summary(df$odds)
df_bayes$odds <- df_bayes$odds + 0.02* exp(5*df_bayes$odds / max(df_bayes$odds))
df_bayes$odds.opponent <- df_bayes$odds.opponent + 0.02* exp(5*df_bayes$odds.opponent / max(df_bayes$odds.opponent))

summary(df_bayes$odds.opponent)
factor_columns <- sapply(df_bayes, is.factor)
factor_columns_names <- names(df_bayes)[factor_columns]

# Convertir las columnas de tipo factor a numérico
for (col in factor_columns_names) {
  df_bayes[[col]] <- as.numeric(as.factor(df_bayes[[col]]))
}




# Análisis de rendimiento Logit en las diferentes zonas.
temporadas <- c(2021,2022,2023,2024)

ec_optima <- V ~ odds + odds.opponent + hour + weekday +  diff_victorias   + 
  Opponent_Zone + temporada +  local + avg_V_1 + avg_V_4 + avg_V_8 + avg_V_12  + avg_V_32 + avg_P_1 + avg_P_4 +  avg_V_o_4 + avg_V_o_8  + avg_V_o_12  + 
  avg_V_o_32  + days_since_last + days_since_last_o + odds * local + score + score_last_10_between + 
  avg_diff_P_1 + avg_diff_P_4 +  avg_diff_P_o_1 + avg_diff_P_o_4  +score_last_5_between


rendimiento_en_temporadas_logit <- function(temporadas, df, ec_optima){
  par(mfrow = c(2, 2))  # Crear una ventana de gráficos con 2x2
  
  for (i in 1:length(temporadas)) {
    # Filtrar los datos de entrenamiento y prueba
    trainData <- df_bayes[df_bayes$temporada < temporadas[i], ]
    testData  <- df_bayes[df_bayes$temporada >= temporadas[i], ]
    testData  <- testData[odds<=1.5 | odds.opponent<=1.5, ]
    # Ajustar el modelo de regresión logística
    model_team <- glm(ec_optima, data = trainData, family = binomial(link = "logit"))
    
    # Realizar predicciones en el conjunto de prueba
    predictions <- predict(model_team, newdata = testData, type = "response")
    predictions <- ifelse(predictions > 0.5, 1, 0)
    
    # Calcular el resultado para cada juego
    testData <- cbind(testData, predictions)
    testData$result <- ifelse(testData$predictions== "Z_G" , 0,
                              ifelse(testData$predictions == 1 & testData$V == 1, (testData$odds - 1), 
                                     ifelse(testData$predictions == 0 & testData$V == 0, (testData$odds.opponent - 1), -1)))
    # Calcular el rendimiento acumulativo y eliminar valores NA
    results <- testData[, .(result = result[1]), by = game_id]
    cumulative_sum <- na.omit(cumsum(results$result))
    
    # Graficar el rendimiento acumulativo para cada temporada
    plot(cumulative_sum, type = "l", col = "blue", lwd = 1, xlab = " ", 
         ylab = " ", main = paste(temporadas[i]))
    grid()
    abline(h = 0, col = "red", lwd = 2)
    final_pl <- tail(cumulative_sum, 1)  # Último valor de P/L
    legend("bottomright", legend = paste("P/L final:", round(final_pl, 2)), 
           col = "blue", lwd = 2, bty = "n", cex = 0.8)
  }
  par(mfrow = c(1, 1))  # Restablecer el layout de la ventana de gráficos
}

rendimiento_en_temporadas_logit(temporadas = temporadas,df, ec_optima = ec_optima)

# Modelo tipo árbol ( ctree )
temporadas <- c(2021,2022,2023,2024)

ec_optima <- V ~ odds + odds.opponent + hour + weekday +  diff_victorias   + 
  Opponent_Zone + temporada +  local + avg_V_1 + avg_V_4 + avg_V_8 + avg_V_12  + avg_V_32 + avg_P_1 + avg_P_4 +  avg_V_o_4 + avg_V_o_8  + avg_V_o_12  + 
  avg_V_o_32  + days_since_last + days_since_last_o + odds * local + score + score_last_10_between + 
  avg_diff_P_1 + avg_diff_P_4 +  avg_diff_P_o_1 + avg_diff_P_o_4  +score_last_5_between


# =============================================================================

# Modelo ctree

library(partykit)
control <- ctree_control(
  minsplit = 100,    # Número mínimo de observaciones en un nodo para intentar una partición
  minbucket = 25,   # Número mínimo de observaciones en cualquier nodo terminal
  maxdepth = 3,     # Profundidad máxima del árbol
  mincriterion = 0.95  # Valor mínimo del criterio de información (mayor valor es más restrictivo)
)

rendimiento_en_temporadas_ctree <- function(temporadas, df, ec_optima, control, delta,theta){
  par(mfrow = c(2, 2))  # Crear una ventana de gráficos con 2x2
  
  for (i in 1:length(temporadas)) {
    # Filtrar los datos de entrenamiento y prueba
    trainData <- df_bayes[df_bayes$temporada < temporadas[i], ]
    testData  <- df_bayes[df_bayes$temporada >= temporadas[i], ]
    maintenance_tree <- ctree(formula = ec_optima  , data = trainData, control =  control)
    pred_tree <- predict(maintenance_tree, newdata = testData, type="response")
    predictions_v <- ifelse(pred_tree > (0.5-delta), ifelse(pred_tree > (0.5+delta), 1, "Z_G"), 0)
    table(as.factor(predictions_v), as.factor(testData$V))
    predictions_v <- as.vector(predictions_v)
    predictions_v <- data.frame(predictions_v = predictions_v)
    testData <- cbind(testData,predictions_v)
    table(testData$V == testData$predictions_v)
    testData$result <- ifelse(testData$predictions_v== "Z_G" , 0,
                              ifelse(testData$predictions_v == 1 & testData$V == 1, (testData$odds - 1), 
                                     ifelse(testData$predictions_v == 0 & testData$V == 0, (testData$odds.opponent - 1), -1)))
    
    
    testData$result <- ifelse(testData$odds < (1+theta)| testData$odds.opponent < (1+ theta), 0, testData$result)
    #View(testData[,c("Team","Opponent","Points","OpponentPoints","Odds.x","odds.opponent","predictions","result")])
    results <- testData[ , .(result = result[1]), by = game_id]
    cumulative_sum <- na.omit(cumsum(results$result))
    
    # Graficar el rendimiento acumulativo para cada temporada
    plot(cumulative_sum, type = "l", col = "blue", lwd = 1, xlab = " ", 
         ylab = " ", main = paste(temporadas[i]))
    grid()
    abline(h = 0, col = "red", lwd = 2)
    final_pl <- tail(cumulative_sum, 1)  # Último valor de P/L
    legend("bottomright", legend = paste("P/L final:", round(final_pl, 2)), 
           col = "blue", lwd = 2, bty = "n", cex = 0.8)
  }
  par(mfrow = c(1, 1))  # Restablecer el layout de la ventana de gráficos
}

# Ajustar

rendimiento_en_temporadas_ctree(temporadas,df = df ,ec_optima = ec_optima ,
                                control = control, delta = 0.15, theta = 0.4)

# =============================================================================
# Modelo de redes bayesianas 
library(e1071)

ec_optima <- V ~ odds + odds.opponent + hour + weekday +  diff_victorias   + 
  Opponent_Zone + temporada +  local + avg_V_1 + avg_V_4 + avg_V_8 + avg_V_12  + avg_V_32 + avg_P_1 + avg_P_4 +  avg_V_o_4 + avg_V_o_8  + avg_V_o_12  + 
  avg_V_o_32  + days_since_last + days_since_last_o + score + score_last_10_between + 
  avg_diff_P_1 + avg_diff_P_4 +  avg_diff_P_o_1 + avg_diff_P_o_4  +score_last_5_between


rendimiento_en_temporadas_bayes <- function(temporadas, df, ec_optima, control, delta,theta){
  par(mfrow = c(2, 2))  # Crear una ventana de gráficos con 2x2
  
  for (i in 1:length(temporadas)) {
    # Filtrar los datos de entrenamiento y prueba
    trainData <- df[df$temporada < temporadas[i], ]
    testData  <- df[df$temporada >= temporadas[i], ]
    topredict_set <- testData[,c(2:49)] 
    model_bayes <- naiveBayes(formula = ec_optima,
                              data = data_train,
                              laplace = 3,   
                              usekernel = TRUE)
    preds_bayes <- predict(model_bayes, newdata = topredict_set, type = "raw")
    predictions_v <- ifelse(preds_bayes[,2] > (0.5-delta), ifelse(preds_bayes[,2] > (0.5+delta), 1, "Z_G"), 0)
    predictions_v <- as.vector(predictions_v)
    predictions_v <- data.frame(predictions_v = predictions_v)
    testData <- cbind(testData,predictions_v)
    testData$result <- ifelse(testData$predictions_v== "Z_G" , 0,
                              ifelse(testData$predictions_v == 1 & testData$V == 1, (testData$odds - 1), 
                                     ifelse(testData$predictions_v == 0 & testData$V == 0, (testData$odds.opponent - 1), -1)))
    
    
    testData$result <- ifelse(testData$odds < (1+theta)| testData$odds.opponent < (1+ theta), 0, testData$result)
    #View(testData[,c("Team","Opponent","Points","OpponentPoints","Odds.x","odds.opponent","predictions","result")])
    results <- testData[ , .(result = result[1]), by = game_id]
    cumulative_sum <- na.omit(cumsum(results$result))
    
    # Graficar el rendimiento acumulativo para cada temporada
    plot(cumulative_sum, type = "l", col = "blue", lwd = 1, xlab = " ", 
         ylab = " ", main = paste(temporadas[i]))
    grid()
    abline(h = 0, col = "red", lwd = 2)
    final_pl <- tail(cumulative_sum, 1)  # Último valor de P/L
    legend("bottomright", legend = paste("P/L final:", round(final_pl, 2)), 
           col = "blue", lwd = 2, bty = "n", cex = 0.8)
  }
  par(mfrow = c(1, 1))  # Restablecer el layout de la ventana de gráficos
}
rendimiento_en_temporadas_bayes(temporadas,df = df_bayes ,ec_optima = ec_optima ,
                                control = control, delta = 0.49, theta = 0.2)
# Temporada 2024 bien, las demás regularmente mal.


# =============================================================================
 # Xgboost 
library(xgboost)
library(caret)
rendimiento_en_temporadas_xgboost <- function(temporadas, df, delta,theta){
  par(mfrow = c(2, 2))  # Crear una ventana de gráficos con 2x2
  
  for (i in 1:length(temporadas)) {
    # Filtrar los datos de entrenamiento y prueba
    trainData <- df[df$temporada < temporadas[i], ]
    testData  <- df[df$temporada == temporadas[i], ]
    data_train_numeric <- model.matrix(~ . -1, data = trainData[,-c(1,50)])  # Eliminamos las columnas 1 y 35
    data_test_numeric <- model.matrix(~ . -1, data = trainData[,-c(1, 50)])  # Eliminamos las columnas 1 y 35
    label = as.matrix(trainData[,1])
    bstSparse <- xgboost(data = data_train_numeric, 
                         label = label, 
                         gamma = 1,  
                         verbose  = T, 
                         print_every_n = 500,
                         max.depth = 5, 
                         eta = 0.001, 
                         nthread = 14, 
                         nrounds = 1500, 
                         objective = "binary:logistic")
    pred <- predict(bstSparse, data_test_numeric)
    predictions_v <- ifelse(pred > (0.5-delta), ifelse(pred > (0.5+delta), 1, "Z_G"), 0)
    predictions_v <- as.vector(predictions_v)
    predictions_v <- data.frame(predictions_v = predictions_v)
    testData <- cbind(testData,predictions_v)
    testData$result <- ifelse(testData$predictions_v== "Z_G" , 0,
                              ifelse(testData$predictions_v == 1 & testData$V == 1, (testData$odds - 1), 
                                     ifelse(testData$predictions_v == 0 & testData$V == 0, (testData$odds.opponent - 1), -1)))
    
    
    testData$result <- ifelse(testData$odds < (1+theta)| testData$odds.opponent < (1+ theta), 0, testData$result)
    #View(testData[,c("Team","Opponent","Points","OpponentPoints","Odds.x","odds.opponent","predictions","result")])
    results <- testData[ , .(result = result[1]), by = game_id]
    cumulative_sum <- na.omit(cumsum(results$result))
    
    # Graficar el rendimiento acumulativo para cada temporada
    plot(cumulative_sum, type = "l", col = "blue", lwd = 1, xlab = " ", 
         ylab = " ", main = paste(temporadas[i]))
    grid()
    abline(h = 0, col = "red", lwd = 2)
    final_pl <- tail(cumulative_sum, 1)  # Último valor de P/L
    legend("bottomright", legend = paste("P/L final:", round(final_pl, 2)), 
           col = "blue", lwd = 2, bty = "n", cex = 0.8)
  }
  par(mfrow = c(1, 1))  # Restablecer el layout de la ventana de gráficos
}
rendimiento_en_temporadas_xgboost(temporadas, df_bayes , 0.3 , 0.5 )
  
# Rentable para delta= theta = 0.2





## Random forest para análizar importancia de variables


# =============================================================================
# Xgboost 
library(randomForest)
library(caret)
rendimiento_en_temporadas_random_forest <- function(temporadas, df, delta,theta){
  par(mfrow = c(2, 2))  # Crear una ventana de gráficos con 2x2
  
  for (i in 1:length(temporadas)) {
    # Filtrar los datos de entrenamiento y prueba
    trainData <- df[df$temporada < temporadas[i], ]
    testData  <- df[df$temporada == temporadas[i], ]
    temp.model <- randomForest(ec_optima, data = trainData, importance=TRUE, 
                               ntree = 500, mtry = 5 , verbose = 1)
    preds_rf <- predict(temp.model, testData) 
    predictions_v <- ifelse(preds_rf > (0.5-delta), ifelse(preds_rf > (0.5+delta), 1, "Z_G"), 0)
    predictions_v <- as.vector(predictions_v)
    predictions_v <- data.frame(predictions_v = predictions_v)
    testData <- cbind(testData,predictions_v)
    testData$result <- ifelse(testData$predictions_v== "Z_G" , 0,
                              ifelse(testData$predictions_v == 1 & testData$V == 1, (testData$odds - 1), 
                                     ifelse(testData$predictions_v == 0 & testData$V == 0, (testData$odds.opponent - 1), -1)))
    
    
    testData$result <- ifelse(testData$odds < (1+theta)| testData$odds.opponent < (1+ theta), 0, testData$result)
    #View(testData[,c("Team","Opponent","Points","OpponentPoints","Odds.x","odds.opponent","predictions","result")])
    results <- testData[ , .(result = result[1]), by = game_id]
    cumulative_sum <- na.omit(cumsum(results$result))
    
    # Graficar el rendimiento acumulativo para cada temporada
    plot(cumulative_sum, type = "l", col = "blue", lwd = 1, xlab = " ", 
         ylab = " ", main = paste(temporadas[i]))
    grid()
    abline(h = 0, col = "red", lwd = 2)
    final_pl <- tail(cumulative_sum, 1)  # Último valor de P/L
    legend("bottomright", legend = paste("P/L final:", round(final_pl, 2)), 
           col = "blue", lwd = 2, bty = "n", cex = 0.8)
  }
  par(mfrow = c(1, 1))  # Restablecer el layout de la ventana de gráficos
}

rendimiento_en_temporadas_random_forest(temporadas, df_bayes , 0.15 , 0.1 )
