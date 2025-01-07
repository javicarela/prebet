# Análisis predicciones  + cuotas
load("~/prebet/df.RData")
library(data.table)
library(knitr)
trainData <- df[temporada<2024, ]
testData  <- df[temporada>=2024, ]
ec_optima <- V ~ odds + odds.opponent + hour + weekday    + Opponent_Zone + temporada +  local + avg_V_1 + 
  avg_V_4 + avg_V_8 + avg_V_12 + avg_V_16 + avg_V_32 + avg_P_1 + avg_P_4    +  
  avg_V_o_4 + avg_V_o_8 + avg_V_o_12  + avg_V_o_32 + n_victorias +
  n_victorias_o + days_since_last + days_since_last_o

model_team <- glm( ec_optima,
                   data = trainData, 
                   family = binomial(link = "logit"))
predictions <- predict(model_team, newdata = testData, type = "response")





results <- function (predictions,testData, delta, theta){
  predictions_v <- ifelse(predictions > (0.5-delta), ifelse(predictions > (0.5+delta), 1, "Z_G"), 0)
  testData <- cbind(testData,predictions_v)
  testData$result <- ifelse(testData$predictions_v== "Z_G" , 0,
                            ifelse(testData$predictions_v == 1 & testData$V == 1, (testData$odds - 1), 
                                   ifelse(testData$predictions_v == 0 & testData$V == 0, (testData$odds.opponent - 1), -1)))
  testData$result <- ifelse(testData$odds < (1+theta)| testData$odds.opponent < (1+theta), 0, testData$result)
  #View(testData[,c("Team","Opponent","Points","OpponentPoints","Odds.x","odds.opponent","predictions","result")])
  results <- testData[ , .(result = result[1]), by = game_id]
  cumulative_sum <- na.omit(cumsum(results$result))
  plot(cumulative_sum, type = "l", col = "blue",lwd = 1 ,  xlab = "Tiempo", 
       ylab = "P/L", main =paste("Evolution betting with delta", delta, sep=" "))
  grid()
  abline(h = 0, col = "red", lwd = 2)  # lwd ajusta el grosor de la línea
  final_pl <- tail(cumulative_sum, 1)  # Último valor de P/L
  legend("bottomright", legend = paste("P/L final:", round(final_pl, 2)), 
         col = "blue", lwd = 2, bty = "n", cex = 0.8)
  cat("\n Se han realizado " , nrow(results[results$result!=0,]),"predicciones sobre", (nrow(testData))/2)
  delta = NULL
  theta = NULL
  plot_capture <- recordPlot()
  return(plot_capture)
}


results(predictions = predictions, testData = testData,delta = 0.1, theta = 0.5)
 # No rentable
#====================================================================================
# Prueba con bayesian model
load("~/prebet/df.RData")
df <- df %>%
  arrange(Date, game_id)

df_bayes <- df[,c("V","hour","Team","weekday","month","Opponent_Zone","temporada","local",
            "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
            "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
            "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
            "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
            "avg_P_o_32" ,"days_since_last","days_since_last_o","games_played",
            "odds.opponent", "odds","game_id")]

df_bayes <- df_bayes[!rowSums(is.na(df_bayes[, 1:(ncol(df_bayes)-9)])) > 0, ]
factor_columns <- sapply(df_bayes, is.factor)
factor_columns_names <- names(df_bayes)[factor_columns]
factor_columns_names <- factor_columns_names[1:3]
# Convertir las columnas de tipo factor a numérico
for (col in factor_columns_names) {
  df_bayes[[col]] <- as.numeric(as.factor(df_bayes[[col]]))
}
table(df_bayes$temporada)
data_train <- df_bayes[!temporada==2024, ]
data_test <- df_bayes[temporada==2024, ] 
ecuacion  <- V ~ . - game_id 
topredict_set<-data_test[,c(1:32)] 

library(e1071)
model_bayes <- naiveBayes(formula = ecuacion,
                          data = data_train )
preds_bayes <- predict(model_bayes, newdata = topredict_set, type = "raw")
results(preds_bayes[,2],data_test,0.47 , 0.2)
# Posible rentabilidad para valores extremos, apartir de 0,03 para derrotas  y 0,97 para victorias
# ====================================================================
### ctree
library(partykit)
control <- ctree_control(
  minsplit = 10,      # Reduce el mínimo de observaciones para permitir más divisiones
  minbucket = 5,      # Reduce el mínimo de observaciones en nodos terminales
  maxdepth = 7,       # Aumenta la profundidad máxima para captar más complejidades
  mincriterion = 0.001   # Ajusta a un criterio más bajo para permitir más divisiones
)
maintenance_tree <- ctree(formula = ec_optima, data = data_train, control =  control)

pred_tree <- predict(maintenance_tree, newdata = data_test, type="response")
pred_tree <- ifelse(pred_tree > (0.5-0.1), ifelse(pred_tree > (0.5+0.1), 1, "Z_G"), 0)
testData <- cbind(testData,pred_tree)
testData$result <- ifelse(testData$pred_tree== "Z_G" , 0,
                          ifelse(testData$pred_tree == 1 & testData$V == 1, (testData$odds - 1), 
                                 ifelse(testData$pred_tree == 0 & testData$V == 0, (testData$odds.opponent - 1), -1)))
testData$result <- ifelse(testData$odds < (1 + 0.1)| testData$odds.opponent < (1 +  0.1), 0, testData$result)
#View(testData[,c("Team","Opponent","Points","OpponentPoints","Odds.x","odds.opponent","predictions","result")])
results <- testData[ , .(result = result[1]), by = game_id]
cumulative_sum <- na.omit(cumsum(results$result))

plot(cumulative_sum, type = "l")
results(pred_tree,data_test,0.3,0.3)

# Posible rentabilidad para valores extremos, apartir de 0,3 para derrotas  y 0,7 para victorias

# ==============================================
# Xgboost
library(xgboost)
library(caret)
str(data_train)
colnames(data_train)
str(data_train[,-c(1,35)])
data_train_numeric <- model.matrix(~ . -1, data = data_train[,-c(1,33)])  # Eliminamos las columnas 1 y 35
data_test_numeric <- model.matrix(~ . -1, data = data_test[,-c(1, 35)])  # Eliminamos las columnas 1 y 35

bstSparse <- xgboost(data = data_train_numeric, 
                     label = label, 
                     gamma = 1,  
                     verbose  = T, 
                     max.depth = 2, 
                     eta = 0.005, 
                     nthread = 14, 
                     nrounds = 1000, 
                     objective = "binary:logistic")
pred <- predict(bstSparse, data_test_numeric)
pred <- ifelse(pred>0.5,1,0)
confusionMatrix(as.factor(pred),as.factor(data_test$V))
results(pred,data_test,0.1,0)
# Rentable para delta= theta = 0.2
# ====================================================================================
temp.model <- randomForest(V~ ., data = data_train[,-c(31:39)], importance=TRUE, 
                           ntree = 500, mtry = 5 , verbose = 1)
preds_rf <- predict(temp.model, data_test[,-c(31:39)]) 
preds_rf <- ifelse(preds_rf>0.5,1,0)
confusionMatrix(as.factor(preds_rf),as.factor(data_test$V))
results(preds_rf , data_test , 0.2, 0.1)






