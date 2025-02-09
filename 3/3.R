# Algorithms script

load("~prebet/data_frames/NBA_games_info.RData")
library(sjPlot)
library(caret)
library(data.table)
df$V <- as.factor(df$V)
df$Team <- as.factor(df$Team)
setDT(df)
ec_optima <- V ~ odds + odds.opponent + hour + weekday    + Opponent_Zone + temporada +  local + avg_V_1 + 
 avg_V_4 + avg_V_8 + avg_V_12 + avg_V_16 + avg_V_32 + avg_P_1 + avg_P_4    +  
  avg_V_o_4 + avg_V_o_8 + avg_V_o_12  + avg_V_o_32 + n_victorias +
  n_victorias_o + days_since_last + days_since_last_o
#ecuacion <- V ~ hour + weekday    +  month + Opponent_Zone + temporada +  local + avg_V_1 + 
#     avg_V_4 + avg_V_8 + avg_V_12 + avg_V_16 + avg_V_32 + avg_P_1 + avg_P_4    +
 # avg_P_8 + avg_P_12 + avg_P_16 + avg_P_32 + avg_V_o_1 +  
 #    avg_V_o_4 + avg_V_o_8 + avg_V_o_12  + avg_V_o_32 + n_victorias +
 #    n_victorias_o + n_derrotas + n_derrotas_o + avg_P_o_1 + avg_P_o_4 + 
 #    avg_P_o_8 + avg_P_o_12 + avg_P_o_16 + avg_P_o_32    
# Function to check the accuracy of any model in the script
check_modelo <- function(df){
  accuracies_d <- c()
  accuracies_v <- c()
  accuracies <- c()
  for (i in 1:length(levels(df$Team))){
    df_Team <- df[Team == levels(df$Team)[i] ,]
    set.seed(123)
    trainIndex <- createDataPartition(df_Team$V, p = 0.7, 
                                      list = FALSE, 
                                     times = 1)
    trainData <- df_Team[trainIndex, ]
    testData  <- df_Team[-trainIndex, ]
    #trainData <- df_Team[!temporada==2024, ]
    #testData  <- df_Team[temporada==2024, ]
    model_team <- glm( ec_optima,
                        data = trainData, 
                       family = binomial(link = "logit"))
    try(predictions <- predict(model_team, newdata = testData, type = "response"))
    try(predicted_classes <- ifelse(predictions > 0.5, 1, 0))
    try(conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(testData$V)))
    try(acc_derrotas <- conf_matrix$byClass[3])
    try(acc_victorias <- conf_matrix$byClass[4])
    try(valor_prediccion <- conf_matrix$overall[1])
    cat(levels(df$Team)[i], "|Acc derrotas ", "del ", round(acc_derrotas*100,2), "%| \n")
    cat("                    |Acc victorias " , "del ", round(acc_victorias*100,2), "%| \n")
    cat("                    |Acc media ", "del ", round(valor_prediccion*100,2), "%| \n")  
    try(accuracies_d <-c(accuracies_d, acc_derrotas))
    try(accuracies_v <-c(accuracies_v, acc_victorias))
     try(accuracies <-c(accuracies, valor_prediccion))
  }
  cat("===================================== /n 
     |   La predicción media de las derrotas es " , mean(accuracies_d)*100 , "%    " )
  cat("===================================== /n 
     |   La predicción media de las victorias es " , mean(accuracies_v)*100 , "%    " )
  cat("===================================== /n 
     |   La predicción media del modelo es" , mean(accuracies)*100 , "%    " )
}
colSums(is.na(df))
try(check_modelo(df))

library("e1071")
        library(caret)
trainData <- df[!temporada==2024, ]
testData  <- df[temporada==2024, ]
svm_model1 <- svm(ec_optima,kernel = "radial", data=trainData)
summary(svm_model1)
pred1 <- predict(svm_model1, newdata = testData)
pred1 <- as.factor(pred1)
testData$V <- as.factor(testData$V)
confusionMatrix(pred1, testData$V )
# X gradient 
library(xgboost)
model <- xgboost(data = as.matrix(train), label = train$V, max_depth = 6, eta = 0.1, nrounds = 100, objective = "binary:logistic")

# Revisar h2o
df <- df[,c("V","hour","weekday","month","Opponent_Zone","temporada","local",
                  "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
                  "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
                  "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
                  "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
                  "avg_P_o_32" ,"days_since_last","days_since_last_o","games_played",
            "odds.opponent", "odds")]
trainData <- df[!temporada==2024, ]
testData  <- df[temporada==2024, ]
library(h2o)
h2o.init()
# Check this out
aml <- h2o.automl(y = "V", training_frame = as.h2o(trainData), max_models = 10, seed = 1,
                  max_runtime_secs = 60)
best_model <- aml@leader

testData_h2o <- as.h2o(testData)
perf <- h2o.performance(best_model, newdata = testData_h2o)
print(perf)





df_Team <- df[Team == levels(df$Team)[1] ,]
df_Team <- df
set.seed(123)
trainIndex <- createDataPartition(df_Team$V, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
trainData <- df_Team[trainIndex, ]
testData  <- df_Team[-trainIndex, ]
#trainData <- df_Team[!temporada==2024, ]
#testData  <- df_Team[temporada==2024, ]
testData$month <- factor(testData$month, levels = levels(trainData$month))
model_team <- glm( ec_optima,
                   data = trainData, 
                   family = binomial(link = "logit"))
vif(model_team)
try(predictions <- predict(model_team, newdata = testData, type = "response"))
try(predicted_classes <- ifelse(predictions > 0.5, 1, 0))
try(conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(testData$V)))
try(acc_derrotas <- conf_matrix$byClass[1])
try(acc_victorias <- conf_matrix$byClass[2])
try(valor_prediccion <- conf_matrix$overall[1])
cat(levels(df$Team)[1], "|Acc derrotas ", "del ", round(acc_derrotas*100,2), "%| \n")
cat("            |Acc victorias " , "del ", round(acc_victorias*100,2), "%| \n")
cat("            |Acc media ", "del ", round(valor_prediccion*100,2), "%| \n")
try(accuracies <-c(accuracies, valor_prediccion))

#  La predicción media del modelo es 64.77747 %  
df_Team <- df[Team == "New York Knicks" ,]
tail(df_Team,1)
model_team <- glm( ecuacion,
                   data = df_Team, 
                   family = binomial(link = "logit"))

summary(model_team)
tail(df_Team,1)
partido <- data.table(
  Date = as.POSIXct("2025-10-22 19:00:00"),
  Team = factor("New York Knicks"),
  Points = NA,
  Opponent = factor("Boston Celtics"),
  OpponentPoints = NA,
  Attend = NA,
  LOG = NA,
  temporada = 2025,
  hour = 19,
  weekday = factor("ma."),
  month = factor("oct."),
  local = 0,
  Team_Zone = factor("Este_Atlántico"),
  Opponent_Zone = factor("Este_Atlántico"),
  V = NA,
  V_o = NA,
  E = NA,
  avg_V_1 = 1,
  avg_V_4 = 0.5,
  avg_V_8 = 0.75,
  avg_V_12 = 0.5833333,
  avg_V_16 = 0.625,
  avg_V_32 = 0.59375,
  avg_P_1 = 109,
  avg_P_4 = 108.25,
  avg_P_8 = 113.75,
  avg_P_12 = 114.6667,
  avg_P_16 = 115.6875,
  avg_P_32 = 116.4688,
  avg_V_o_1 = 1,
  avg_V_o_4 = 0.5,
  avg_V_o_8 = 0.75,
  avg_V_o_12 = 0.6666667,
  avg_V_o_16 = 0.75,
  avg_V_o_32 = 0.8125,
  year = 2025,
  n_victorias = 24  ,
  n_victorias_o = 63
)

df_Team2 <- df[Team == "Boston Celtics" ,]
model_team2 <- glm( ecuacion,
                   data = df_Team, 
                   family = binomial(link = "logit"))
tail(df_Team2,1)
partido2 <- data.table(
  Date = as.POSIXct("2025-10-22 19:00:00"),
  Team = factor("Boston Celtics"),
  Points = NA,
  Opponent = factor("New York Knicks"),
  OpponentPoints = NA,
  Attend = NA,
  LOG = NA,
  temporada = 2025,
  hour = 19,
  weekday = factor("ma."),
  month = factor("oct."),
  local = 1,
  Team_Zone = factor("Este_Atlántico"),
  Opponent_Zone = factor("Este_Atlántico"),
  V = NA,
  V_o = NA,
  E = NA,
  avg_V_1 = 1,
  avg_V_4 = 0.5,
  avg_V_8 = 0.75,
  avg_V_12 = 0.6666667,
  avg_V_16 = 0.75,
  avg_V_32 = 0.8125,
  avg_P_1 = 131,
  avg_P_4 = 113.75,
  avg_P_8 = 114.125,
  avg_P_12 = 117.1667,
  avg_P_16 = 119,
  avg_P_32 = 120.9688,
  avg_V_o_1 = 1,
  avg_V_o_4 = 0.5,
  avg_V_o_8 = 0.75,
  avg_V_o_12 = 0.5833333,
  avg_V_o_16 = 0.625,
  avg_V_o_32 = 0.59375,
  year = 2025, 
  n_victorias = 63,
  n_victorias_o = 24 
)
partidos <- rbind(partido,partido2)

try(predictions <- predict(model_team, newdata = partidos[1], type = "response"))
try(predictions2 <- predict(model_team2, newdata = partidos[2], type = "response"))

try(predicted_classes <- ifelse(predictions > 0.5, 1, 0))

predictions2 / (1-predictions2) + 1 
predictions /(1-predictions ) + 1 

try(conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(testData$V)))
try(valor_prediccion <- conf_matrix$overall[1])
cat("|Precisión para ", levels(df$Team)[i], "del ", round(valor_prediccion*100,2), "%| \n")
try(accuracies <-c(accuracies, valor_prediccion))




library(caret)
set.seed(123)
df$V <- as.factor(df$V)
df_Team <- df[Team == levels(df$Team)[1] , c("V","hour","month","local","Opponent_Zone","avg_V_1")]
df_Team <- na.omit(df_Team)
df_Team$hour <- as.factor(df_Team$hour)
df_Team$local <- as.factor(df_Team$local)

ecuacion <-  V ~ hour + month + local + Opponent_Zone + avg_V_1
trainIndex <- createDataPartition(df_Team$V, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
trainData <- df_Team[trainIndex, ]
testData  <- df_Team[-trainIndex, ]
model <- train(ecuacion, data = trainData, method = "glm", family = "binomial")
predictions <- predict(model, newdata = testData)
conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(testData$V))

#       Balanced Accuracy : 0.5966          

# =============================================0
library(randomForest)
rf_model <- randomForest(ecuacion, data = trainData, ntree = 1000)
predictions <- predict(rf_model, newdata = testData)
conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(testData$V))
conf_matrix
#       Balanced Accuracy : 0.5854          
# =============================================
library(xgboost)
df_Team$V = as.numeric(as.character(df_Team$V))
df_Team$hour = as.numeric(as.character(df_Team$hour))
df_Team$month = as.numeric(df_Team$month)
df_Team$local = as.numeric(df_Team$local)
df_Team$Opponent_Zone = as.numeric(df_Team$Opponent_Zone)

set.seed(123)
trainIndex <- createDataPartition(df_Team$V, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
trainData <- df_Team[trainIndex, ]
testData  <- df_Team[-trainIndex, ]


#prepare training data
trainm = sparse.model.matrix(V ~., data = trainData)
train_label = trainData[,"V"]
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label = train_label)





# Modelo para 2023/2024
# Algoritmos a aplicar a nuestra df
# Análisis a partir de la zona gris.
df_Team <- df[Team == levels(df$Team)[2] ,]
table(df_Team$ranking)
trainData <- df_Team[!temporada==2024, ]
testData  <- df_Team[temporada==2024, ]
#trainData <- df_Team[!temporada==2024, ]
#testData  <- df_Team[temporada==2024, ]
model_team <- glm( ec_optima,
                   data = trainData, 
                   family = binomial(link = "logit"))
summary(model_team)
library(car)
vif(model_team)

predictions <- predict(model_team, newdata = testData, type = "response")
predictions <- ifelse(predictions > 0.35, ifelse(predictions > 0.65, 1, "Z_G"), 0)
tabla <- table(predictions, as.factor(testData$V) )
tabla <- tabla[-3,]
confusionMatrix(tabla)















df <- df[!is.na(df$odds) & !is.na(df$odds.opponent), ]
table(df$temporada)
ec_optima <- V ~ hour + weekday    + Opponent_Zone + temporada +  local + avg_V_1 + 
  avg_V_4 + avg_V_8 + avg_V_12 + avg_V_16 + avg_V_32 + avg_P_1 + avg_P_4    +  
  avg_V_o_4 + avg_V_o_8 + avg_V_o_12  + avg_V_o_32 + n_victorias +
  n_victorias_o + days_since_last + days_since_last_o + odds + odds.opponent
variables <- c( "odds", "odds.opponent", "hour", "Opponent_Zone", "local", 
                "avg_V_1", "avg_V_4", "avg_V_8", "avg_V_12", "avg_V_16",
                "avg_V_32", "avg_P_1", "avg_P_4", "avg_V_o_4", "avg_V_o_8")
"avg_V_o_12", "avg_V_o_32", "n_victorias", "n_victorias_o", 
"days_since_last", "days_since_last_o")

# Crear el grid con todas las combinaciones posibles
grid_combinations <- expand.grid(lapply(variables, function(x) c(TRUE, FALSE)))

# Asignar nombres a las columnas
colnames(grid_combinations) <- variables
df <- na.omit(df)
trainData = df[!temporada == 2024,]
testData = df[temporada == 2024,]

mejor_modelo <- function(data, testData, grid_combinations) {
  mejor_precision <- 0
  mejor_modelo <- NULL
  mejor_comb <- NULL
  
  # Iterar sobre cada combinación de variables en el grid
  for (i in 1:nrow(grid_combinations)) {
    
    # Seleccionar las variables con TRUE en la combinación actual
    variables_seleccionadas <- names(grid_combinations)[grid_combinations[i+1, ] == TRUE]
    
    # Crear la fórmula dinámica para el modelo
    if (length(variables_seleccionadas) == 0) {
      formula_actual <- as.formula("V ~ 1")  # Modelo con solo el intercepto si no hay predictores
    } else {
      formula_actual <- as.formula(paste("V", "~", paste(variables_seleccionadas, collapse = " + ")))
    }
    modelo <- glm(formula_actual, data = data, family = binomial(link = "logit"))
    try(predictions <- predict(modelo, newdata = testData, type = "response"))
    try(predicted_classes <- ifelse(predictions > 0.5, 1, 0))
    precision <- mean(as.factor(predicted_classes) == as.factor(testData$V))* 100  # Convertir a porcentaje
    if (precision > mejor_precision) {
      mejor_precision <- precision
      mejor_modelo <- modelo
      mejor_comb <- variables_seleccionadas
      cat("Nueva mejor combinación:", paste(variables_seleccionadas, collapse = ", "), 
          "| Precisión:", round(precision, 2), "%\n")
    }
  }
  
  # Retornar el mejor modelo, su precisión y la mejor combinación de variables
  list(modelo = mejor_modelo, precision = mejor_precision, combinacion = mejor_comb)
}

resultado <- mejor_modelo(data = trainData,  testData = testData , grid_combinations = grid_combinations)




