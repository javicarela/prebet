library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(caret)
library(rpart)
library(rattle)
library(randomForest)
library(DT)
library(scatterplot3d)
library(ks)

# Load the scatterplot3d package
library(scatterplot3d)
library(glmnet)
library(ISLR)
library(e1071)
library(caret)
library(rpart)

# For data visualization
library(rpart.plot)
# Contains the data
library(ISLR)
library(party)
library(partykit)

library(dplyr) # for data wrangling
library(ggplot2) # to visualize data
library(gridExtra) # to display multiple graph
library(inspectdf) # for EDA
library(tidymodels) # to build tidy models
library(caret) # to pre-process data
library(e1071) # model naive bayes
library(partykit)
#install.packages("inspectdf")


load("~prebet/data_frames/NBA_games_info.RData")
set.seed(100)
library(sjPlot)
library(caret)
library(data.table)
setDT(df)
df_bayes <- df[,c("V","hour","weekday","month","Opponent_Zone","temporada","local",
                  "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
                  "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
                  "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
                  "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
                  "avg_P_o_32" ,"days_since_last","days_since_last_o","games_played","odds","odds.opponent" )]
df_bayes <- na.omit(df_bayes)
df_bayes <- na.omit(df_bayes)
samplesize <- round(0.9 * nrow(df_bayes), 0)
index <- sample(seq_len(nrow(df_bayes)), size = samplesize)
data_train <- df_bayes[!temporada==2024, ]
data_test <- df_bayes[temporada==2024, ]    

topredict_set<-data_test[,c(2:21)] 
model_bayes <- naiveBayes(formula = V ~.,
                          data = data_train)
preds_bayes <- predict(model_bayes, newdata = topredict_set) 
(conf_matrix_bayes <- table(preds_bayes, data_test$V))   

confusionMatrix(data = preds_bayes, reference = as.factor(data_test$V), positive="1") 
#      Accuracy : 0.635        
        
control <- ctree_control(
  minsplit = 100,    # Número mínimo de observaciones en un nodo para intentar una partición
  minbucket = 25,   # Número mínimo de observaciones en cualquier nodo terminal
  maxdepth = 3,     # Profundidad máxima del árbol
  mincriterion = 0.95  # Valor mínimo del criterio de información (mayor valor es más restrictivo)
)
maintenance_tree <- ctree(formula = V~., data = data_train,
                          control =  control)
pred_tree <- predict(maintenance_tree, newdata = data_test, type="response")
pred_tree <- ifelse(pred_tree >0.5,1,0)
conf_matrix_tree <- table(as.factor(pred_tree), as.factor(data_test$V))

plot(maintenance_tree, main = "Árbol de Decisión para Demencia")
varimp(maintenance_tree)
confusionMatrix(data = as.factor(pred_tree), reference = as.factor(data_test$V), positive="1")
Accuracy : 0.6869          


#     Balanced Accuracy :  0.6869 
vif(maintenance_tree)

oob.values <- vector(length = 30)
for(i in 1:28){
  temp.model <- randomForest(V~ ., data = data_train, importance=TRUE, 
                           ntree = 650, mtry = i)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
  print(i)
}
setDT(df_bayes)
predictors <- df_bayes[,c("hour","weekday","month","Opponent_Zone","temporada","local",
      "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
      "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
      "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
      "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
      "avg_P_o_32" ,"days_since_last","days_since_last_o","games_played" )]
variable  <- unlist(df_bayes[,c("V" )])
predictors_test <- predictors[temporada==2024,]
variable_test <- df_bayes[temporada==2024,c("V" )]

variable_test <- as.factor(variable_test$V)


1-temp.model$err.rate[nrow(temp.model$err.rate),1]
predict(temp.model, data_test) 
temp.model
oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
print(i)

plot(1-oob.values[1:28],type = "l")
# 1-oob.values
#[1] 0.6511231 0.6516924 0.6520719 0.6515371 0.6507263 0.6518476 0.6499155 0.6506228
#[9] 0.6487079 0.6474140 0.6488286 0.6497257 0.6472070 0.6483456 0.6486389 0.6478625
#[17] 0.6483456 0.6482593 0.6479833 0.6464479 0.6461891 0.6474140 0.6481558 0.6452231
#[25] 0.6466549 0.6472587 0.6447573 0.6464307 1.0000000 1.0000000



temp.model <- randomForest(V~ ., data = data_train, importance=TRUE, 
                           ntree = 650, mtry = 5 , verbose = 1)
preds_rf <- predict(temp.model, data_test) 

preds_rf <- ifelse(preds_rf > 0.5,1,0)
(conf_matrix_forestII <- table(preds_rf, data_test$V))

confusionMatrix(conf_matrix_forestII, positive="1")
importance(temp.model)
varImpPlot(temp.model)
vif(temp.model)
###################33

temp.model <- randomForest(V~ ., data = data_train, importance=TRUE, 
                           ntree = 650, mtry = 1)
preds_rf <- predict(temp.model, data_test) 
preds_rf <- ifelse(preds_rf>0.5,1,0)
(conf_matrix_forestII <- table(preds_rf, data_test$V))
confusionMatrix(conf_matrix_forestII, positive="1")


# Combinación random forest con bayesian networks

df <- as.data.frame(cbind(preds_rf, preds_bayes))
df$combinacion <- ifelse(preds_bayes == 0, 1, df$preds_rf)
levels(df$combinacion) <- c("0","1")
conf_matrix_forestII <- table(df$combinacion, as.factor(data_test$V))
confusionMatrix(df$combinacion, as.factor(data_test$V))

df$combinacion <- as.factor(df$combinacion)
modFit <- train(V ~ ., method = "rpart", data = data_train)
rattle::fancyRpartPlot(modFit$finalModel)
predictions <- predict(modFit, newdata = data_test)
confusionMatrix(predictions, data_test$V)

#Statistical modelling - Backward regression

ctrl <- trainControl(method = "cv", 
                     number = 10,    # Number of folds for cross-validation
                     selectionFunction = "oneSE") 
model_bw <- train(V ~ ., 
                  data = data_train, 
                  method = "leapBackward", 
                  trControl = ctrl,
                  tuneGrid = expand.grid(nvmax = 1:6)) 



### Lasso model 
# Fit the Lasso regression model

y_train <- data_train$V
x_train <- model.matrix(V ~ ., data = data_train)[,-1]
lasso_model <- glmnet(x_train, y_train, alpha = 1)
cv <- cv.glmnet(x_train, y_train, alpha = 1)

# Find the best lambda using cross-validation
set.seed(123)
cv <- cv.glmnet(x_train, y_train, alpha = 1)

# Fit the final model on the training data using the minimum lambda
model_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = cv$lambda.min)

# Display Lasso regression coefficients
coef(model_lasso)




#
df_bayes <-df[ ,c("V","hour","weekday","month","Opponent_Zone","temporada","local",
               "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
               "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
               "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
               "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
               "avg_P_o_32"  )]
df_bayes <- na.omit(df_bayes)
y <- df_bayes[,c("V")]
x <- df_bayes[,c("hour","weekday","month","Opponent_Zone","temporada","local",
                  "avg_V_1","avg_V_4","avg_V_8","avg_V_12","avg_V_16","avg_V_32","avg_P_1",
                  "avg_P_4","avg_V_o_4","avg_V_o_8","avg_V_o_12","avg_V_o_32",
                  "n_victorias","n_victorias_o", "n_derrotas","n_derrotas_o",
                  "avg_P_o_1","avg_P_o_4","avg_P_o_12","avg_P_o_16",
                  "avg_P_o_32"  )]

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=2, 
                        repeats=1)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)
#Number randomely variable selected is mtry
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(V~., 
                    data=df_bayes, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)


library(class)
train <- as.matrix(df_bayes[1:50000, 2:28])  # 50,000 filas
test <- as.matrix(df_bayes[50001:59799, 2:28] ) # 10,604 filas)
cl <- as.integer(as.vector(unlist(as.vector(df_bayes[1:50000, 1]))))
kn <- knn(train, test, cl, k = 1)
nrow(df_bayes)
nrow(train)
nrow(cl)
(success <- sum(data[751:1000, 17] == kn))


