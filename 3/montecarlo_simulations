# Montecarlo simulation
library(data.table)
load("~prebet/data_frames/NBA_games_info.RData")
setDT(df)
df_Team <- df[Team == "Atlanta Hawks",]
df_Team$V <- as.numeric(df_Team$V)
df_Team <- df_Team[,c("Date","V","temporada")]
setDT(df_Team)
data_train <- df_Team[!temporada==2024, c("V")]
data_test <- df_Team[temporada==2024, c("V")]
z <- (data_train$V-mean(data_train$V))/sd(data_train$V)
n <- length(z)
Nz <- qnorm(seq(from=1, to=n, by = 1)/(n+1),lower.tail = F)
# Empirical CDF
Fz <- pnorm(Nz, lower.tail = T)
# Teorical CDF
Psiz <- pnorm(z, lower.tail = T)
Psiz
plot(Nz,Fz, min = "Emp/thr Standard Normal CDF for our Data Sets",
     pch = 1, cex = .8)
lines(Nz, Fz, col = 'steelblue',lwd = 2)
points(Nz,Psiz, pch = 1, cex = .8)
lines(Nz, Psiz, col ='violet',lwd = 2)


model = tf.keras.Sequential([
  tf.keras.layers.Dense(128, activation="relu"),
  tf.keras.layers.Dense(256, activation="relu"),
  tf.keras.layers.Dense(128, activation="relu"),
  tf.keras.layers.Dense(1, activation="sigmoid")
])
