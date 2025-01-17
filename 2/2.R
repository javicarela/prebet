# Exploration and modification for 4 part
load("~prebet/data_frames/NBA_games_info.RData")
df$Date <- as.Date(df$Date)
df <- df %>%
  group_by(game_id) %>%
  mutate(odds.opponent = ifelse(Team == first(Team), 
                                nth(odds, 2), 
                                nth(odds, 1))) %>%
  ungroup()
setDT(df)
setwd("C:/Users/franc/Documents/prebet")
df <- df[!is.na(df$odds),]
df$Team <- factor(df$Team)
library(dplyr)
library(zoo)
df <- df %>%
  # Asignar 1 si Team gana y -1 si Opponent gana
  mutate(score_tmp = ifelse(Points > OpponentPoints, 1, -1)) %>%
  # Agrupar por Team y Opponent para calcular el puntaje acumulado
  group_by(Team, Opponent) %>%
  # Calcular el puntaje acumulado excluyendo el partido actual
  mutate(score = lag(cumsum(score_tmp), default = 0)) %>%
  ungroup() %>%
  # Eliminar la columna temporal
  select(-score_tmp)

df <- df %>%
  # Crear una columna que indica 1 si Team gana y -1 si pierde
  mutate(score_tmp = ifelse(Points > OpponentPoints, 1, -1)) %>%
  # Agrupar por Team y Opponent para considerar solo partidos entre ellos
  group_by(Team, Opponent) %>%
  # Calcular el puntaje acumulado de los últimos 10 partidos entre Team y Opponent, excluyendo el actual
  mutate(score_last_10_between = lag(rollapply(score_tmp, width = 10, align = "right", 
                                               FUN = sum, fill = NA, partial = TRUE), default = NA)) %>%
  ungroup() %>%
  # Eliminar la columna temporal
  select(-score_tmp)
df <- df %>%
  # Crear una columna que indica 1 si Team gana y -1 si pierde
  mutate(score_tmp = ifelse(Points > OpponentPoints, 1, -1)) %>%
  # Agrupar por Team y Opponent para considerar solo partidos entre ellos
  group_by(Team, Opponent) %>%
  # Points acumulated in the last 10 games between  Team and Opponent, extracting the actual game
  mutate(score_last_5_between = lag(rollapply(score_tmp, width = 5, align = "right", 
                                               FUN = sum, fill = NA, partial = TRUE), default = NA)) %>%
  ungroup() %>%
  # Delete the temporal column
  select(-score_tmp)

df$hour <- (df$hour - min(df$hour)) / (max(df$hour) - min(df$hour))
df$diff_victorias <- df$n_victorias- df$n_victorias_o
summary((df$odds)^1.1)
# Como puedo obtener mejores cuotas apostando en diferentes sitios,
#obtengo un 0.03 - 0.04 más en cada cuota de media 

save(df, file = "df.RData")

# Análisis de outliers
#install.packages("ggstatsplot")
library(ggstatsplot)
boxplot(df)$out
# Not run
