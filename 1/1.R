library(rvest)
library(data.table)
library(stringr)
library(lubridate)
library(zoo)
# Descarga de los resultados
# Preparacion urls
t <- proc.time() # Inicia el cronómetro
web <- "https://www.basketball-reference.com"
months <- c("october","november","december","january","february","march",
               "april","may","june")
months_number <- c(10,11,12,01,02,03,04,05,06)
years <- c(2001:2019,2022:2025)

#=============================================================================
#=======================================================
# Revisar 2020 y 2021
years <- c(2001:2019,2022:2024)
url <- paste("https://www.basketball-reference.com/leagues/NBA_",years[1],"_games-october.html",sep="")
url <- read_html(url)
df <- url %>% 
  html_table() %>% 
  as.data.frame()
df <- cbind (df, temporada = rep(years[1],nrow(df)))
for(j in 1:length(years)){
  for (i in 1:length(months)){
    url <- paste("https://www.basketball-reference.com/leagues/NBA_",years[j],"_games-",
                 months[i],".html",sep = "")
    Sys.sleep(runif(1, min = 2, max = 10))
    try(url <- read_html(url))
    try(tabla <- url %>% 
      html_table() %>% 
      as.data.frame())
    try(tabla <- cbind (tabla, temporada = rep(years[j],nrow(tabla))))
    try(df <- rbind(df,tabla))
  }
  cat("Download of season ", years[j], "complete \n")
}
proc.time()-t    # Detiene el cronómetro
# Revisar 2020 y 2021
years_2021<- c(2020,2021,2025)
months_2020 <- c("october","november","december","january","february","march",
                 "april","may","june","july","august","september")

url <- paste("https://www.basketball-reference.com/leagues/NBA_",years_2021[1],"_games-october.html",sep="")
url <- read_html(url)
df_2020 <- url %>% 
  html_table() %>% 
  as.data.frame()
try(df_2020 <- cbind (df_2020,temporada = rep(years_2021[1],nrow(df_2020))))
for(j in 1:length(years_2021)){
  for (i in 1:length(months_2020)){
    url <- paste("https://www.basketball-reference.com/leagues/NBA_",years_2021[j],"_games-",
                 months_2020[i],".html",sep = "")
    print(months_2020[i])
    try(tryCatch({
      url <- read_html(url)
      Sys.sleep(runif(1, min = 2, max = 10))
    }))
    try(tabla <- url %>% 
          html_table() %>% 
          as.data.frame())
    try(tabla <- cbind (tabla,temporada = rep(years_2021[j],nrow(tabla))))
    try(df_2020 <- rbind(df_2020,tabla, fill = TRUE))
  }
  cat("Download of season ", years_2021[j], "complete \n")
}
df <- rbind(df,df_2020)

# 25 minutos aproximadamente
#save(df , file = "df.RData")

# Añadimos la página web
setDT(df)


# Seleccionamos las columnas de interés
df <- df[,-c("Var.7","Var.8","Notes","Arena")]
# Modificación de variables 
str(df)
df$Date <- lubridate::mdy(df$Date)
df$Home.Neutral <- as.factor(df$Home.Neutral)
df$Visitor.Neutral <- as.factor(df$Visitor.Neutral)
df$Start..ET. <- gsub("p$", "pm", df$Start..ET.)
df$Date <-as.POSIXct(paste(df$Date, df$Start..ET.), format = "%Y-%m-%d %I:%M%p")
df$Attend. <- as.integer(gsub(",", "", df$Attend.))
df$LOG  <- as.numeric(sub("(\\d+):(\\d+)", "\\1", df$LOG)) * 60 + as.numeric(sub("(\\d+):(\\d+)", "\\2", df$LOG))
df <- df[,-c("Start..ET.")]
save(df , file = "df.RData")
## Añadimos el weekday ##
df$weekday <- as.factor(weekdays.POSIXt(df$Date,abbreviate = TRUE))
df$weekday <- factor(df$weekday, levels = c("lu.", "ma.", "mi.", "ju.", "vi.", "sá.", "do."))
df$month <- as.factor(months.POSIXt(df$Date, abbreviate = TRUE))
df$month <- factor(df$month, levels = c("oct.", "nov.", "dic.", "ene.", "feb.", "mar.", "abr.", "may.", "jun.","jul.","ago.","sep."))
df$hour <- hour(df$Date)
df[, year := year(Date)]
## Modificación   & 2 Filas por cada resultado, 1 fila por cada resultado/equipo & ##
df <- rbind(
  df[, .(Date, Team = Visitor.Neutral, Points = PTS, Opponent = Home.Neutral, OpponentPoints = PTS.1,
         Attend., LOG,temporada, hour, weekday, month, local = 0)],  # Primera fila, equipo visitante
  df[, .(Date, Team = Home.Neutral, Points = PTS.1, Opponent = Visitor.Neutral, OpponentPoints = PTS,
         Attend., LOG, temporada, hour, weekday, month, local = 1)]  # Segunda fila, equipo local
)


df <- df[order(Date),]

#Cambiamos el nombre a los equipos históricos que han cambiado de nombre
df$Team <- as.factor(gsub("New Jersey Nets", "Brooklyn Nets", df$Team))
df$Team <- as.factor(gsub("Charlotte Bobcats", "Charlotte Hornets", df$Team))
df$Team <- as.factor(gsub("New Orleans Hornets|New Orleans/Oklahoma City Hornets", "New Orleans Pelicans", df$Team))
df$Team <- as.factor(gsub("Seattle SuperSonics", "Oklahoma City Thunder", df$Team))
df$Team <- as.factor(gsub("Vancouver Grizzlies", "Memphis Grizzlies", df$Team))

df$Opponent <- as.factor(gsub("New Jersey Nets", "Brooklyn Nets", df$Opponent))
df$Opponent <- as.factor(gsub("Charlotte Bobcats", "Charlotte Hornets", df$Opponent))
df$Opponent <- as.factor(gsub("New Orleans Hornets|New Orleans/Oklahoma City Hornets", "New Orleans Pelicans", df$Opponent))
df$Opponent <- as.factor(gsub("Seattle SuperSonics", "Oklahoma City Thunder", df$Opponent))
df$Opponent <- as.factor(gsub("Vancouver Grizzlies", "Memphis Grizzlies", df$Opponent))

# Equipos de la Conferencia Este
atlantic_teams <- c("Boston Celtics", "Brooklyn Nets", "New York Knicks", "Philadelphia 76ers", "Toronto Raptors")
central_teams <- c("Chicago Bulls", "Cleveland Cavaliers", "Detroit Pistons", "Indiana Pacers", "Milwaukee Bucks")
southeast_teams <- c("Atlanta Hawks", "Miami Heat","Charlotte Hornets", "Orlando Magic", "Washington Wizards")

# Equipos de la Conferencia Oeste
northwest_teams <- c("Denver Nuggets", "Minnesota Timberwolves", "Oklahoma City Thunder", "Portland Trail Blazers", "Utah Jazz")
pacific_teams <- c("Golden State Warriors", "Los Angeles Clippers", "Los Angeles Lakers", "Phoenix Suns", "Sacramento Kings")
southwest_teams <- c("Dallas Mavericks", "Houston Rockets", "Memphis Grizzlies", "New Orleans Pelicans", "San Antonio Spurs")

# Asignar la Conferencia
df$Conferencia <- ifelse(df$Team %in% c(atlantic_teams, central_teams, southeast_teams), "Este", "Oeste")
# Asignar la División según el equipo
df$División <- ifelse(df$Team %in% atlantic_teams, "Atlántico",
                      ifelse(df$Team %in% central_teams, "Central",
                             ifelse(df$Team %in% southeast_teams, "Sureste",
                                    ifelse(df$Team %in% northwest_teams, "Noroeste",
                                           ifelse(df$Team %in% pacific_teams, "Pacífico",
                                                  ifelse(df$Team %in% southwest_teams, "Suroeste", NA))))))
df$Conferencia <- ifelse(df$Team %in% c(atlantic_teams, central_teams, southeast_teams), "Este", "Oeste")
df$Team_Zone <- factor(paste(df$Conferencia, df$División, sep = "_"))
df <- df[!Team=="Playoffs",]
df <- df[,-c("Conferencia","División")]
# Asignar la División según el equipo oponente
df$Conferencia <- ifelse(df$Opponent %in% c(atlantic_teams, central_teams, southeast_teams), "Este", "Oeste")
df$División <- ifelse(df$Opponent %in% atlantic_teams, "Atlántico",
                      ifelse(df$Opponent %in% central_teams, "Central",
                             ifelse(df$Opponent %in% southeast_teams, "Sureste",
                                    ifelse(df$Opponent %in% northwest_teams, "Noroeste",
                                           ifelse(df$Opponent %in% pacific_teams, "Pacífico",
                                                  ifelse(df$Opponent %in% southwest_teams, "Suroeste", NA))))))
df$Opponent_Zone <- factor(paste(df$Conferencia, df$División, sep = "_"))
df <- df[,-c("Conferencia","División")]


## Creación columna victoria ##
df$Points <- as.integer(df$Points)
df$OpponentPoints <- as.integer(df$OpponentPoints)
df[,V:= ifelse(Points>OpponentPoints,1,0) ]
df[,V_o:= ifelse(Points<OpponentPoints,1,0) ]
df <- df[!duplicated(df[, c("Date", "Team", "Opponent")]), ]
save(df , file = "df.RData")
#=============================================================================
# Creación de valores estadísticos
library(dplyr)
# Días después del último partido
df <- df %>%
  arrange(Team, Date) %>%  # Ordenar por equipo y fecha
  group_by(Team) %>%       # Agrupar por equipo
  mutate(days_since_last = difftime(Date, lag(Date), units = "days"),
         days_since_last = ifelse(days_since_last > 80, 30, days_since_last)) %>%  # Transformar > 83 en NA
  ungroup()
df$days_since_last <- ifelse(df$days_since_last > 80, 30, df$days_since_last)
# Días después del último partido del oponente
df <- df %>%
  arrange(Opponent, Date) %>%  # Ordenar por equipo y fecha
  group_by(Opponent) %>%       # Agrupar por equipo
  mutate(days_since_last_o = difftime(Date, lag(Date), units = "days"),
         days_since_last_o = ifelse(days_since_last_o > 80, 30, days_since_last_o)) %>%  # Transformar > 83 en NA
  ungroup()
df$days_since_last_o <- ifelse(df$days_since_last_o > 80, 30, df$days_since_last_o)
# Partidos jugados
df <- df %>%
  arrange(Team, Date) %>%  # Ordenar por equipo y fecha
  group_by(Team, temporada) %>%  # Agrupar por equipo y temporada
  mutate(games_played = row_number()) %>%  # Contar el número de partidos hasta la fecha
  ungroup()


# Medía los X dias anteriores
df <- df %>%
  group_by(Team) %>%  # Agrupar por equipo
  arrange(Team, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    avg_V_1 = rollapply(V, width = 2, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_V_4 = rollapply(V, width = 5, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_V_8 = rollapply(V, width = 9, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_V_12 = rollapply(V, width = 13, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_V_16 = rollapply(V, width = 17, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_V_32 = rollapply(V, width = 33, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right")  
    ) %>%
  ungroup()  
setDT(df)



# Creación de valores estadísticos
df <- df %>%
  group_by(Team) %>%  # Agrupar por equipo
  arrange(Team, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    avg_P_o_1 = rollapply(OpponentPoints, width = 2, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_P_o_4 = rollapply(OpponentPoints, width = 5, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_P_o_8 = rollapply(OpponentPoints, width = 9, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_P_o_12 = rollapply(OpponentPoints, width = 13, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_P_o_16 = rollapply(OpponentPoints, width = 17, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_P_o_32 = rollapply(OpponentPoints, width = 33, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right")  
  ) %>%
  ungroup()  


# Creación de valores estadísticos
df <- df %>%
  group_by(Team) %>%  # Agrupar por equipo
  arrange(Team, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    avg_P_1 = rollapply(Points, width = 2, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_P_4 = rollapply(Points, width = 5, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_P_8 = rollapply(Points, width = 9, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_P_12 = rollapply(Points, width = 13, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_P_16 = rollapply(Points, width = 17, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_P_32 = rollapply(Points, width = 33, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right")  
  ) %>%
  ungroup()  


# Creación de valores estadísticos
df <- df %>%
  group_by(Opponent) %>%  # Agrupar por equipo
  arrange(Opponent, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    avg_V_o_1 = rollapply(V_o, width = 2, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_V_o_4 = rollapply(V_o, width = 5, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_V_o_8 = rollapply(V_o, width = 9, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_V_o_12 = rollapply(V_o, width = 13, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_V_o_16 = rollapply(V_o, width = 17, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_V_o_32 = rollapply(V_o, width = 33, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right")  
  ) %>%
  ungroup()  
setDT(df)


# Creación de valores estadísticos
df <- df %>%
  group_by(Team) %>%  # Agrupar por equipo
  arrange(Team, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    avg_diff_P_1 = rollapply(Points-OpponentPoints, width = 2, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_diff_P_4 = rollapply(Points-OpponentPoints, width = 5, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_diff_P_8 = rollapply(Points-OpponentPoints, width = 9, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_diff_P_12 = rollapply(Points-OpponentPoints, width = 13, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_diff_P_16 = rollapply(Points-OpponentPoints, width = 17, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_diff_P_32 = rollapply(Points-OpponentPoints, width = 33, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right")  
  ) %>%
  ungroup()  
df <- df %>%
  group_by(Opponent) %>%  # Agrupar por equipo
  arrange(Opponent, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    avg_diff_P_o_1 = rollapply(OpponentPoints-Points, width = 2, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_diff_P_o_4 = rollapply(OpponentPoints-Points, width = 5, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"), 
    avg_diff_P_o_8 = rollapply(OpponentPoints-Points, width = 9, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_diff_P_o_12 = rollapply(OpponentPoints-Points, width = 13, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_diff_P_o_16 = rollapply(OpponentPoints-Points, width = 17, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right"),  
    avg_diff_P_o_32 = rollapply(OpponentPoints-Points, width = 33, FUN = function(x) mean(x[-length(x)]), fill = NA, align = "right")  
  ) %>%
  ungroup()  



df <- df[!Team=="TRUE",]
df$Team <- droplevels(df$Team)
df <- df[df$Opponent_Zone != "Oeste_NA", ]
df <- droplevels(df)


# Añadimos la suma de las victorias y derrotas, para cada equipo y su oponente
df <- df %>%
  group_by(Team, temporada) %>%  # Agrupar por equipo y temporada
  arrange(Team, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    n_victorias = lag(cumsum(V == 1), default = 0),
    n_derrotas = lag(cumsum(V == 0), default = 0)
  ) %>%
  ungroup()

df <- df %>%
  group_by(Opponent, temporada) %>%  # Agrupar por equipo y temporada
  arrange(Opponent, Date) %>%  # Ordenar por equipo y fecha
  mutate(
    n_victorias_o = lag(cumsum(V_o == 1), default = 0),
    n_derrotas_o = lag(cumsum(V_o == 0), default = 0)
  ) %>%
  ungroup()
setDT(df)
df[, year := year(Date)]
df[year == 2020 & month %in% c("jul.", "ago.", "sep."), 
   prueba := factor(month, levels = c("jul.", "ago.", "sep."), labels = c("oct.", "nov.", "dic."))]


df <- df %>%
  mutate(month = if_else(month %in% c("jul.","sep."), "jun.", month))
df <- df %>%
  mutate(month = if_else(month %in% c("ago."), "abr.", month))
df$month <- as.factor(df$month)
df <- df[,-c("prueba")]
str(df)
df <- df[!duplicated(df), ]

df <- df %>%
  mutate(game_id = paste(Date, pmin(as.character(Team), as.character(Opponent)), 
                         pmax(as.character(Team), as.character(Opponent)), sep = "_"))
colnames(df)

save(df , file = "df.RData")



# ==================================================================================
df_ranking <- df[,c("Date","temporada","Team","Opponent","n_victorias","n_derrotas")]
str(df_ranking)
# Creación de victorias medias del equipo anterior
df_ranking <- df_ranking %>%
  group_by(temporada, Team) %>%  # Agrupar por temporada y equipo
  arrange(temporada, Team, Date) %>%  # Ordenar por temporada, equipo y fecha
  mutate(n_victorias_acumuladas = cumsum(n_victorias)) %>%  # Sumar las victorias acumuladas hasta esa fecha
  ungroup() %>%
  group_by(temporada, Date) %>%  # Agrupar por temporada y fecha
  arrange(desc(n_victorias_acumuladas)) %>%  # Ordenar por victorias acumuladas
  mutate(ranking = dense_rank(desc(n_victorias_acumuladas))) %>%  # Establecer el ranking basado en victorias acumuladas
  ungroup()

# Creación matriz de rankings
equipos <- levels(df$Team)
setDT(df)
df[, Date := as.Date(Date)]
temporadas <- unique(df$temporada)
min_fecha <- min(as.Date(df$Date[df$temporada == temporadas[1]]), na.rm = TRUE)
max_fecha <- max(as.Date(df$Date[df$temporada == temporadas[1]]), na.rm = TRUE)
fechas <- seq(from = min_fecha, 
                to = max_fecha, 
                by = "day")
dt_equipos <- data.table(Date = fechas)
for(equipo in equipos) {
    dt_equipos[, (equipo) := NA_real_]  # Puedes cambiar NA_real_ por otro valor si lo deseas
}
for (equipo in names(dt_equipos)[-1]) {
    dt_equipos[, (equipo) := df[Team == equipo, n_victorias][match(dt_equipos$Date, df[Team == equipo, Date])]]
 }
dt_equipos[, (names(dt_equipos)[-1]) := lapply(.SD, nafill, type = "locf"), .SDcols = -1]
dt_equipos[, (names(dt_equipos)[-1]) := lapply(.SD, as.numeric), .SDcols = names(dt_equipos)[-1]]
dt_equipos[, (names(dt_equipos)[-1]) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = names(dt_equipos)[-1]]

dt_equipos_total <- dt_equipos
    


for (j in 1:length(temporadas)){
  min_fecha <- min(as.Date(df$Date[df$temporada == temporadas[j]]), na.rm = TRUE)
  max_fecha <- max(as.Date(df$Date[df$temporada == temporadas[j]]), na.rm = TRUE)
  fechas <- seq(from = min_fecha, 
                to = max_fecha, 
                by = "day")
  dt_equipos <- data.table(Date = fechas)
  for(equipo in equipos) {
    dt_equipos[, (equipo) := NA_real_]  # Puedes cambiar NA_real_ por otro valor si lo deseas
  }
  for (equipo in names(dt_equipos)[-1]) {
    dt_equipos[, (equipo) := df[Team == equipo, n_victorias][match(dt_equipos$Date, df[Team == equipo, Date])]]
  }
  dt_equipos[, (names(dt_equipos)[-1]) := lapply(.SD, nafill, type = "locf"), .SDcols = -1]
  dt_equipos[, (names(dt_equipos)[-1]) := lapply(.SD, as.numeric), .SDcols = names(dt_equipos)[-1]]
  dt_equipos[, (names(dt_equipos)[-1]) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = names(dt_equipos)[-1]]
  dt_equipos_total <- rbind(dt_equipos_total , dt_equipos)
  
}
df[,ranking := NA]
df_red <- df[,c("Date","Team","Opponent","ranking","temporada")]
df_red$Date <- as.Date(df_red$Date)
df_red[, Team := as.character(Team)]


df_red <- merge(
  df_red,
  dt_equipos_total,
  by = "Date",
  all.x = TRUE
)

# Asignamos el ranking basado en el equipo en la columna correspondiente de dt_equipos_total

head(df_red)
df_prueba <- merge(df,df_red[,c("Date","Team","ranking")], by = c("Date","Team") ,all.x = TRUE)

table(df_prueba$ranking.y)
for (j in 1:nrow(df_red)) {
  for (i in 1:nrow(dt_equipos_total)) {
    try(if (dt_equipos_total$Date[i] == df_red$Date[j]) {
      equipo <- as.character(df_red$Team[j])
      df_red$ranking[j] <- dt_equipos_total[i, get(equipo)]
    })
  }
  if (j %% 1000 == 0) {
    cat("Iteración", j, "de", nrow(dt_equipos), "\n")
  }
  
}
save(df , file = "df.RData")
df_red[, Opponent := as.character(Opponent)]
t <- proc.time() # Inicia el cronómetro
for (j in 1:nrow(df_red)) {
  for (i in 1:nrow(dt_equipos_total)) {
    try(if (dt_equipos_total$Date[i] == df_red$Date[j]) {
      equipo <- as.character(df_red$Opponent[j])
      df_red$ranking_o[j] <- dt_equipos_total[i, get(equipo)]
    })
  }
  if (j %% 1000 == 0) {
    cat("Iteración", j, "de", nrow(dt_equipos_total), "\n")
  }
  
}
proc.time()-t    # Detiene el cronómetro

save(df_red, file = "df_red.RData")



for (j in 1:nrow(df_red)) {
  # Extraer el equipo de la fila actual
  equipo <- as.character(df_red$Team[j])
  
  # Buscar la fecha en dt_equipos y asignar el valor correspondiente en la columna del equipo
  # usando la fecha y el nombre del equipo
  ranking_value <- dt_equipos[Date == df_red$Date[j], get(equipo), with = FALSE]
}

df_red$Date
dt_equipos[1,get(as.character(df$Team[1]))]
dt_equipos[1,c("Charlotte Hornets")]

head(dt_equipos)
df2$ranking
df2[, ranking := get(Team), by = .(Date, Team)]
View(df2)
table(duplicated(df))

df [,ranking := NA]

View(dt_equipos)
colnames(dt_equipos)
dt_equipos[,c(1,32:61)]
dt_equipos <- dt_equipos[,c()]
#################
months_prev <- c("october","november","december")
months_post <-c("january","february","march",
                "april","may","june")
months_urls_prev <- paste0("https://www.basketball-reference.com/leagues/NBA_2023_games-",
                           months_prev,".html",sep="")
months_urls_post <- paste0("https://www.basketball-reference.com/leagues/NBA_2023_games-",
                           months_post,".html",sep="")
boxes_total <- c()
for (j in 12:(length(years)-1)){
  for (i in 1:length(months_prev)){
    url<-read_html(months_urls_prev[i])
    urls <- url %>% html_nodes("a") %>% html_attr("href")
    boxes_month <- urls[grepl(paste(years[j],months_number[i],sep=""), urls,fixed = FALSE)]
    boxes_month <- paste(web, boxes_month,sep="")
    boxes_total <- c(boxes_total,boxes_month)
    Sys.sleep(runif(1, min = 0,  max = 5))
    cat("Download of ",months_prev[i],years[j], "complete \n")
  }
  cat ( "========================== \n")
  for (i in 1:length(months_post)){
    url<-read_html(months_urls_post[i])
    urls <- url %>% html_nodes("a") %>% html_attr("href")
    boxes_month <- urls[grepl(paste(years[j+1],0,months_number[i+3],sep=""), urls,fixed = FALSE)]
    boxes_month <- paste(web, boxes_month,sep="")
    boxes_total <- c(boxes_total,boxes_month)
    Sys.sleep(runif(1, min = 0, max = 5))
    cat("Download of ",months_post[i],years[j+1],  "complete \n")
  }
  
  cat("Download of season",years[j], "-", years[j+1] ," urls COMPLETE \n
    ============================================ \n")
}
# Revisar 2011  y 2020
boxes_total <- c()
for (i in 1:length(months_prev)){
  url<-read_html(months_urls_prev[i])
  urls <- url %>% html_nodes("a") %>% html_attr("href")
  boxes_month <- urls[grepl(paste("2000",months_number[i],sep=""), urls,fixed = FALSE)]
  boxes_month <- paste(web, boxes_month,sep="")
  boxes_total <- c(boxes_total,boxes_month)
  Sys.sleep(runif(1, min = 0, max = 5))
  cat("Download of ",months_prev[i]," 2000 complete \n")
}
cat ( "========================== \n")
for (i in 1:length(months_post)){
  url<-read_html(months_urls_post[i])
  urls <- url %>% html_nodes("a") %>% html_attr("href")
  boxes_month <- urls[grepl(paste("2001",0,months_number[i+3],sep=""), urls,fixed = FALSE)]
  boxes_month <- paste(web, boxes_month,sep="")
  boxes_total <- c(boxes_total,boxes_month)
  Sys.sleep(runif(1, min = 0, max = 5))
  cat("Download of ",months_post[i]," 2001 complete \n")
}

cat("Download of season 2000-2001 urls COMPLETE \n
    ============================================")
rm(boxes_month,url,urls)



# Extracción de playoffs
url <- "https://www.basketball-reference.com/playoffs/2023-nba-finals-heat-vs-nuggets.html"
url <- read_html(url)
html_table(url)
# Selección páginurl# Selección página Play by Play
pbp_url <- paste0(substr(boxes_total, 1, 46), "/pbp", substr(boxes_total, 47, nchar(boxes_total)))
df <- cbind(df, pbp_url)

# Selección estadísticas del partido por los equipos
shot_chart_url <- paste0(substr(boxes_total, 1, 46), "/shot-chart", substr(boxes_total, 47, nchar(boxes_total)))

# Selección equipos
for (i in 1:length(shot_chart_url)){
  url_box <- shot_chart_url[i]
  url<-read_html(url_box)
  Sys.sleep(runif(1, min = 0, max = 2))
  table_team1 <- html_table(url)[[1]][5,]
  table_team2 <- html_table(url)[[2]][5,]
  colnames(table_team2)<-paste(colnames(table_team2),".1",sep="")
  df <- cbind(df,table_team1)
}
url_box <- months_urls_prev[1]
url<-read_html(url_box)
table_team1 <- html_table(url)[[1]][5,]
table_team2 <- html_table(url)[[2]][5,]
colnames(table_team2)<-paste(colnames(table_team2),".1",sep="")
estadisticas <- cbind(table_team1,table_team2)
#FG -- Field Goals
#FGA -- Field Goal Attempts
#3P -- 3-Point Field Goals
#3PA -- 3-Point Field Goal Attempts
#FT -- Free Throws
#FTA -- Free Throw Attempts
#ORB -- Offensive Rebounds
#DRB -- Defensive Rebounds
#AST -- Assists




# Selección de los resultados por cuartos
url <- "https://www.basketball-reference.com/boxscores/pbp/202210180BOS.html"
df_partido <- data.table(html_table(pbp)[1])
df_partido <- df_partido[[1]]

# Convertir a data frame
df <- as.data.frame(df_partido)
df <- df[,c("X1st.Q.3")]
end_of_rows <- grep("End of", df)
selected_rows <- c(end_of_rows - 1, end_of_rows)
df_selected <- df[selected_rows]
df_selected[1:4]
# =================================================================================
