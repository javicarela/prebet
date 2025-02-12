#Data extraction for 2024
setwd("~/prebet/df_odds")
library(pdftools)
lines_total <- c()
for(i in 1:424){
  pdf <- pdf_text(pdf = paste(i,".pdf",sep=""), opw = "", upw = "")
  # Limpieza del pdf 
  lines <- unlist(strsplit(pdf, "\n"))
  lines <- lines[lines != "" & trimws(lines) != ""]
  lines <- lines[-(1:12)]
  start_index <- which(grepl("28\\s+Next", lines))
  # Eliminar todas las filas desde esa fila en adelante
  if (length(start_index) > 0) {
    lines <- lines[1:(start_index - 1)]
  }
  lines <- lines[!grepl("^\\s*Top\\s*$", lines)]
  lines <- gsub("\\s{3,}", "  ", lines)
  for (i in 1:length(lines)){
    if(nchar(lines[i])<=20){
      lines[i+1] = paste(lines[i],lines[i+1])
    }
  }
  lines <- lines[nchar(lines) >= 15]
  lines <- lines[!grepl("Remove", lines, ignore.case = TRUE)]
  lines <- lines[!grepl("add", lines, ignore.case = TRUE)]
  lines <- gsub("\\s{3,}", "  ", lines)
  lines <- trimws(lines)
  lines <- trimws(gsub("Top", "", lines))
  lines <- trimws(gsub("Top", "", lines))
  lines <- gsub("^(\\d{1,2} \\w{3} \\d{4}).*", "\\1", lines)  # Mantener solo la fecha
  lines <- lines[!grepl("Prev 1", lines, ignore.case = TRUE)]
  lines_total <- c(lines_total,lines)
}

lines_total <- trimws(gsub(" - Play Offs  1  2  B's", "", lines_total))
lines_total <- gsub("\\s{2,}", " ", lines_total)
lines_total <- gsub(
  "^(\\d{2}:\\d{2})\\s+(\\d+\\.\\d+\\s+\\d+\\.\\d+)\\s+(.+)$",
  "\\1 \\3 \\2",
  lines_total
)
lines_total <- gsub(" \\d{2}$", "", lines_total)

lines_total <- lines_total[!grepl("^\\d{2}:\\d{2}\\s+\\d+\\.\\d+\\s+\\d+\\.\\d+$", lines_total)]


dates <- c()
teams1 <- c()
teams2 <- c()
odds1 <- c()
odds2 <- c()
times <- c()  # Nuevo vector para almacenar las horas

# Variable para almacenar la fecha actual
current_date <- ""

# Bucle a través de los datos
for (line in lines_total) {
  # Comprobar si la línea es una fecha
  if (grepl("\\d{1,2} \\w+ \\d{4}", line)) {
    current_date <- line  # Actualizar la fecha actual
  } else {
    # Extraer los detalles del partido
    match_info <- strsplit(line, " ")[[1]]
    score_index <- which(match_info == "–")  # Encuentra el índice del guion medio
    
    # Verificar que el guion medio fue encontrado
    if (length(score_index) > 0) {
      # Los equipos se encuentran entre el tiempo y las cuotas
      time <- match_info[1]
      team1 <- paste(match_info[2:(score_index - 1)], collapse = " ")
      team2 <- paste(match_info[(score_index + 1):(length(match_info) - 2)], collapse = " ")
      odds1_value <- match_info[length(match_info) - 1]
      odds2_value <- match_info[length(match_info)]
      
      # Agregar datos a las listas
      dates <- c(dates, current_date)
      teams1 <- c(teams1, team1)
      teams2 <- c(teams2, team2)
      odds1 <- c(odds1, odds1_value)
      odds2 <- c(odds2, odds2_value)
      times <- c(times, time)  # Agregar la hora al nuevo vector
    }
  }
}

# Mostrar el resultado
df <- data.frame(Date = dates, Time = times, Team1 = teams1, Team2 = teams2, Odds1 = odds1, Odds2 = odds2)


df$Team1 <- gsub("\\s\\d+$", "", df$Team1)
df$Team2 <- gsub("^\\d+\\s", "", df$Team2)
df$Date <-  trimws(df$Date)
df$Date <- gsub(" \\d+ \\d+ B's", "", df$Date)
library(lubridate)
df$Date <- dmy(df$Date)
df$combined <- as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M")
df$combined <- df$combined - as.difftime(6, units = "hours")
df$Date <- as.Date(df$combined)
library(data.table)
setDT(df)
df$Team1 <- gsub(" OT$", "", df$Team1)
df$Team1 <- gsub(" [0-9 ]+$", "", df$Team1)
df$Team2 <- gsub(" OT$", "", df$Team2)
df$Team2 <- gsub(" [0-9 ]+$", "", df$Team2)
df <- df[!(df$Team1 == "East" | df$Team2 == "East"), ]
df$Team1 <- gsub("^[0-9]{1,2}:[0-9]{2} ", "", df$Team1)  # Elimina "03:00 " al inicio
df$Team2 <- gsub("^[0-9]{1,2}:[0-9]{2} ", "", df$Team2)  # Elimina "03:00 " al inicio
df <- df[!Team1=="Team LeBron",]
df <- df[!Team1=="Team USA",]
df <- df[!Team1=="Team World",]
df <- df[!Team1=="Team Giannis",]
df <- df[!Team1=="Team Durant",]
df <- df[!Team1=="Team Chuck",]
df <- df[!Team1=="Team Stephen",]
df <- df[!Team1=="Sophomores",]
df <- df[!Team1=="West",]
df <- df[!grepl("canc.", df$Team2, ignore.case = TRUE), ]
df$Team2 <- gsub(" -$", "", df$Team2)
df$Team2 <- gsub("^Sacramento$", "Sacramento Kings", df$Team2, ignore.case = TRUE)
df$Team2 <- gsub("^Minnesota Timberwolves 2.95$", "Minnesota Timberwolves", df$Team2, ignore.case = TRUE)
df$Team2 <- gsub("^Milwaukee$", "Milwaukee Bucks", df$Team2, ignore.case = TRUE)
df$Time <- gsub("^26$", "", df$Time, ignore.case = TRUE)
df$Odds1 <- gsub("^-$", "", df$Odds1, ignore.case = TRUE)
df$Odds1 <- gsub("^Bucks$", "", df$Odds1, ignore.case = TRUE)
df$Odds1 <- gsub("^Kings$", "", df$Odds1, ignore.case = TRUE)
df$Odds2 <- gsub("^-$", "", df$Odds2, ignore.case = TRUE)
df$Odds2 <- gsub("^1$", "", df$Odds2, ignore.case = TRUE)
df <- df[df$Odds1 != "" & !is.na(df$Odds1), ]
df <- df[!grepl("\\d\\.\\d{2}$", df$Team2), ]


# Supongamos que df ya tiene las columnas adecuadas (Date, Team1, Team2, Odds1, Odds2)
# Crear un dataframe vacío para almacenar los nuevos datos
new_df <- data.frame(Date = character(),
                     Team = character(),
                     odds = numeric(),
                     stringsAsFactors = FALSE)

# Iterar sobre cada fila del dataframe original
for (i in 1:nrow(df)) {
  # Extraer la fecha, los nombres de los equipos y sus cuotas
  date <- df$Date[i]
  team1 <- df$Team1[i]
  team2 <- df$Team2[i]
  odds1 <- as.numeric(df$Odds1[i])
  odds2 <- as.numeric(df$Odds2[i])
  
  # Crear dos nuevas filas: una para Team1 y otra para Team2
  new_row1 <- data.frame(Date = date, Team = team1, odds = odds1)
  new_row2 <- data.frame(Date = date, Team = team2, odds = odds2)
  
  # Añadir las nuevas filas al nuevo dataframe
  new_df <- rbind(new_df, new_row1, new_row2)
}
table(new_df$Team)
setDT(new_df)
new_df <- new_df[!Team=="–",]
library(dplyr)
df_odds <- new_df %>%
  arrange(Date, Team)
#Join with the first df
library(data.table)
load("~/prebet/df.RData")
df <- df[,-c("odds")]
setDT(df)
setDT(df_odds)
df_odds$Date <- df_odds$Date 

# Análisis 2023-11-12
save(df_odds, file = "df.odds.RData")
setDT(df_odds)
df_odds[, Date := as.Date(Date)]
df[, Date := as.Date(Date)]
df_odds[, Team := as.character(Team)]
df[, Team := as.character(Team)]
# join the data.frames
df$year>2009
df <- merge(df_odds, df[df$year>=2009,], by = c("Date", "Team"), all.y = TRUE)
# Introduce the rest manually
save(df, file = "df.RData")
