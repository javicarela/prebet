# Cargar las librerías
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(data.table)

# Cargar los datos
load("~prebet/data_frames/NBA_games_info.RData")
# Preprocesamiento de datos
df_chart <- df %>%
  select(Team, Points, Date) %>%
  filter(Team != "Playoffs") %>%
  droplevels() %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(Points_Acc = cumsum(Points),
         Month = floor_date(as.Date(Date), "month")) %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(Rank = rank(-Points_Acc),
         Label = paste0(" ", round(Points_Acc, 2))) %>%
  ungroup()

# Colores de los equipos
team_colors <- c(
  "Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", "Brooklyn Nets" = "#000000", 
  "Charlotte Hornets" = "#1D1160", "Chicago Bulls" = "#CE1141", "Cleveland Cavaliers" = "#860038", 
  "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", "Detroit Pistons" = "#C8102E", 
  "Golden State Warriors" = "#1D428A", "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#002D62", 
  "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", "Memphis Grizzlies" = "#5D76A9", 
  "Miami Heat" = "#98002E", "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#0C2340", 
  "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#006BB6", "Oklahoma City Thunder" = "#007AC1", 
  "Orlando Magic" = "#0077C0", "Philadelphia 76ers" = "#006BB6", "Phoenix Suns" = "#1D1160", 
  "Portland Trail Blazers" = "#E03A3E", "Sacramento Kings" = "#5A2D81", "San Antonio Spurs" = "#C4CED4", 
  "Toronto Raptors" = "#CE1141", "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C"
)

# Ordenar y completar datos
chart_data <- df_chart %>%
  arrange(Month, desc(Points_Acc)) %>%
  group_by(Team, Month) %>%
  summarise(Points_Acc = last(Points_Acc), Rank = last(Rank), Label = last(Label)) %>%
  ungroup()
chart_data <- na.omit(chart_data)
# Crear una secuencia completa de meses
all_months <- seq(min(chart_data$Month), max(chart_data$Month), by = "month")

# Completar para que todos los equipos tengan valores para todos los meses
chart_data <- chart_data %>%
  complete(Team, Month = all_months) %>%
  group_by(Team) %>%
  fill(Points_Acc, Rank, Label, .direction = "down") %>%
  ungroup() %>%
  group_by(Month) %>%
  mutate(Rank = rank(-Points_Acc)) %>%
  ungroup()

# Configuración del gráfico inicial
p <- ggplot(chart_data, aes(Rank, group = Team, fill = as.factor(Team), color = as.factor(Team))) +
  geom_tile(aes(y = Points_Acc / 2, height = Points_Acc, width = 0.9), alpha = 0.9, color = NA) +
  geom_text(aes(y = 0, label = Team), vjust = 0.3, hjust = 1.1, size = 3.3) +  # Mueve el texto hacia la derecha
  geom_text(aes(y = Points_Acc, label = Label, hjust = -0.15), size = 4) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_fill_manual(values = team_colors) +
  scale_color_manual(values = team_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 0, face = "bold"),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18, face = "bold", color = "#424242"),
    legend.position = "none",
    plot.margin = margin(1, 2, 1, 3, unit = "cm")
  )

p
# Crear la animación
anim <- p + transition_states(Month, transition_length = 4, state_length = 0.5) +
  view_follow(fixed_x = TRUE) +
  labs(title = "Accumulated points in the NBA({closest_state})")

# Renderizar la animación
animate(anim,
        duration = 180,
        width = 1024,
        height = 768,
        res = 100,
        nframes = 400,
        fps = 100,
        end_pause = 180,
        renderer = gifski_renderer("Points_Acc.gif"))




library(dplyr)
library(lubridate)
library(gganimate)
library(quantmod)
# Define stock tickers and the time period
tickers <- c("AMZN", "AAPL", "GOOGL", "MSFT", "META", "TSLA", "NVDA")
start_date <- Sys.Date() - years(10)
end_date <- Sys.Date()

# The getSymbols() function returns a data.frame of stock data for a given ticker
head(getSymbols("NVDA", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE), 10)

# Function to download and aggregate stock data
get_monthly_averages <- function(ticker) {
  # Download stock data from Yahoo Finance
  stock_data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) 
  # Remove ticker prefix from column names
  colnames(stock_data) <- gsub(paste0(ticker, "\\."), "", colnames(stock_data)) 
  # Keep only the 'Adjusted' column from the data
  stock_data <- stock_data[, "Adjusted"] 
  # Convert the data to a data frame with Date and Adjusted columns
  stock_data <- data.frame(Date = index(stock_data), Adjusted = coredata(stock_data))
  # Add a YearMonth column by flooring the Date to the nearest month
  stock_data$YearMonth <- floor_date(stock_data$Date, "month") 
  
  # Group the data by month and calcualte the mean adjusted price for each period
  monthly_data <- stock_data %>% 
    group_by(YearMonth) %>%
    summarize(Value = mean(Adjusted, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Ticker = ticker)
  
  return(monthly_data)
}
chart_colors <- c(
  AMZN = "#FF9900",
  AAPL = "#555555",
  GOOGL = "#0F9D58",
  MSFT = "#FFB900",
  META = "#0081FB",
  TSLA = "#cc0000",
  NVDA = "#76B900"
)

data <- lapply(tickers, get_monthly_averages)
data <- bind_rows(data)
data <- data %>%
  arrange(YearMonth, desc(Value))
data
head(data, 10)
chart_data <- data %>%
  group_by(YearMonth) %>%
  mutate(
    Rank = rank(-Value),
    Label = paste0("$", round(Value, 2))
  )

head(chart_data, 10)

ggplot(chart_data %>% filter(YearMonth == "2024-05-01"), aes(Rank, group = Ticker, fill = as.factor(Ticker), color = as.factor(Ticker))) +
  geom_tile(aes(y = Value / 2, height = Value, width = 0.9), alpha = 0.9, color = NA) +
  geom_text(aes(y = 0, label = Ticker), vjust = 0.2, hjust = 1.3) +
  geom_text(aes(y = Value, label = Label, hjust = -0.15)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_fill_manual(values = chart_colors) +
  scale_color_manual(values = chart_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18, face = "bold", color = "#424242"),
    legend.position = "none",
    plot.margin = margin(1, 2, 1, 2, unit = "cm")
  )

p <- ggplot(chart_data, aes(Rank, group = Ticker, fill = as.factor(Ticker), color = as.factor(Ticker))) +
  geom_tile(aes(y = Value / 2, height = Value, width = 0.9), alpha = 0.9, color = NA) +
  geom_text(aes(y = 0, label = Ticker), vjust = 0.2, hjust = 1.3) +
  geom_text(aes(y = Value, label = Label, hjust = -0.15)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_fill_manual(values = chart_colors) +
  scale_color_manual(values = chart_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18, face = "bold", color = "#424242"),
    legend.position = "none",
    plot.margin = margin(1, 2, 1, 2, unit = "cm")
  )
p
length(unique(chart_data$YearMonth))
str(chart_data)
chart_data$
anim <- p + transition_states(YearMonth, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE) +
  labs(title = "Average monthly stock price in USD ({closest_state})")
animate(
  anim,
  width = 1024,
  height = 768,
  res = 150,
  nframes = 600,
  fps = 60,
  end_pause = 60,
  renderer = gifski_renderer("stock_race_chart.gif")
)

