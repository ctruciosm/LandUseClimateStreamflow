library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(forecast)
library(trendchange)
data <- read_xlsx("data/Streamflow serie/Measured vs. Simulated.xlsx")


data <- data |> 
  separate_wider_delim(
    cols = Data, 
    delim = "_", 
    names = c(NA, NA, "mes", "ano"),
    cols_remove = FALSE) |> 
  mutate(
    mes = as.numeric(mes),
    ano = as.numeric(ano),
    Data = make_date(ano, mes, 1)) |> 
  mutate(Dif = Medido - Simulado) |> 
  select(Data, Medido, Simulado, Dif) 

# EDA original series 
vazao_ts <- ts(data$Medido, start = c(1985, 1), frequency = 12)
ggtsdisplay(vazao_ts)

# EDA residuals 
residuos <- data$Medido - data$Simulado
residuos_ts <- ts(residuos, start = c(1985, 1), frequency = 12)
ggtsdisplay(residuos_ts)

# Apply SARIMA to the residuals
residuos_ts <- ts(residuos, start = c(1985, 1), frequency = 12)
fit <- arima(residuos_ts, order = c(2, 0, 0), seasonal = list(order = c(2, 0, 1), period = 12))
ggtsdisplay(fit$residuals)
fit