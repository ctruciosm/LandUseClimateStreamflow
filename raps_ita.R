library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(trendchange)
data <- read_xlsx("data/Streamflow serie/Artemis River Gauging.xlsx")



compute_raps <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  raps <- cumsum((x - mu) / sigma)
  return(raps)
}

# RAPS
data_raps <- data |> 
  mutate(Year = make_date(ANO)) |> 
  select(-ANO) |> 
  filter(Year >= "1985-01-01", Year <= "2015-01-01") |> 
  rename(`Minimum streamflow` =  `Vazão Mínima (média anual)`,
    `Maximum streamflow` = `Vazão Máxima (média anual)`,
    `Average streamflow` = `Vazão Média Anual`,
    `Total streamflow` = `Vazão Total Anual` ) |> 
  mutate_if(is.numeric, compute_raps) |> 
  pivot_longer(cols = contains("streamflow"), 
  names_to='Type',
  values_to='Streamflow') 


data_raps |> 
  filter(Type == "Maximum streamflow") |> 
  ggplot(aes(Year, Streamflow)) + 
  geom_line() +
  theme_bw()

data_raps |> 
  filter(Type == "Minimum streamflow") |> 
  ggplot(aes(Year, Streamflow)) + 
  geom_line() +
  theme_bw()

# ITA
data_ita <- data |> 
  mutate(Year = make_date(ANO)) |> 
  select(-ANO) |> 
  filter(Year >= "1985-01-01", Year <= "2015-01-01") |> 
  rename(`Minimum streamflow` =  `Vazão Mínima (média anual)`,
    `Maximum streamflow` = `Vazão Máxima (média anual)`,
    `Average streamflow` = `Vazão Média Anual`,
    `Total streamflow` = `Vazão Total Anual` )

innovtrend(data_ita$`Minimum streamflow`, ci = 95)
innovtrend(data_ita$`Maximum streamflow`, ci = 95)
