pacman::p_load(tidyverse, lubridate, knitr, DT, ggplot2, plotly, ggthemes, ggfortify, forecast, MLmetrics, tsbox, xts, imputeTS, tseries, hrbrthemes, autoplotly)

SES_1 <- read_excel("data/SES_TS.xlsx", sheet = "T3.4")

write_rds(monthly_temp, "data/monthly_temp.rds")


monthly_temp <- read_rds("data/monthly_temp.rds")

#filter out Admiralty weather station 
adm <- monthly_temp %>%
  filter(station == "Admiralty")

#check the resultant dataframe
summary(adm)