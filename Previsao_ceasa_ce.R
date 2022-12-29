library(pacman)
pacman::p_load(readxl,# Open Data in Xl (Excel)
               plotly, # Making dynamic graphs
               tidymodels, # Model statistics
               bayesmodels, # Bayes times models 
               modeltime, # Times model classics 
               tidyverse, # All tidy for manipulation
               timetk, # Making a tables and graph's of timeseries
               lubridate, # Working if date
               datawizard, # making a spells on the datas.
               plyr,
               modeltime,
               easystats)

library(feasts)
Dados_Ceasa_Preco <- read_excel("E:/edime/Thalis/MEU/Ceasa/Dados_Ceasa_Preco.xlsx", 
                                col_types = c("text", "text", "text", "numeric", "date"))

# Deixando os dados do formato que eu preciso
data <- Dados_Ceasa_Preco %>% dplyr::select(id,date,value) 

############ Vendo a variancao de um ano (1 year)
data %>% filter(id == 1 ) %>% plot_time_series_boxplot(date,value ,.period = "1 year")

# Fazendo a regressão da serie temporal
data %>% filter(id == 1 ) %>%  plot_time_series_regression(
  .date_var     = date,
  .formula      = value ~ date + month(date, label = TRUE)
)

############## Vendo o mes por ano 
data %>% mutate(day = day(date),
                month = month(date,label = T),
                year = year(date),
                week = week(date),
                semester = semester(date),
                quarter = quarter(date)) %>% 
                filter(id == 24) %>% 
                group_by(year) %>% 
                plot_ly( x = ~month,y = ~value,color = ~as.character(year)) %>% 
                add_lines(line = list(shape = "linear",width = 3))

################## vendo o dia por ano
data %>% mutate(day = day(date),
                year = year(date)) %>% 
  filter(id == 24) %>% 
  group_by(year) %>% 
  plot_ly( x = ~day,y = ~value,color = ~as.character(year)) %>% 
  add_lines(line = list(shape = "linear",width = 3))

################## Vendo a semana por ano
data %>% mutate(year = year(date),
                week = week(date)) %>% 
  filter(id == 24) %>% 
  group_by(year) %>% 
  plot_ly( x = ~week,y = ~value,color = ~as.character(year)) %>% 
  add_lines(line = list(shape = "linear",width = 3))

######## vendo o valor por quarto de ano
data %>% mutate(day = day(date),
                month = month(date,label = T),
                year = year(date),
                week = week(date),
                semester = semester(date),
                quarter = quarter(date)) %>% 
  filter(id == 24) %>% 
  group_by(year) %>% 
  plot_ly( x = ~quarter,y = ~value,color = ~as.character(year)) %>% 
  add_lines(line = list(shape = "linear",width = 3))


####### Vendo se tem outliers 
data %>% filter(id == 24 ) %>% plot_anomaly_diagnostics(date,value)

####### vendo o acf
data %>% filter(id == 24 ) %>% plot_acf_diagnostics(date,value)

