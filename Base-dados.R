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
               rstanarm)

# Base preço dos alimentos Ceasa
Dados_Ceasa_Preco <- read_excel("E:/edime/Thalis/MEU/Ceasa/Dados_Ceasa_Preco.xlsx", 
                                col_types = c("text", "text", "text", "numeric", "date"))

# Base dos preços dos combustiveis (foco no DIESEL S10)
ANPCOMBUSTIVEL_2015_2022 <- read_excel("E:/edime/Thalis/MEU/Ceasa/Base-explicativas/ANPCOMBUSTIVEL -2015-2022.xlsx", 
                                       col_types = c("text", "text", "text", 
                                                     "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "date", "numeric", 
                                                     "text", "text"))

# Base de preciptação 
INMENT_FORTALEZA_2015_2022 <- read_excel("Base-explicativas/INMENT-FORTALEZA-2015-2022.xlsx", 
                                         col_types = c("date", "date", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric"))

# Data ceasa
data <- Dados_Ceasa_Preco  %>% dplyr::select(id,date,value)

remove(Dados_Ceasa_Preco)

# Data combustivel
data_combustivel <- ANPCOMBUSTIVEL_2015_2022 %>% 
              dplyr::mutate(date = date(`Data da Coleta`),value = `Valor de Venda`) %>% 
              dplyr::filter(Municipio == "FORTALEZA",Produto == "DIESEL S10") %>% 
              dplyr::select(Municipio,Produto,date,value) %>% 
              dplyr::group_by(date) %>% 
              dplyr::summarise(value = mean(value))

remove(ANPCOMBUSTIVEL_2015_2022)
  

# Data tempo
data_temp <- INMENT_FORTALEZA_2015_2022 %>% 
              dplyr::mutate(date = date(`DATA (YYYY-MM-DD)`),
                            precipitation = `PRECIPITAÇÃO TOTAL, HORÁRIO (mm)`,
                            temperature =`TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)` ) %>% 
              dplyr::select(date,
                            precipitation,
                             temperature) %>% 
              dplyr::group_by(date) %>% 
              dplyr::summarise(precipitation = sum(precipitation) , temperature = mean(temperature))


remove(INMENT_FORTALEZA_2015_2022)


# AED CEASA -----

itens_abc <- c(24,8,27,34,44,33,16)

EDA <- function(data,id_i){ 
  dt <- data %>%
    dplyr::group_by(id) %>%
    dplyr::filter(id == id_i)
  
  n <- length(plyr::count(id_i)$x)
  
  if (n > 3) {
    x = 3
  } else {
    x = 1
  }
  
  graph <-  dt %>%
    timetk::plot_time_series(.facet_ncol = x,
                             .date_var    = date,
                             .value       = value)
  
  
  table <-  dt %>%  datawizard::describe_distribution()
  
  anomaly <- dt %>% plot_anomaly_diagnostics(date,
                                             value,
                                             .facet_ncol = x)
  
  acf <- dt %>% plot_acf_diagnostics(
    date, value,               # ACF & PACF
    .lags = "30 days",          # 7-Days of hourly lags
    .interactive = FALSE
  )
  
  seasonal <- dt %>% plot_seasonal_diagnostics(date, value, .interactive = T)
  stl <- dt %>% plot_stl_diagnostics(  date, value,
                                       .feature_set = c("observed","season","trend","remainder"),
                                       .interactive = T)
  
  return(list(
    table <- table,
    graph <- graph ,
    anomaly <- anomaly ,
    acf <- acf,
    seasonal <- seasonal,
    stl <- stl
  ))  
  
}

########## EDA - COMBUSTIVEL 
EDA <- function(data){ 
  dt <- data
  
  
  graph <-  dt %>%
    timetk::plot_time_series(.date_var    = date,
                             .value       = value)
  
  
  table <-  dt %>%  datawizard::describe_distribution()
  
  anomaly <- dt %>% plot_anomaly_diagnostics(date,
                                             value)
  
  acf <- dt %>% plot_acf_diagnostics(
    date, value,               # ACF & PACF
    .lags = "30 days",          # 7-Days of hourly lags
    .interactive = FALSE
  )
  
  seasonal <- dt %>% plot_seasonal_diagnostics(date, value, .interactive = T)
  stl <- dt %>% plot_stl_diagnostics(  date, value,
                                       .feature_set = c("observed","season","trend","remainder"),
                                       .interactive = T)
  
  return(list(
    table <- table,
    graph <- graph ,
    anomaly <- anomaly ,
    acf <- acf,
    seasonal <- seasonal,
    stl <- stl
  ))  
  
}
edacomb <- EDA(data_combustivel)
