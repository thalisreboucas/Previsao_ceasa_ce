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


ANPCOMBUSTIVEL_2015_2022 <- read_excel("E:/edime/Thalis/MEU/Ceasa/Base-explicativas/ANPCOMBUSTIVEL -2015-2022.xlsx", 
                                       col_types = c("text", "text", "text", 
                                                     "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "date", "numeric", 
                                                     "text", "text"))




# Data ceasa
data <- Dados_Ceasa_Preco  %>% dplyr::select(id,date,value)

# Data combustivel

data_combustivel <- ANPCOMBUSTIVEL_2015_2022 %>% 
              dplyr::select(Municipio,Produto,`Data da Coleta`,`Valor de Venda`) %>% 
              dplyr::filter(Municipio == "FORTALEZA",Produto == "DIESEL S10") %>% 
              dplyr::mutate(date = `Data da Coleta`,value = `Valor de Venda`) %>% 
              dplyr::select(-`Data da Coleta`,-`Valor de Venda`) 



data_combustivel %>% 
dplyr::select()
left_join()

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
