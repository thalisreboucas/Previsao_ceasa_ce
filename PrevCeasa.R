library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(readxl)
library(parsnip)
library(datawizard)
library(easystats)

Dados_Ceasa_Preco <- read_excel("E:/edime/Thalis/MEU/Ceasa/Dados_Ceasa_Preco.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "skip", "skip", "skip", "numeric", 
                                              "date"))

data <- Dados_Ceasa_Preco %>% dplyr::select(id,date,value) %>%
  set_names(c("id", "date", "value"))

#47

# AED -----

graph <- function(id_i ){

data %>%
  group_by(id) %>%
  filter(id == id_i) %>%
  plot_time_series(
    .date_var    = date,
    .value       = value
  )

}

graph(24)

table <- function(id_i ){
  
data %>% filter(id == id_i) %>%  describe_distribution()
  
}

table(24)


##### models -----


forecast_model <- function(product_id){

  df <- data %>% 
    dplyr::group_by(id) %>%
    dplyr::filter(id == product_id ) 
  
  df <- df %>% ungroup() %>% select(-id)
  
  # Separação ----
  
splits <- df %>% initial_time_split(prop = 0.88)

  
  # Ajuste ----
  
  ## NNETAR (Neural Network AutoRegression) ----
  
  model_fit_nnetar <- nnetar_reg(num_networks = 100) %>%
    set_engine("nnetar") %>%
    fit(value ~ date + month(date), training(splits))

  ## Prophet ----
  
  model_fit_prophet <- prophet_boost(seasonality_daily = T ,
                                     growth = 'linear',
                                     seasonality_yearly = F,changepoint_range = 0.77
                                      ) %>%
    set_engine("prophet_xgboost")  %>%
    fit(value ~ date + month(date) + year(date), training(splits))


  
  # Avaliação ----
  
  model_table <- modeltime_table(
    model_fit_nnetar,
    model_fit_prophet
  )
  
calibration_table <- model_table %>%
    modeltime_calibrate(new_data = testing(splits))

value_table <- calibration_table %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  )
  
graph <- value_table %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

accuracy <- calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = T)
  

return(list(model_table,
            calibration_table,
            value_table,
            graph,
            accuracy))
}

ABACATE<-forecast_model(1)
ABACAXI<-forecast_model(2)
ABOBORA<-forecast_model(3)
ABOBRINHA<-forecast_model(4)
ALFACE<-forecast_model(5)
ALHO<-forecast_model(6)
BANANA_NANICA<-forecast_model(7)
BANANA_PRATA<-forecast_model(8)
BATATA<-forecast_model(9)
BATATA_DOCE<-forecast_model(10)
BERINJELA<-forecast_model(11)
BETERRABA<-forecast_model(12)
BROCOLO<-forecast_model(13)
CARA<-forecast_model(14)
CEBOLA<-forecast_model(15)
CENOURA<-forecast_model(16)
CHUCHU<-forecast_model(17)
COCO_VERDE<-forecast_model(18)
COUVE<-forecast_model(19)
COUVE_FLOR<-forecast_model(20)
GOIABA<-forecast_model(21)
INHAME<-forecast_model(22)
JILO<-forecast_model(23)
LARANJA_PERA<-forecast_model(24)
LIMAO_TAHITI<-forecast_model(25)
MACA<-forecast_model(26)
MAMAO_FORMOSA<-forecast_model(27)
MAMAO_HAWAY<-forecast_model(28)
MANDIOCA<-forecast_model(29)
MANDIOQUINHA<-forecast_model(30)
MANGA<-forecast_model(31)
MARACUJA_AZEDO<-forecast_model(32)
MELANCIA<-forecast_model(33)
MELAO_AMARELO<-forecast_model(34)
MILHO_VERDE<-forecast_model(35)
MORANGO<-forecast_model(36)
OVOS<-forecast_model(37)
PEPINO<-forecast_model(38)
PERA_IMPORTADA<-forecast_model(39)
PIMENTAO_VERDE<-forecast_model(40)
QUIABO<-forecast_model(41)
REPOLHO<-forecast_model(42)
TANGERINA<-forecast_model(43)
TOMATE<-forecast_model(44)
UVA_ITALIA<-forecast_model(45)
UVA_NIAGARA<-forecast_model(46)
VAGEM<-forecast_model(47)



Forcast <- function(Product,product_id){

 df <- data %>% 
    dplyr::group_by(id) %>%
    dplyr::filter(id == product_id ) 
  
  df <- df %>% ungroup() %>% select(-id)
  
  
refit_tbl <- Product[[2]] %>%
  modeltime_refit(data = df)

value_prev <- refit_tbl %>%
  modeltime_forecast(h = "4 month", actual_data = df)

graph <- value_prev %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

return(list(refit_tbl,
            value_prev,
            graph))

}


