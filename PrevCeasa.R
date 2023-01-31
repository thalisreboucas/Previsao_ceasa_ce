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



##### models -----


forecast_model <- function(product_id){

  df <- data %>% 
    dplyr::group_by(id) %>%
    dplyr::filter(id == product_id ) 
  
  df <- df %>% ungroup() %>%  select(-id)
  # Separação ----
  
splits <- df %>% initial_time_split(prop = 0.85)

  
  # Ajuste ----
  
  ## NNETAR (Neural Network AutoRegression) ----
  
  model_fit_nnetar <- nnetar_reg(epochs = 10,
                                 num_networks =50) %>%
     set_engine("nnetar") %>%
     fit(value ~ date + month(date,label = TRUE ) +week(date)+ value_comp , training(splits))

  ## Prophet boost ----
  
  model_fit_prophet <- prophet_boost(seasonality_daily = T,
                                     seasonality_weekly = F,
                                     growth = 'linear',
                                     seasonality_yearly = T,
                                     learn_rate = 0.1,
                                     mtry = 5,
                                     trees = 10,
                                     tree_depth = 550
                                      ) %>%
    set_engine("prophet_xgboost")  %>%
    fit(value ~ date + month(date,label = TRUE) + week(date) + value_comp +  temperature + precipitation , training(splits))
 
   ## ARIMA boost ----
  
  model_fit_arima_boost <- arima_boost( tree_depth = 550,
                                        mtry = 6,
                                        trees = 12,
                                     learn_rate = 0.4) %>%
    set_engine("arima_xgboost")  %>%
    fit(value ~ date + month(date,label = TRUE) + week(date) + value_comp +  temperature + precipitation , training(splits))
  
  # Avaliação ----
  
  model_table <- modeltime_table(
    model_fit_prophet,
    model_fit_nnetar,
    model_fit_arima_boost
  )
  
calibration_table <- model_table %>%
    modeltime_calibrate(new_data = testing(splits))

value_table <- calibration_table %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>% select(.model_id,.model_id,
               .model_desc,.key,.index,.value,
               .conf_lo,.conf_hi) %>% mutate(.model_id = product_id , 
                                             .value = round(.value,2),
                                             .conf_lo = round(.conf_lo,2),
                                             .conf_hi = round(.conf_hi,2))
  
graph <- value_table %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

accuracy <- calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = T)
  

residuals_tb <- model_table %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals()

residuals <- residuals_tb %>% plot_modeltime_residuals()

return(list(model_table,
            calibration_table,
            value_table,
            graph,
            accuracy,
            residuals_tb,
            residuals))
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
  
refit_tbl <- Product[[2]] %>%
  modeltime_refit(data = df)


future_tbl <- df %>% group_by(id) %>% 
  future_frame(.length_out = 180,.date_var = date)

future_tbl <- future_tbl %>% ungroup() %>%  select(-id)
df <-df %>% ungroup() %>%  select(-id)


value_prev <- refit_tbl %>%
  modeltime_forecast(new_data = future_tbl, actual_data = df)

graph <- value_prev %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

return(list(refit_tbl,
            value_prev,
            graph))

}

ABACATE_Prev<-Forcast(ABACATE,1)
ABACAXI_Prev<-Forcast(ABACAXI,2)
ABOBORA_Prev<-Forcast(ABOBORA,3)
ABOBRINHA_Prev<-Forcast(ABOBRINHA,4)
ALFACE_Prev<-Forcast(ALFACE,5)
ALHO_Prev<-Forcast(ALHO,6)
BANANA_NANICA_Prev<-Forcast(BANANA_NANICA,7)
BANANA_PRATA_Prev<-Forcast(BANANA_PRATA,8)
BATATA_Prev<-Forcast(BATATA,9)
BATATA_DOCE_Prev<-Forcast(BATATA_DOCE,10)
BERINJELA_Prev<-Forcast(BERINJELA,11)
BETERRABA_Prev<-Forcast(BETERRABA,12)
BROCOLO_Prev<-Forcast(BROCOLO,13)
CARA_Prev<-Forcast(CARA,14)
CEBOLA_Prev<-Forcast(CEBOLA,15)
CENOURA_Prev<-Forcast(CENOURA,16)
CHUCHU_Prev<-Forcast(CHUCHU,17)
COCO_VERDE_Prev<-Forcast(COCO_VERDE,18)
COUVE_Prev<-Forcast(COUVE,19)
COUVE_FLOR_Prev<-Forcast(COUVE_FLOR,20)
GOIABA_Prev<-Forcast(GOIABA,21)
INHAME_Prev<-Forcast(INHAME,22)
JILO_Prev<-Forcast(JILO,23)
LARANJA_PERA_Prev<-Forcast(LARANJA_PERA,24)
LIMAO_TAHITI_Prev<-Forcast(LIMAO_TAHITI,25)
MACA_Prev<-Forcast(MACA,26)
MAMAO_FORMOSA_Prev<-Forcast(MAMAO_FORMOSA,27)
MAMAO_HAWAY_Prev<-Forcast(MAMAO_HAWAY,28)
MANDIOCA_Prev<-Forcast(MANDIOCA,29)
MANDIOQUINHA_Prev<-Forcast(MANDIOQUINHA,30)
MANGA_Prev<-Forcast(MANGA,31)
MARACUJA_AZEDO_Prev<-Forcast(MARACUJA_AZEDO,32)
MELANCIA_Prev<-Forcast(MELANCIA,33)
MELAO_AMARELO_Prev<-Forcast(MELAO_AMARELO,34)
MILHO_VERDE_Prev<-Forcast(MILHO_VERDE,35)
MORANGO_Prev<-Forcast(MORANGO,36)
OVOS_Prev<-Forcast(OVOS,37)
PEPINO_Prev<-Forcast(PEPINO,38)
PERA_IMPORTADA_Prev<-Forcast(PERA_IMPORTADA,39)
PIMENTAO_VERDE_Prev<-Forcast(PIMENTAO_VERDE,40)
QUIABO_Prev<-Forcast(QUIABO,41)
REPOLHO_Prev<-Forcast(REPOLHO,42)
TANGERINA_Prev<-Forcast(TANGERINA,43)
TOMATE_Prev<-Forcast(TOMATE,44)
UVA_ITALIA_Prev<-Forcast(UVA_ITALIA,45)
UVA_NIAGARA_Prev<-Forcast(UVA_NIAGARA,46)
VAGEM_Prev<-Forcast(VAGEM,47)



future_tbl <- data %>% group_by(id) %>% 
  future_frame(.length_out = 26)

future_tbl %>% view()
