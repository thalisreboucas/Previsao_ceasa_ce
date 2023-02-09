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
               imputeTS,
               rstanarm)


nested_data_tbl <- data %>%
  extend_timeseries(
    .id_var        = id,
    .date_var      = date,
    .length_future = 180
  ) %>%
  
  nest_timeseries(
    .id_var        = id,
    .length_future = 180,
    .length_actual = 180*2
  ) %>%
  split_nested_timeseries(
    .length_test = 180
  )


##################################################

rec_prophet <- recipe(value ~ . , extract_nested_train_split(nested_data_tbl))  %>% 
  timetk::step_timeseries_signature(date) %>% 
  step_rm() %>%
  step_zv(all_predictors())

wflw_prophet <- workflow() %>%
  add_model(prophet_boost(seasonality_daily = T,
                          seasonality_weekly = F,
                          growth = 'linear',
                          seasonality_yearly = T,
                          learn_rate = 0.1,
                          mtry = 5,
                          trees = 10,
                          tree_depth = 850
  ) %>%
    set_engine("prophet_xgboost")
  ) %>%
  add_recipe(rec_prophet)



rec_arima <- recipe(value ~ . , 
                    extract_nested_train_split(nested_data_tbl))  %>% 
                    timetk::step_timeseries_signature(date) %>% 
                    step_rm() %>%
                    step_zv(all_predictors())

wflw_arima <- workflow() %>%
  add_model(arima_boost(learn_rate = 0.3,
                        mtry = 5,
                        trees = 12,
                        tree_depth = 850
  ) %>%
    set_engine("arima_xgboost")  
  ) %>%
  add_recipe(rec_arima)

rec_nnetar <- recipe(value ~ date , 
                     extract_nested_train_split(nested_data_tbl))  %>% 
  timetk::step_timeseries_signature(date) 

wflw_nnetar <- workflow() %>%
  add_model(nnetar_reg(epochs = 10,
                       num_networks =50) %>%
              set_engine("nnetar")
  ) %>% 
  add_recipe(rec_nnetar)


nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  
  # Add workflows
  wflw_prophet,
  wflw_arima,
  wflw_nnetar
)

nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  filter(id == 1) %>% 
  plot_modeltime_forecast(  )

nested_modeltime_tbl %>% 
  extract_nested_error_report()

nested_modeltime_refit_tbl <-   modeltime_nested_refit(object = nested_modeltime_tbl,
    control = control_nested_refit(verbose = TRUE)
  )


nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(id) %>%
  filter(id == 34 ) %>% 
  plot_modeltime_forecast(
  )
