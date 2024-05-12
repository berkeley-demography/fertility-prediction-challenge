
model_logistic <- function(rec, df_train, folds) {

  logistic_model <- 
    logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')
  
  logistic_workflow <- 
    workflow() %>%
    add_model(logistic_model) %>%
    add_recipe(rec) 
  
  logistic_fit <-
    logistic_workflow %>%
    fit(data = df_train)
  
  # assess CV performance
  logistic_fit_cv_res <-
    logistic_fit %>%
    fit_resamples(folds)
  
  logistic_cv_performance <-
    collect_metrics(logistic_fit_cv_res,
                    metrics = metric_set(recall, precision, f_meas))
  
  return(list(fit=logistic_fit,
              cv_metrics=logistic_cv_performance))

}