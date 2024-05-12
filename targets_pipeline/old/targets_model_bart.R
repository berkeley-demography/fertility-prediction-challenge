
model_bart <- function(rec, df_train, folds) {
  
  bart_model <- 
    bart(trees = tune()) %>%
    set_engine('dbarts') %>%
    set_mode('classification')
  
  bart_workflow <-
    workflow() %>%
    add_model(bart_model) %>%
    add_recipe(rec)
  
  bart_tune_grid <- tibble(trees = c(50, 100, 150, 200, 250, 300))
  
  
  bart_tune_res <- bart_workflow %>% 
    tune_grid(resamples = folds,
              grid = bart_tune_grid,
              control = control_grid(verbose = FALSE, save_pred = TRUE),
              metrics = metric_set(recall, precision, f_meas))
  
  bart_metrics <- collect_metrics(bart_tune_res)
  
  bart_best_mod <- bart_tune_res %>%
    select_best(metric = "f_meas")
  
  bart_fit <-
    bart_workflow %>%
    finalize_workflow(bart_best_mod) %>%
    fit(data = df_train)
  
  # assess CV performance
  bart_fit_cv_res <-
    bart_fit %>%
    fit_resamples(folds)
  
  bart_cv_performance <-
    collect_metrics(bart_fit_cv_res,
                    metrics = metric_set(recall, precision, f_meas))
  
  return(list(fit=bart_fit,
              tune=bart_tune_res,
              cv_metrics=bart_cv_performance))
}