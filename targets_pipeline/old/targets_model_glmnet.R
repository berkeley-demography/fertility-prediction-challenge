
model_glmnet <- function(rec, df_train, folds) {
  
  glmnet_model <- 
    #logistic_reg(penalty=double(1), mixture=double(1)) %>%
    logistic_reg(penalty=tune(), 
                 mixture=1) %>%
    set_engine('glmnet') %>%
    set_mode('classification')
  
  glmnet_workflow <-
    workflow() %>%
    add_model(glmnet_model) %>%
    add_recipe(rec)
  
  glmnet_tune_grid <- tibble(penalty = 10^seq(-2, -1, length.out = 10))
  
  glmnet_tune_res <- glmnet_workflow %>% 
    tune_grid(resamples = folds,
              grid = glmnet_tune_grid,
              control = control_grid(verbose = FALSE, save_pred = TRUE),
              metrics = metric_set(recall, precision, f_meas))
  
  glmnet_best_mod <- glmnet_tune_res %>% 
    select_best(metric = "f_meas")
  
  glmnet_fit <-
    finalize_workflow(glmnet_workflow, glmnet_best_mod) %>%
    fit(data = df_train)
  
  # assess CV performance
  glmnet_fit_cv_res <-
    glmnet_fit %>%
    fit_resamples(folds)
  
  glmnet_cv_performance <-
    collect_metrics(glmnet_fit_cv_res,
                    metrics = metric_set(recall, precision, f_meas))
  
  return(list(fit=glmnet_fit,
              tune=glmnet_tune_res,
              cv_metrics=glmnet_cv_performance))
  
}