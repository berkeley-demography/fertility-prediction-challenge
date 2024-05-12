
model_xgboost <- function(rec, df_train, folds) {
  
  xgboost_model <-
    #boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
    #    loss_reduction = tune(), sample_size = tune()) %>% 
    #boost_tree(trees = tune()) %>% 
    boost_tree(learn_rate = tune()) %>% 
    set_mode("classification") %>% 
    set_engine("xgboost")   
  #boost_tree(trees=20) %>%
  #set_engine('xgboost') %>%
  #set_mode('classification')
  
  xgboost_workflow <-
    workflow() %>%
    add_model(xgboost_model) %>%
    add_recipe(rec)
  
  set.seed(52097)
  
  xgboost_tuned <-
    tune_grid(xgboost_workflow, 
              resamples = folds,
              metrics = metric_set(recall, precision, f_meas))
  # let's see how auto grid works
  #grid = stop("add number of candidate points"))
  
  xgboost_best_mod <- xgboost_tuned %>% 
    select_best(metric = "f_meas")
  
  xgboost_fit <-
    finalize_workflow(xgboost_workflow, xgboost_best_mod) %>%
    fit(data = df_train)
  
  # assess CV performance
  xgboost_fit_cv_res <-
    xgboost_fit %>%
    fit_resamples(folds)
  
  xgboost_cv_performance <-
    collect_metrics(xgboost_fit_cv_res,
                    metrics = metric_set(recall, precision, f_meas)) 
  
  return(list(fit=xgboost_fit,
              tune=xgboost_tuned,
              cv_metrics=xgboost_cv_performance))
  
}