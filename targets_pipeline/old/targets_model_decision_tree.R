model_decision_tree <- function(rec, df_train, folds) {
  
  dt_model <-
    decision_tree() %>%
    set_engine('rpart') %>%
    set_mode('classification')
  
  dt_workflow <-
    workflow() %>%
    add_model(dt_model) %>%
    add_recipe(rec)
  
  decision_tree_fit <-
    dt_workflow %>%
    fit(data=df_train)
  
  # assess CV performance
  decision_tree_fit_cv_res <-
    decision_tree_fit %>%
    fit_resamples(folds)
  
  decision_tree_cv_performance <-
    collect_metrics(decision_tree_fit_cv_res,
                    metrics = metric_set(recall, precision, f_meas))
  
  return(list(fit=decision_tree_fit,
              cv_metrics=decision_tree_cv_performance))
  
}