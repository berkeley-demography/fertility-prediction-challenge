fit_model_targets <- list(
  #######################
  ## logistic model
  tar_target(
    logistic_fit_list,
    model_logistic(rec, df_train, folds)
  ),
  tar_target(
    save_logistic_fit,
    command = saveRDS(bundle(logistic_fit_list$fit),
                      file=here(prepped_data_dir, 'fit_logistic.rds')),
    format = 'file'
  ),
  #######################
  ## glmnet model
  tar_target(
    glmnet_fit_list,
    model_glmnet(rec, df_train, folds)
  ),
  tar_target(
    save_glmnet_fit,
    command = saveRDS(bundle(glmnet_fit_list$fit),
                      file=here(prepped_data_dir, 'fit_glmnet.rds')),
    format = 'file'
  ),
  #######################
  ## BART model
  tar_target(
    bart_fit_list,
    model_bart(rec, df_train, folds)
  ),
  tar_target(
    save_bart_fit,
    command = saveRDS(bundle(bart_fit_list$fit),
                      file=here(prepped_data_dir, 'fit_bart.rds')),
    format = 'file'
  ),
  #######################
  ## decision tree model
  tar_target(
    decision_tree_fit_list,
    model_decision_tree(rec, df_train, folds)
  ),
  tar_target(
    save_decision_tree_fit,
    command = saveRDS(bundle(decision_tree_fit_list$fit),
                      file=here(prepped_data_dir, 'fit_decision_tree.rds')),
    format = 'file'
  ),
  #######################
  ## xgboost model
  tar_target(
    xgboost_fit_list,
    model_xgboost(rec, df_train, folds)
  ),
  tar_target(
    save_xgboost_fit,
    command = saveRDS(bundle(xgboost_fit_list$fit),
                      file=here(prepped_data_dir, 'fit_xgboost.rds')),
    format = 'file'
  )    
)

####################################
## run model comparisons
compare_model_targets <- list(
  tar_target(
    model_list,
    list('logistic' = logistic_fit_list$fit,
         'glmnet' = glmnet_fit_list$fit,
         'bart' = bart_fit_list$fit,
         'decision tree' = decision_tree_fit_list$fit,
         'xgboost' = xgboost_fit_list$fit)
  ),
  tar_target(
    model_comparison_all,
    compare_models(model_list,
                   df_test,
                   df_train)
  ),
  tar_target(
    model_cv_metrics,
    cv_metrics(model_list)
  ),  
  tar_target(
    model_comparison_test,
    command = model_comparison_all %>% 
      filter(type == 'test') %>%
      arrange(desc(f1_score))    
  )
)
