
compare_models <- function(models, df_test, df_train) {
  
  res <- map2(models, names(models),
             ~ check_fit(.x, 
                         df_train=df_train,
                         df_test=df_test,
                         .y))  %>%
    bind_rows()
  
  return(res)

}

cv_metrics <- function(models) {
  
  res <- map2(models, names(models),
              function(model, name) {
                cv_metrics <- model$cv_metrics
                cv_metrics$model <- name
                return(cv_metrics)
              })  %>%
    bind_rows()
  
  return(res)

}