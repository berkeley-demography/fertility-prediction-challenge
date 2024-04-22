# This script will not be run on the holdout data, 
# but the resulting model model.joblib will be applied to the holdout data.
# 
# It is important to document your training steps here, including seed, 
# number of folds, model, et cetera

train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).

  # Combine cleaned_df and outcome_df
  combined_df <- cleaned_df %>%
    left_join(outcome_df, by=c('nomem_encr'))
  
  # set seed
  set.seed(101319)
  
  simple_df_split <- initial_split(simple_df, prop=0.8, strata=new_child)
  simple_df_train <- training(simple_df_split)
  simple_df_test <- testing(simple_df_split)
  
  # create recipe
  rec_obj <- recipe(new_child ~ ., data = simple_df_train) %>%
    # the ID variable is not a predictor
    remove_role(nomem_encr, old_role = 'predictor') %>%
    update_role(nomem_encr, new_role = "ID")  
  
  # impute missing
  imputed <- rec_obj %>%
    step_impute_knn(all_predictors())
  
  ind_vars <- imputed %>%
    step_dummy(all_nominal_predictors())
  
  # normalize
  standardized <- ind_vars %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors())
  
  trained_rec <- prep(standardized, training = simple_df_train)
  
  train_data <- bake(trained_rec, new_data = simple_df_train)
  test_data <- bake(trained_rec, new_data = simple_df_test)
  
  # set up XGBoost model
  boosted_trees_model <-
    boost_tree(trees=20) %>%
    set_engine('xgboost') %>%
    set_mode('classification')
  
  boosted_trees_fit <-
    boosted_trees_model %>%
    fit(new_child ~ ., 
        data = train_data  %>% 
          select(-nomem_encr))
  
  # Save the model
  saveRDS(boosted_trees_fit, "model.rds")
}
