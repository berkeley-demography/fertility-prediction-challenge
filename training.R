# This script will not be run on the holdout data, 
# but the resulting model model.joblib will be applied to the holdout data.
# 
# It is important to document your training steps here, including seed, 
# number of folds, model, et cetera

train_save_model <- function(model_path = "best_final_fit.rds") {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).
  
  # prepping our final fit model to be replace model.rds
  model <- readRDS(model_path) %>% unbundle()
  
  # Save the model
  saveRDS(model, "model.rds")
}
