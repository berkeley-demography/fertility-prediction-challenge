# This is an example script to generate the outcome variable given the input dataset.
# 
# This script should be modified to prepare your own submission that predicts 
# the outcome for the benchmark challenge by changing the clean_df and predict_outcomes function.
# 
# The predict_outcomes function takes a data frame. The return value must
# be a data frame with two columns: nomem_encr and outcome. The nomem_encr column
# should contain the nomem_encr column from the input data frame. The outcome
# column should contain the predicted outcome for each nomem_encr. The outcome
# should be 0 (no child) or 1 (having a child).
# 
# clean_df should be used to clean (preprocess) the data.
# 
# run.R can be used to test your submission.

# List your packages here. Don't forget to update packages.R!
library(here)
library(tidyverse)
library(tidymodels)
library(bundle)

print("Starting script...")

clean_df <- function(df, background_df = NULL){
  # Preprocess the input dataframe to feed the model.
  
  # data are cleaned in the model pipeline, not here
  return(df)
}


predict_outcomes <- function(df, background_df = NULL, model_path = "./best_final_fit.rds"){
  # Generate predictions using the saved model and the input dataframe.
  
  # The predict_outcomes function accepts a dataframe as an argument
  # and returns a new dataframe with two columns: nomem_encr and
  # prediction. The nomem_encr column in the new dataframe replicates the
  # corresponding column from the input dataframe The prediction
  # column contains predictions for each corresponding nomem_encr. Each
  # prediction is represented as a binary value: '0' indicates that the
  # individual did not have a child during 2021-2023, while '1' implies that
  # they did.
  
  # Parameters:
  # df (dataframe): The data dataframe for which predictions are to be made.
  # background_df (dataframe): The background data dataframe for which predictions are to be made.
  # model_path (str): The path to the saved model file (which is the output of training.R).
  
  # Returns:
  # dataframe: A dataframe containing the identifiers and their corresponding predictions.
  
  ## This script contains a bare minimum working example
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }
  
  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)
  
  # Load the model
  model <- readRDS(model_path) %>% unbundle()

  print("Model loaded...")

  print(summary(model))

  print("Reading fake data (for debugging)...")
  #test_data <- read_csv("Prefer_fake_data.csv")
  test_data <- read_csv("/data/PreFer_fake_data.csv")
  print("... fake data read.")

  # this seems to be the same fake data that doesn't cause
  # errors locally
  #print("fake data, first row: ")
  #print(test_data[1,])

  print("pulling preprocessor from workflow")
  prepped_rec <- pull_workflow_preprocessor(model)

  print("running recipe from workflow")
  print(prepped_rec)

  print("predicting on fake data (for debugging)...")
  fake_pred <- predict(model, test_data)
  print("... predicted on fake data")

  print("predicting for real...")
  
  df_predict <- predict(model, df) %>%
    #bind_cols(df %>% select(nomem_encr)) %>%
    mutate(prediction = as.numeric(paste(.pred_class))) %>%
    select(prediction)
    #select(nomem_encr, prediction)

  print("Model predictions run...")

  ids <- df %>% select(nomem_encr)

  df_predict <- bind_cols(df_predict, ids)

  print("Saved predictions")
  ## Exclude the variable nomem_encr if this variable is NOT in your model
  #vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  #
  ## Generate predictions from model
  #predictions <- predict(model, 
  #                       subset(df, select = vars_without_id), 
  #                       type = "class") 
  #
  ## Create predictions that should be 0s and 1s rather than, e.g., probabilities
  #predictions <- ifelse(predictions > 0.5, 1, 0)  
  #
  ## Output file should be data.frame with two columns, nomem_encr and predictions
  #df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  ## Force columnnames (overrides names that may be given by `predict`)
  #names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return( df_predict )
}

print("Script complete")

