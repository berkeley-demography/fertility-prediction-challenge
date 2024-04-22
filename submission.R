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

clean_df <- function(df, background_df = NULL){
  # Preprocess the input dataframe to feed the model.
  ### If no cleaning is done (e.g. if all the cleaning is done in a pipeline) leave only the "return df" command
  df <- df %>%
    # filter by whether outcome is available
    filter(outcome_available == 1)
  # Parameters:
  # df (dataframe): The input dataframe containing the raw data (e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background (dataframe): Optional input dataframe containing background data (e.g., from PreFer_train_background_data.csv or PreFer_fake_background_data.csv).

  # Returns:
  # data frame: The cleaned dataframe with only the necessary columns and processed variables.

  # identifying variables
  fixed_covars <- c(
    birthyear_bg = "Birth year of respondent; created and cleaned",
    gender_bg = "Gender of respondent; created and cleaned",
    # TODO - leave this out for now, b/c it is missing in some cases
    #migration_background_bg = "Migration background of the respondent; created and cleaned",
    age_bg = "The age of the respondent for each wave; created (not fixed, but records similar information as birthyear)"
  )
  
  varying_covars <- c(
    "partner",
    "woonvorm",
    "burgstat",
    "woning",
    "sted",
    "brutohh_f",
    "nettohh_f",
    "belbezig",
    "brutoink",
    "nettoink",
    "oplzon",
    "oplmet",
    "oplcat",
    "brutoink_f",
    "netinc",
    "nettoink_f"
  )
  
  covars <- c(
    nomem_encr = "Number of household member encrypted",
    nohouse_encr = "Number of household encrypted",
    wave = "Year and month of the field work period",
    positie = "Position within the household",
    lftdcat = "Age in CBS (Statistics Netherlands) categories",
    lftdhhh = "Age of the household head",
    aantalhh = "Number of household members",
    aantalki = "Number of living-at-home children in the household, children of the household head or his/her partner",
    partner = "The household head lives together with a partner (wedded or unwedded)",
    burgstat = "Civil status",
    woonvorm = "Domestic situation",
    woning = "Type of dwelling that the household inhabits",
    belbezig = "Primary occupation",
    brutoink = "Personal gross monthly income in Euros",
    nettoink = "Personal net monthly income in Euros (incl. nettocat)",
    brutocat = "Personal gross monthly income in categories",
    nettocat = "Personal net monthly income in categories",
    oplzon = "Highest level of education irrespective of diploma",
    oplmet = "Highest level of education with diploma",
    oplcat = "Level of education in CBS (Statistics Netherlands) categories",
    doetmee = "Household member participates in the panel",
    sted = "Urban character of place of residence",
    simpc = "Does the household have a simPC?",
    brutoink_f = "Personal gross monthly income in Euros, imputed",
    netinc = "Personal net monthly income in Euros",
    nettoink_f = "Personal net monthly income in Euros, imputed",
    brutohh_f = "Gross household income in Euros",
    nettohh_f = "Net household income in Euros",
    werving = "From which recruitment wave the household originates",
    birthyear_imp = "Year of birth [imputed by PreFer organisers] (based on original gebjaar variable)",
    gender_imp = "Gender [imputed by PreFer organisers] (based on original geslacht variable)",
    migration_background_imp = "Origin [imputed by PreFer organisers] (based on original herkomstgroep variable)",
    age_imp = "Age of the household member [imputed by PreFer organisers] (based on original leeftijd variable)"
  )

  # Filtering dataframe by selected variables 
  simple_df <- df %>% 
    select(contains(names(fixed_covars)), 
           contains(varying_covars), 
           contains(covars),
           nomem_encr) %>%
    # some of the vars from 2007 were giving us grief, so
    # we're not including them for now
    select(-contains('2007'))

  
  return(df)
}

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
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

  # Load the model
  model <- readRDS(model_path)
    
  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)

  # Exclude the variable nomem_encr if this variable is NOT in your model
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  
  # Generate predictions from model
  predictions <- predict(model, 
                         subset(df, select = vars_without_id), 
                         type = "class") 
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  #predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return( df_predict )
}
