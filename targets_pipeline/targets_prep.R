
background_vars <- function() {
  
  return(c(
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
  ))
  
}

fixed_predictors <- function() {
  
  
  return(c(
    "birthyear_bg",
    "gender_bg",
    "migration_background_bg",
    "age_bg"
  ))
  
}

fertility_intentions_predictors <- function() {
  return(c(
    "cf08a128",
    "cf09b128",
    "cf10c128",
    "cf11d128",
    "cf12e128",
    "cf13f128",
    "cf14g128",
    "cf15h128",
    "cf16i128",
    "cf17j128",
    "cf18k128",
    "cf19l128",
    "cf20m128",
    "cf08a129",
    "cf09b129",
    "cf10c129",
    "cf11d129",
    "cf12e129",
    "cf13f129",
    "cf14g129",
    "cf15h129",
    "cf16i129",
    "cf17j129",
    "cf18k129",
    "cf19l129",
    "cf20m129",
    "cf08a130",
    "cf09b130",
    "cf10c130",
    "cf11d130",
    "cf12e130",
    "cf13f130",
    "cf14g130",
    "cf15h130",
    "cf16i130",
    "cf17j130",
    "cf18k130",
    "cf19l130",
    "cf20m130"
  ))
} 

varying_predictors <- function() {
  
  # these covars have
  return(c(
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
  ))
  
}

get_raw_data <- function(raw_data_dir, hh_membership) {
  
  raw_df <- data.table::fread(here(raw_data_dir, 
                                     'training_data',
                                     'PreFer_train_data.csv'),
                                keepLeadingZeros = TRUE, # if FALSE adds zeroes to some dates
                                data.table = FALSE) # returns a data.frame object rather than data.table
  

  
  outcome_df <- data.table::fread(here(raw_data_dir, 
                                       'training_data', 
                                       'PreFer_train_outcome.csv'),
                                  keepLeadingZeros = TRUE,
                                  data.table = FALSE)
  
  presplit_df <- raw_df %>% 
    # only use data for which outcome is observed
    filter(outcome_available == 1) %>%
    # add the outcome
    left_join(outcome_df, by=c('nomem_encr')) %>%
    # and add hh membership
    left_join(hh_membership,
              by=c('nomem_encr')) 
  
  any_new_child_in_hh <- presplit_df %>%
    group_by(nohouse_encr) %>%
    summarize(any_new_child_in_hh = max(new_child))
  
  df_for_split <- presplit_df %>%
    left_join(any_new_child_in_hh, 
              by=c('nohouse_encr')) %>%
    mutate(new_child = factor(new_child))
  
  return(df_for_split)
}


get_background_data <- function(data_dir) {
  
  background_df <- data.table::fread(here(data_dir, 
                                          'other_data', 
                                          'PreFer_train_background_data.csv'),
                                     keepLeadingZeros = TRUE,
                                     data.table = FALSE)
  
  return(background_df)

}

get_hh_membership_data <- function(background_data) {
  
  ## grab the household ID - this is the only background variable we
  ## are using for now
  hh_membership <- background_data %>%
    # for each respondent
    group_by(nomem_encr) %>% 
    # pick the most recent wave
    filter(wave == max(wave)) %>%
    ungroup() %>%
    # most recent hh id 
    mutate(most_recent_hh = nohouse_encr) %>%
    select(nomem_encr, nohouse_encr)
  
  return(hh_membership)
  
}

select_relevant_cols <- function(data) {
  
  fp <- fixed_predictors()
  vp <- varying_predictors()
  fip <- fertility_intentions_predictors()
  
  return(data %>%
           select(gender_bg,
                  # fixed predictors
                  all_of(fp),
                  # varying predictors
                  # (which appear with _2008, _2009, etc)
                  contains(vp),
                  # fertility intentions predictors
                  contains(fip),
                  # individual id
                  nomem_encr,
                  # hh id
                  nohouse_encr,
                  # whether or not any new child in hh
                  any_new_child_in_hh,
                  # outcome
                  new_child))
}


get_split <- function(data, cur.seed, train.prop) {
  
  set.seed(cur.seed)
  

  
  ######################
  ## split the data
  df_split <- group_initial_split(data, 
                                    ##%>%
                                    ## only keep predictors,
                                    ## outcome,
                                    ## and the stratification var
                                    ##select(
                                    ##       # fixed predictors
                                    ##       # all_of(fp),
                                    ##       # varying predictors
                                    ##       # (which appear with _2008, _2009, etc)
                                    ##       contains(vp),
                                    ##       # individual id
                                    ##       nomem_encr,
                                    ##       # hh id
                                    ##       nohouse_encr,
                                    ##       # whether or not any new child in hh
                                    ##       any_new_child_in_hh,
                                    ##       # outcome
                                    ##       new_child), 
                                  group=nohouse_encr,
                                  prop=train.prop, 
                                  pool = 0,
                                  #strata=new_child)
                                  strata=any_new_child_in_hh)
  
  return(df_split)
}

get_folds <- function(train_data, cur.seed, num.folds=10) {
  
  set.seed(cur.seed)
  
  folds <- vfold_cv(train_data, v=num.folds)
  
  return(folds)
  
}
