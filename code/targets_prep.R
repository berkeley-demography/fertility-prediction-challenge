##########################################################################
##
## functions for opening and splitting data
##
##########################################################################

get_raw_data <- function(raw_data_dir, hh_membership) {
  
  #raw_df <- data.table::fread(here(raw_data_dir, 
  #                                   'training_data',
  #                                   'PreFer_train_data.csv'),
  #                              keepLeadingZeros = TRUE, # if FALSE adds zeroes to some dates
  #                              data.table = FALSE) # returns a data.frame object rather than data.table
  raw_df <- read_csv(here(raw_data_dir, 
                          'training_data',
                          'PreFer_train_data.csv'),
                     guess_max = 6418, col_types=cols(.default=col_double())) 

  
  outcome_df <- read_csv(here(raw_data_dir, 
                          'training_data',
                          'PreFer_train_outcome.csv'),
                     guess_max = 6418, col_types=cols(.default=col_double())) 
  #outcome_df <- data.table::fread(here(raw_data_dir, 
  #                                     'training_data', 
  #                                     'PreFer_train_outcome.csv'),
  #                                keepLeadingZeros = TRUE,
  #                                data.table = FALSE)
  
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
  
  background_df <- read_csv(here(data_dir, 
                          'other_data',
                          'PreFer_train_background_data.csv'),
                     guess_max = 1e5, col_types=cols(.default=col_double())) 
  #background_df <- data.table::fread(here(data_dir, 
  #                                        'other_data', 
  #                                        'PreFer_train_background_data.csv'),
  #                                   keepLeadingZeros = TRUE,
  #                                   data.table = FALSE)
  
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

get_folds <- function(train_data, cur.seed, num.folds=10, repeats=5) {
  
  set.seed(cur.seed)
  
  #folds <- vfold_cv(train_data, v=num.folds, repeats=repeats, pool = 0)
  folds <- group_vfold_cv(train_data, 
                          v=num.folds, 
                          group=nohouse_encr,
                          repeats=repeats, 
                          pool = 0)
  
  return(folds)
  
}
