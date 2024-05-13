
################################################
## v3
## this recipe hopefully improves some of the previous preparation
## 
## add fertility intentions + some related info
make_recipe_v3 <- function(df_train) {
  
  
  # not using the formula helps avoid overflow problems; see
  # https://github.com/tidymodels/recipes/issues/467
  rec <- recipe(df_train) %>%
    ## by default, everything is a predictor
    update_role(everything()) %>%
    ## EXCEPT...
    ## ... the outcome
    update_role(new_child, new_role = 'outcome') %>%
    ## ... the ID
    update_role(nomem_encr, nohouse_encr, new_role = "ID") %>%
    ## get rid fo any new child in hh and hh ID
    step_rm(any_new_child_in_hh) %>%
    step_rm(nohouse_encr) %>%
  ## 
  # replace NA values for
  # how many children do you think you will have in the future?
  # with 0
  # TODO - this could be improved by comparing to response to 'will you have any' q
  step_mutate_at(matches('.....129'), fn = function(x) ifelse(is.na(x), 0, x)) %>%
  ######################################
  ## missing data
  ######################################
  step_impute_knn(all_predictors()) %>%
  # convert relevants vars to factors
  step_num2factor(gender_bg,
                  levels=c("m", "f")) %>%
  #step_num2factor(contains('partner'),
  #                # needed because the lowest value is 0, 
  #                # which can't be used as an index
  #                transform = function(x) x + 1,
  #                levels=c("0", "1")) %>%
  ###################
  ## convert these to factors
  #########
  # these are 'do you think you will have [more] children in the future?'
  step_mutate_at(matches('.....128'), fn = factor) %>%
    
  step_mutate_at(contains('woonvorm'), fn = factor) %>%
  step_mutate_at(contains('woning'), fn = factor) %>%
  step_mutate_at(contains('sted'), fn = factor) %>%
  step_mutate_at(contains('oplzon'), fn = factor) %>%
  step_mutate_at(contains('oplmet'), fn = factor) %>%
  step_mutate_at(contains('oplcat'), fn = factor) %>%
  step_mutate_at(contains('burgstat'), fn = factor) %>%
  step_mutate_at(contains('belbezig'), fn = factor) %>%
  step_mutate_at(contains('migration_background_bg'), fn = factor) %>%
  step_novel(contains('woonvorm'),
             contains('woning'),
             contains('sted'), 
             contains('oplzon'),
             contains('oplmet'),
             contains('oplcat'),
             contains('burgstat'),
             contains('belbezig'), 
             contains('migration_background_bg')) %>%
  step_unknown(contains('woonvorm'),
               contains('woning'),
               contains('sted'), 
               contains('oplzon'),
               contains('oplmet'),
               contains('oplcat'),
               contains('burgstat'),
               contains('belbezig'), 
               contains('migration_background_bg')) %>%
  # this is the 'how many more children do you think you will have' q
  step_mutate_at(matches('.....129'), fn = as.numeric) %>%
  # center + scale most numeric predictors
  step_center(all_numeric_predictors(), 
              -contains('birthyear_bg'),
              -age_bg,
              -contains('partner'),
              -matches('.....129')) %>%
  step_scale(all_numeric_predictors(), 
             -contains('birthyear_bg'),
             -age_bg,
             -contains('partner'),
             -matches('.....129')) %>%
  # turn all categorical predictors into dummies
  step_dummy(all_nominal_predictors(), 
             -sted_2007) %>%
  # remove zero variance predictors
  step_zv(all_predictors())
  
  return(rec)
}

################################################
## v2
## this recipe hopefully improves some of the previous preparation

make_recipe_v2 <- function(df_train) {
  
  
  # not using the formula helps avoid overflow problems; see
  # https://github.com/tidymodels/recipes/issues/467
  rec <- recipe(df_train) %>%
    ## by default, everything is a predictor
    update_role(everything()) %>%
    ## EXCEPT...
    ## ... the outcome
    update_role(new_child, new_role = 'outcome') %>%
    ## ... the ID
    update_role(nomem_encr, nohouse_encr, new_role = "ID") %>%
    ## get rid fo any new child in hh and hh ID
    step_rm(any_new_child_in_hh) %>%
    step_rm(nohouse_encr) %>%
    ######################################
    ## missing data
    ######################################
    #
    # these gave me trouble b/c of cases where they were all missing
    #
    #step_impute_knn(contains('partner'),
    #                impute_with=imp_vars(contains('partner'))) %>%
    #step_impute_knn(contains('woonvorm'),
    #                impute_with=imp_vars(contains('woonvorm'))) %>%
    #step_impute_knn(contains('burgstat'),
    #                impute_with=imp_vars(contains('burgstat'))) %>%
    #step_impute_knn(contains('woning'),
    #                impute_with=imp_vars(contains('woning'))) %>%
    #step_impute_knn(contains('sted'),
    #                impute_with=imp_vars(contains('sted'))) %>%
    #step_impute_knn(contains('brutohh_f'),
    #                impute_with=imp_vars(contains('brutohh_f'))) %>%
    #step_impute_knn(contains('nettoink'),
    #                impute_with=imp_vars(contains('nettoink'))) %>%
    #step_impute_knn(contains('oplzon'),
    #                impute_with=imp_vars(contains('oplzon'))) %>%
    #step_impute_knn(contains('oplmet'),
    #                impute_with=imp_vars(contains('oplmet'))) %>%
    #step_impute_knn(contains('oplcat'),
    #                impute_with=imp_vars(contains('oplcat'))) %>%
    #step_impute_knn(contains('brutoink_f'),
    #                impute_with=imp_vars(contains('brutoink_f'))) %>%
    #step_impute_knn(contains('netinc'),
    #                impute_with=imp_vars(contains('netinc'))) %>%
    #step_impute_knn(contains('nettoink_f'),
    #                impute_with=imp_vars(contains('nettoink_f'))) %>%
    # use k nearest neighbors to impute missing predictors
    step_impute_knn(all_predictors()) %>%
    # convert relevants vars to factors
    step_num2factor(gender_bg,
                    levels=c("m", "f")) %>%
    #step_num2factor(contains('partner'),
    #                # needed because the lowest value is 0, 
    #                # which can't be used as an index
    #                transform = function(x) x + 1,
    #                levels=c("0", "1")) %>%
    ###################
    ## convert these to factors
    step_mutate_at(contains('woonvorm'), fn = factor) %>%
    step_mutate_at(contains('woning'), fn = factor) %>%
    step_mutate_at(contains('sted'), fn = factor) %>%
    step_mutate_at(contains('oplzon'), fn = factor) %>%
    step_mutate_at(contains('oplmet'), fn = factor) %>%
    step_mutate_at(contains('oplcat'), fn = factor) %>%
    step_mutate_at(contains('burgstat'), fn = factor) %>%
    step_mutate_at(contains('belbezig'), fn = factor) %>%
    step_mutate_at(contains('migration_background_bg'), fn = factor) %>%
    step_novel(contains('woonvorm'),
                 contains('woning'),
                 contains('sted'), 
                 contains('oplzon'),
                 contains('oplmet'),
                 contains('oplcat'),
                 contains('burgstat'),
                 contains('belbezig'), 
                 contains('migration_background_bg')) %>%
    step_unknown(contains('woonvorm'),
                 contains('woning'),
                 contains('sted'), 
                 contains('oplzon'),
                 contains('oplmet'),
                 contains('oplcat'),
                 contains('burgstat'),
                 contains('belbezig'), 
                 contains('migration_background_bg')) %>%
    #step_num2factor(contains('woonvorm'),
    #                levels=paste(1:5)) %>%
    #step_num2factor(contains('woning'),
    #                levels=paste(c(1:4,9))) %>%
    #step_num2factor(contains('sted'),
    #                levels=paste(1:5)) %>%
    #step_num2factor(contains('oplzon'),
    #                levels=paste(1:9)) %>%
    #step_num2factor(contains('oplmet'),
    #                levels=paste(1:9)) %>%
    #step_num2factor(contains('oplcat'),
    #                levels=paste(1:6)) %>%
    #step_num2factor(contains('burgstat'),
    #                levels=paste(1:5)) %>%
    #step_num2factor(contains('belbezig'),
    #                levels=paste(1:14)) %>%
    #step_num2factor(migration_background_bg,
    #                # needed because the lowest value is 0, 
    #                # which can't be used as an index
    #                transform = function(x) x + 1,
    #                levels=paste(c(0, 101, 102, 201, 202, 999))) %>%
    # center + scale most numeric predictors
    step_center(all_numeric_predictors(), 
                -contains('birthyear_bg'),
                -age_bg,
                -contains('partner')) %>%
    step_scale(all_numeric_predictors(), 
               -contains('birthyear_bg'),
               -age_bg,
               -contains('partner')) %>%
    # turn all categorical predictors into dummies
    step_dummy(all_nominal_predictors(), 
               -sted_2007) %>%
    # remove zero variance predictors
    step_zv(all_predictors())
  
  return(rec)
}



################################################
## the original recipe
##
make_recipe <- function(df_train) {
  
  
  # not using the formula helps avoid overflow problems
  # see
  # https://github.com/tidymodels/recipes/issues/467
  #rec <- recipe(new_child ~ ., data=df_train) %>%
  
  rec <- recipe(df_train) %>%
    ## by default, everything is a predictor
    update_role(everything()) %>%
    ## EXCEPT...
    ## ... the outcome
    update_role(new_child, new_role = 'outcome') %>%
    ## ... the ID
    update_role(nomem_encr, nohouse_encr, new_role = "ID") %>%
    ## get rid fo any new child in hh and hh ID
    step_rm(any_new_child_in_hh) %>%
    step_rm(nohouse_encr) %>%
    # use k nearest neighbors to impute missing predictors
    step_impute_knn(all_predictors()) %>%
    # convert new_child to factor
    #step_num2factor(new_child,
    #                levels=paste(0:1)) %>%
    # convert gender to a factor
    step_num2factor(gender_bg,
                    levels=c("m", "f")) %>%
    #step_num2factor(contains('woonvorm'),
    #                levels=paste(1:5)) %>%
    #step_num2factor(contains('partner'),
    #                levels=paste(0:1)) %>%
    # turn all categorical predictors into dummies
    step_dummy(all_nominal_predictors()) %>%
    # center + scale most numeric predictors
    # EXCEPT
    step_center(all_numeric_predictors(), 
                -contains('birthyear_bg')) %>%
    step_scale(all_numeric_predictors(), 
               -contains('birthyear_bg')) %>%
    # center + scale all numeric predictors
    #step_center(all_numeric_predictors()) %>%
    #step_scale(all_numeric_predictors())
    # remove zero variance predictors
    step_zv(all_predictors())
  
  return(rec)
}


