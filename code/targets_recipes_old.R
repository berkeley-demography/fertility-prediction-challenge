##########################################################################
##
## recipes based on throwing lots of stuff into the model 
## (minimal hand-crafting)
##
##########################################################################

source("targets_vars.R")

## TODO - LEFT OFF HERE: need to have this set the old predictors
## correctly

make_orig_vars_predictors <- function(rec) {
  
  fp <- fixed_predictors()
  vp <- varying_predictors()
  fip <- fertility_intentions_predictors()
  
  rec <- rec %>%
    update_role(all_of(unlist(fp, use.names=FALSE)),
                contains(unlist(vp, use.names=FALSE)),
                contains(unlist(fip, use.names=FALSE)))
  
  return(rec)
}

################################################
## v3
## this recipe hopefully improves some of the previous preparation
## 
## add fertility intentions + some related info
make_recipe_v3 <- function(data) {
  
  # not using the formula helps avoid overflow problems; see
  # https://github.com/tidymodels/recipes/issues/467
  rec <- data %>%
    # FILTER OUT UNNEEDED COLUMNS
    select(-any_of(c('any_new_child_in_hh', 'nohouse_encr'))) %>%    
    recipe() %>%
    ## by default, everything is a predictor
    #update_role(everything()) %>%
    make_orig_vars_predictors() %>%
    ## EXCEPT...
    ## ... the outcome
    update_role(new_child, new_role = 'outcome') %>%
    ## ... the ID
    update_role(nomem_encr, new_role = "ID") %>%
    ## get rid of any new child in hh and hh ID
    #step_rm(any_new_child_in_hh, skip=TRUE) %>%
    #step_rm(nohouse_encr, skip=TRUE) %>%
    #step_select(gender_bg,
    #            # fixed predictors
    #            all_of(fp),
    #            # other cols
    #            #all_of(other_cols),
    #            # varying predictors
    #            # (which appear with _2008, _2009, etc)
    #            contains(vp),
    #            # fertility intentions predictors
    #            contains(fip),
    #            # individual id
    #            nomem_encr,
    #            # hh id
    #            nohouse_encr,
    #            # whether or not any new child in hh
    #            any_of(any_new_child_in_hh),
    #            # outcome
    #            any_of(new_child))) %>%
    ## 
    # replace NA values for
    # how many children do you think you will have in the future?
    # with 0
    # TODO - this could be improved by comparing to response to 'will you have any' q
    step_mutate_at(matches('.....129'), fn = function(x) ifelse(is.na(x), 0, x)) %>%
    ######################################
  ## missing data
  ######################################
  # remove zero variance predictors
  step_zv(all_predictors()) %>%  
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
               -any_of('sted_2007')) %>%
    # remove zero variance predictors
    step_zv(all_predictors())
  
  return(rec)
}

################################################
## v2
## this recipe hopefully improves some of the previous preparation

make_recipe_v2 <- function(data) {
  
  # not using the formula helps avoid overflow problems; see
  # https://github.com/tidymodels/recipes/issues/467
  rec <- data %>%
    # FILTER OUT UNNEEDED COLUMNS
    select(-any_of(c('any_new_child_in_hh', 'nohouse_encr'))) %>%    
    recipe() %>%  
    make_orig_vars_predictors() %>%
    ## EXCEPT...
    ## ... the outcome
    update_role(new_child, new_role = 'outcome') %>%
    ## ... the ID
    update_role(nomem_encr, new_role = "ID") %>%
    #step_select(gender_bg,
    #            # fixed predictors
    #            all_of(fp),
    #            # other cols
    #            #all_of(other_cols),
    #            # varying predictors
    #            # (which appear with _2008, _2009, etc)
    #            contains(vp),
    #            # fertility intentions predictors
    #            contains(fip),
    #            # individual id
    #            nomem_encr,
    #            # hh id
    #            nohouse_encr,
    #            # whether or not any new child in hh
    #            any_of(any_new_child_in_hh),
    #            # outcome
    #            any_of(new_child))) %>%    
    # remove zero variance predictors
    step_zv(all_predictors()) %>%
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
               -any_of('sted_2007')) %>%
    # remove zero variance predictors
    step_zv(all_predictors())
  
  return(rec)
}



################################################
## the original recipe
##
make_recipe <- function(data) {
  
  
  # not using the formula helps avoid overflow problems
  # see
  # https://github.com/tidymodels/recipes/issues/467
  #rec <- recipe(new_child ~ ., data=df_train) %>%
  
  rec <- data %>%
    # FILTER OUT UNNEEDED COLUMNS
    select(-any_of(c('any_new_child_in_hh', 'nohouse_encr'))) %>%    
    recipe() %>%  
    #rec <- recipe(data) %>%
    ## by default, everything is a predictor
    #update_role(everything()) %>%
    make_orig_vars_predictors() %>%
    ## EXCEPT...
    ## ... the outcome
    update_role(new_child, new_role = 'outcome') %>%
    ## ... the ID
    update_role(nomem_encr, new_role = "ID") %>%
    ## get rid fo any new child in hh and hh ID
    #step_rm(any_new_child_in_hh, skip=TRUE) %>%
    #step_rm(nohouse_encr, skip=TRUE) %>%
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
