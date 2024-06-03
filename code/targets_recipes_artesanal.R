##########################################################################
##
## recipes based on artesanal, hand-crafted features
##
##########################################################################

source("targets_vars.R")
source("step_take_most_recent.R")

## this should preduce predictors:
##
##  - num_children
##  - future_num_kids
##  - timing_next_kid
##  - years_since_last_birth
##
artesanal_fertility_recodes <- function(rec) {
  
  fert_vars <- fert_predictors()
  
  rec <- rec %>%
    #########################################
    ## FERTILITY RECODES
    #########################################
    #
    # combine the two different versions of the 'did you ever have any children?' q
    # NB: don't assign any role here, because we're going to use this to derive
    # a predictor for num_children
    ### any_children 
    step_take_most_recent(c(fert_vars$everfert1, fert_vars$everfert2),
                          new_var = 'any_children') %>%
    # NB: don't assign any role here, because we're going to use this to derive
    # a predictor for num_children
    step_take_most_recent(c(fert_vars$quantum_fert1, fert_vars$quantum_fert2),
                          new_var = 'reported_num_children') %>%
    ### num_children
    step_mutate(num_children = case_when(any_children == 2 ~ 0,
                                         any_children == 1 ~ reported_num_children,
                                         TRUE ~ NA_integer_),
                role = 'predictor') %>%
    ### future_num_kids
    step_take_most_recent(fert_vars$fert_int,
                          new_var = 'most_recent_fertint') %>%
    step_take_most_recent(fert_vars$fert_num,
                          new_var = 'most_recent_fert_num') %>%
    step_mutate(future_num_kids = if_else(most_recent_fertint %in% c(2, 3), 
                                          0, 
                                          most_recent_fert_num),
                role = 'predictor') %>%
    ### rough estimate of timing of next child
    step_mutate(
      timing_next_kid = case_when(
        cf20m130 %in% c(0, 1) ~ 1,
        cf20m130 %in% c(2, 3) ~ 2,
        cf20m130 %in% c(4, 5) ~ 3,
        cf20m130 > 5 ~ 4,
        TRUE ~ NA_real_  # if none of the conditions match, assign NA
      )) %>%
    # Update timing_next_kid based on cf19l130 if cf20m130 is missing
    step_mutate(timing_next_kid = if_else(
      is.na(timing_next_kid) & !is.na(cf19l130),
      case_when(
        cf19l130 %in% c(0, 1, 2) ~ 1,
        cf19l130 %in% c(3, 4) ~ 2,
        cf19l130 %in% c(5) ~ 3,
        cf19l130 > 5 ~ 4,
        TRUE ~ NA_real_  # if none of the conditions match, assign NA
      ),
      timing_next_kid  # retain the existing value if cf20m130 is not missing
    )) %>%
    step_mutate(timing_next_kid = if_else(
      is.na(timing_next_kid) & !is.na(cf18k130),
      case_when(
        cf18k130 %in% c(0, 1, 2, 3) ~ 1,
        cf18k130 %in% c(4, 5) ~ 2,
        cf18k130 %in% c(6, 7) ~ 3,
        cf18k130 > 7 ~ 4,
        TRUE ~ NA_real_  # if none of the conditions match, assign NA
      ),
      timing_next_kid  # retain the existing value if cf20m130 or cf19l130 are not missing
    ))  %>%
    step_mutate(timing_next_kid = if_else(
      is.na(timing_next_kid) & !is.na(cf17j130),
      case_when(
        cf17j130 %in% c(2, 3, 4) ~ 1,
        cf17j130 %in% c(5, 6) ~ 2,
        cf17j130 %in% c(7, 8) ~ 3,
        cf17j130 > 8 ~ 4,
        TRUE ~ NA_real_  # if none of the conditions match, assign NA
      ),
      timing_next_kid  # retain the existing value if cf20m130 or cf19l130 are not missing
    )) %>%
    step_mutate(timing_next_kid = if_else(
      is.na(timing_next_kid) & !is.na(cf16i130),
      case_when(
        cf16i130 %in% c(2, 3, 4) ~ 1,
        cf16i130 %in% c(5, 6) ~ 2,
        cf16i130 %in% c(7, 8) ~ 3,
        cf16i130 > 8 ~ 4,
        TRUE ~ NA_real_  # if none of the conditions match, assign NA
      ),
      timing_next_kid  # retain the existing value if cf20m130 or cf19l130 are not missing
    )) %>%  
    ### make timing_next_kid a factor with an explicit missing level
    step_mutate(timing_next_kid = factor(timing_next_kid),
                timing_next_kid = fct_na_value_to_level(timing_next_kid, level='missing'),
                role='predictor') %>%
    #step_mutate(timing_next_kid = factor(timing_next_kid),
    #            role='predictor') %>%
    #step_mutate(timing_next_kid = fct_na_value_to_level(timing_next_kid, level='missing')) %>%
    #step_mutate(timing_next_kid = fct_na_value_to_level(factor(timing_next_kid), level='missing')) %>%
    #update_role(timing_next_kid, new_role='predictor') %>%
    ## Fertility Variable 4: Years since the birth of one's last child
    step_take_most_recent(fert_vars$ysb_p1, new_var="birthyr_firstborn") %>%
    step_take_most_recent(fert_vars$ysb_p2, new_var="birthyr_secondborn") %>%
    step_take_most_recent(fert_vars$ysb_p3, new_var="birthyr_thirdborn") %>%
    step_take_most_recent(fert_vars$ysb_p4, new_var="birthyr_fourthborn") %>%
    step_take_most_recent(fert_vars$ysb_p5, new_var="birthyr_fifthborn") %>%
    step_take_most_recent(fert_vars$ysb_p6, new_var="birthyr_sixthborn") %>%
    step_take_most_recent(fert_vars$ysb_p7, new_var="birthyr_seventhborn") %>%
    step_take_most_recent(fert_vars$ysb_p8, new_var="birthyr_eighthborn") %>%
    step_take_most_recent(fert_vars$ysb_p9, new_var="birthyr_ninthborn") %>%
    step_take_most_recent(fert_vars$ysb_p10, new_var="birthyr_tenthborn") %>%
    step_take_most_recent(fert_vars$ysb_p11, new_var="birthyr_eleventhborn") %>%
    step_take_most_recent(fert_vars$ysb_p12, new_var="birthyr_twelfthborn") %>%
    step_take_most_recent(fert_vars$ysb_p13, new_var="birthyr_thirteenthborn") %>%
    step_take_most_recent(fert_vars$ysb_p14, new_var="birthyr_fourteenthborn") %>%
    step_take_most_recent(fert_vars$ysb_p15, new_var="birthyr_fifteenthborn") %>% 
    step_mutate(
      most_recent_birth_year = pmax(
        birthyr_firstborn, birthyr_secondborn, birthyr_thirdborn, birthyr_fourthborn, birthyr_fifthborn,
        birthyr_sixthborn, birthyr_seventhborn, birthyr_eighthborn, birthyr_ninthborn, birthyr_tenthborn,
        birthyr_eleventhborn, birthyr_twelfthborn, birthyr_thirteenthborn, birthyr_fourteenthborn, birthyr_fifteenthborn,
        na.rm = TRUE
      ),
      role = NA_character_) %>%
    step_mutate(
      years_since_last_birth = if_else(is.na(most_recent_birth_year), NA_integer_, 2020 - most_recent_birth_year),
      # NB: dennis changed this a little b/c 
      # ... the var 'num_children' corresponds to Christina's variable 'new_parity' 
      # ... and the var 'any_children' corresponds to Christina's variable 'most_recent_evfert'
      years_since_last_birth = if_else(num_children == 0 | any_children == 2, -1, years_since_last_birth)
    ) %>%
    # for checking
    #step_mutate(yslb_raw = years_since_last_birth) %>% 
    # make this into a categorical var
    step_mutate(years_since_last_birth = cut(years_since_last_birth,
                                             breaks=c(-2, -1, 0, 1, 2, 3, 4, 5, 6, Inf)),
                years_since_last_birth =  fct_na_value_to_level(years_since_last_birth, level='missing'),
                role='predictor') 
  
  return(rec)
}

##
## this should produce predictors:
##
## - highest_edu
## - educ_satisfaction
## - educ_work_mismatch
##
artesanal_education_recodes <- function(rec) {
  
  educ_vars <- educ_predictors()
  
  rec <- rec %>%
    #########################################
    ## EDUCATION RECODES
    #########################################
    #
    # Highest level of education: highest_edu
    step_take_most_recent(educ_vars$highest_ed_vars, new_var = 'highest_edu_det') %>%
    #step_mutate(highest_edu = if_else(highest_edu_det %in% c(8, 9), 0, highest_edu_det),
    step_mutate(highest_edu = 
                  case_when(
                            highest_edu_det %in% c(8, 9) ~ 'none',
                            highest_edu_det %in% c(1,2,3) ~ 'up to hs',
                            highest_edu_det %in% c(4,5) ~ 'vocational',
                            highest_edu_det %in% c(6) ~ 'university',
                            highest_edu_det %in% c(7) ~ 'other'
                            ),
                highest_edu = factor(highest_edu),
                role='predictor') %>%
    #update_role(highest_edu, new_role = 'predictor') %>%
    # Adequate/Inadequate education for one's job: educ_work_mismatch
    step_take_most_recent(educ_vars$edu_satisfaction_vars, new_var = 'educ_satisfaction_raw') %>%
    step_mutate(educ_work_mismatch = 
                  case_when(
                    educ_satisfaction_raw == 1 ~ 0,
                    educ_satisfaction_raw == 2 ~ 1,
                    educ_satisfaction_raw %in% c(4, 6) ~ 2,
                    educ_satisfaction_raw %in% c(3, 5, 7) ~ 3,
                    TRUE ~ NA_real_  # If none of the conditions match, assign NA
                  )) %>%
    step_mutate(educ_work_mismatch = factor(educ_work_mismatch),
                educ_work_mismatch =  fct_na_value_to_level(educ_work_mismatch, level='missing'),
                role='predictor') %>%
    step_mutate(educ_satisfaction = factor(dplyr::ntile(educ_satisfaction_raw, 4)),
                educ_satisfaction = fct_na_value_to_level(educ_satisfaction, level='missing'),
                role='predictor') 
    #update_role(highest_edu, new_role = 'predictor') %>%
    #update_role(educ_satisfaction, new_role = 'predictor')
  
  return(rec)
}

##
## this should produce predictors:
##
## - current_domesticsit
## - childcare_split_score
## - infantcare_split_score
## - hhwork_split_score
##
artesanal_partnership_recodes <- function(rec) {
  
  split_vars <- split_predictors()
  misc_vars <- misc_predictors()
  
  rec <- rec %>%
    #########################################
  ## PARTNERSHIP RECODES
  #########################################
  #
  # Current domestic situation: current_domesticsit
  step_take_most_recent(misc_vars$cur_domestic_vars, 
                        new_var = 'current_domesticsit_raw') %>%
    step_mutate(current_domesticsit = factor(current_domesticsit_raw),
                role='predictor') %>%
    # A score measuring division of childcare: childcare_split_score 
    step_take_most_recent(split_vars$var_split_play, new_var='play') %>%
    step_take_most_recent(split_vars$var_split_fetching, new_var='fetching') %>%
    step_take_most_recent(split_vars$var_split_emosupport, new_var='emosupport') %>%
    step_take_most_recent(split_vars$var_split_outings, new_var='outings') %>%
    step_mutate(across(
      c("play", "fetching", "emosupport", "outings"),
      ~ case_when(
        . == 6 ~ 0,
        . == 5 ~ 1,
        . == 4 ~ 2,
        . == 3 ~ 3,
        . == 2 ~ 4,
        . == 1 ~ 5
      ))) %>%
    # TODO DEBUG MAKE PREDICTOR
    #step_mutate(childcare_split_score = rowSums(across(c("play", "fetching", "emosupport", "outings"))), role='predictor') %>%
    step_mutate(childcare_split_score = rowSums(across(c("play", "fetching", "emosupport", "outings")))) %>%
    # make this into a categorical var
    # for checking
    #step_mutate(css_raw = childcare_split_score) %>%
    step_discretize(childcare_split_score, num_breaks=3, min_unique=1) %>%
    step_mutate(childcare_split_score =  fct_na_value_to_level(childcare_split_score, level='missing'),
                role = 'predictor') %>%
    #  score measuring division of infant care: infantcare_split_score
    step_take_most_recent(split_vars$var_split_diapers, new_var = 'diapers') %>%
    step_take_most_recent(split_vars$var_split_bedtime, new_var = 'bedtime') %>%
    step_take_most_recent(split_vars$var_split_washing, new_var = 'washing') %>%
    step_take_most_recent(split_vars$var_split_doctor, new_var = 'doctor') %>%
    step_take_most_recent(split_vars$var_split_sick, new_var = 'sick') %>%
    step_mutate(across(
      c("diapers", "bedtime", "washing", "doctor", "sick"),
      ~ case_when(
        . == 8 ~ 0,
        . == 7 ~ 1,
        . == 6 ~ 2,
        . == 5 ~ 3,
        . == 4 ~ 4,
        . == 3 ~ 5,
        . == 2 ~ 6,
        . == 1 ~ 7,
      ))) %>%
    step_mutate(infantcare_split_score = rowSums(across(c("diapers", "bedtime", "washing", "doctor", "sick")))) %>%
    #step_mutate(iss_raw = childcare_split_score) %>%
    ## TODO DEBUG
    step_discretize(infantcare_split_score, num_breaks=3, min_unique = 1) %>%
    step_mutate(infantcare_split_score =  fct_na_value_to_level(infantcare_split_score, level='missing'),
                role = 'predictor') %>%
    # A score measuring division of household work: hhwork_split_score
    step_take_most_recent(split_vars$var_hhsplit_cook, new_var="hhworksplit_cook") %>%
    step_take_most_recent(split_vars$var_hhsplit_clothes, new_var="hhworksplit_clothes") %>%
    step_take_most_recent(split_vars$var_hhsplit_clean, new_var="hhworksplit_clean")  %>%
    step_take_most_recent(split_vars$var_hhsplit_other, new_var="hhworksplit_other") %>%
    step_take_most_recent(split_vars$var_hhsplit_fin, new_var="hhworksplit_fin") %>%
    step_take_most_recent(split_vars$var_hhsplit_shop, new_var="hhworksplit_shop") %>%
    step_mutate(across(
      c("hhworksplit_cook", "hhworksplit_clothes", "hhworksplit_clean", 
        "hhworksplit_other","hhworksplit_fin", "hhworksplit_shop"),
      ~ case_when(
        . == 9 ~ 0,
        . == 5 ~ 1,
        . == 4 ~ 2,
        . == 3 ~ 3,
        . == 2 ~ 4,
        . == 1 ~ 5,
      )
    )) %>%
    step_mutate(hhwork_split_score = rowSums(across(c("hhworksplit_cook", "hhworksplit_clothes", "hhworksplit_clean", 
                                                      "hhworksplit_other","hhworksplit_fin", "hhworksplit_shop"))))  %>%
    #step_mutate(hss_raw = childcare_split_score) %>%
    step_discretize(hhwork_split_score, num_breaks=3, min_unique=1) %>%
    step_mutate(hhwork_split_score =  fct_na_value_to_level(hhwork_split_score, level='missing'),
                role = 'predictor')
  
  return(rec)
  
}

##
## this should produce predictors:
##
## - current_domesticsit
## - childcare_split_score
## - infantcare_split_score
## - hhwork_split_score
##
artesanal_housing_recodes <- function(rec) {
  
  misc_vars <- misc_predictors()
  
  rec <- rec %>%
    #########################################
    ## HOUSING RECODES
    #########################################
    #
    # place of residence: urbanicity
    step_take_most_recent(misc_vars$var_urbanicity, 
                          new_var = 'urbanicity', 
                          role='predictor') %>%
    step_mutate(urbanicity = factor(urbanicity),
                urbanicity = fct_na_value_to_level(urbanicity, level='missing')) %>%
    # place of residence: housing
    step_take_most_recent(misc_vars$var_housing, 
                          new_var = 'housing', 
                          role='predictor') %>%
    step_mutate(housing = factor(housing),
                housing = fct_na_value_to_level(housing, level='missing'))
  
  return(rec)
}

##
## this should produce predictors:
##
## - ownincome
## - hhincome
##
artesanal_income_recodes <- function(rec) {
  
  misc_vars <- misc_predictors()
  
  rec <- rec %>%
    #########################################
    ## INCOME RECODES
    #########################################
    #
    # NOTE: THESE SHOULD BE IMPROVED!
    # Personal gross monthly income in Euros, imputed
    step_take_most_recent(misc_vars$var_ownincome, 
                          new_var = 'ownincome', 
                          role='predictor') %>%
    step_take_most_recent(misc_vars$var_hhincome, 
                          new_var = 'hhincome', 
                          role='predictor') 
  
  return(rec)
}

##
## this should produce predictors:
##
## - birthyear_bg
## - gender_bg
## - age_bg
## - migration_background_bg
##
artesanal_basic_recodes <- function(rec) {
  
  rec <- rec %>%
    step_mutate(migration_background_bg = factor(migration_background_bg),
                migration_background_bg = fct_na_value_to_level(migration_background_bg, level='missing')) %>%
    step_mutate(gender_bg = fct_na_value_to_level(factor(gender_bg), level='missing')) %>%
    update_role(
      birthyear_bg,
      gender_bg,
      age_bg,
      migration_background_bg,
      new_role = 'predictor')
  
  return(rec)
}

################################################
## make_recipe_artesanal
## 
## this recipe is going to try the hand-crafted vars 
## 
## add fertility intentions + some related info
make_recipe_artesanal <- function(data) {
  
  ## NB: can't update the role of mutated variables - see here:
  ## https://stackoverflow.com/questions/75128329/cant-update-role-of-mutated-variables
  
  # not using the formula helps avoid overflow problems; see
  # https://github.com/tidymodels/recipes/issues/467
  rec <- data %>%
    recipe() %>%
    ## the outcome
    update_role(new_child, new_role = 'outcome') %>%
    ## the ID
    update_role(nomem_encr, new_role = "ID") %>%
    ## not needed
    update_role(nohouse_encr, new_role = "stratification") %>%
    update_role(any_new_child_in_hh, new_role = "stratification") %>%
    update_role_requirements(role = 'stratification', bake=FALSE) %>%
    # these recode variables and set appropriate columns
    # to the role 'predictor'
    artesanal_basic_recodes() %>%
    artesanal_fertility_recodes() %>%
    artesanal_education_recodes() %>%
    artesanal_partnership_recodes() %>%
    artesanal_housing_recodes() %>%
    artesanal_income_recodes() %>%
    step_zv(all_predictors()) %>%
    # turn all categorical predictors into dummies
    step_dummy(all_nominal_predictors()) %>%
    # remove zero variance predictors
    #
    #########################################
    ## Impute missing values from some predictors 
    ##
    ## NB: could play with whether this comes before or after selecting
    ## only the predictors we actually use
    #########################################
    #
    #step_impute_knn(ownincome,
    #                hhincome,
    #                any_children,
    #                num_children,
    #                future_num_kids)
    step_impute_knn(all_predictors())
    # TODO - SEVERAL OF THESE ARE CATEGORICAL?
    # TODO - NOT CENTERING/SCALING
  
  return(rec)
}


