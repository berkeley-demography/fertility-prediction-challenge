##########################################################################
##
## custom recipe step that takes a bunch of column names, assumed to be
## ordered in time, and takes value from the most recent of them
##
##########################################################################

##
## the original helper function, which was turned into this recipe step
##
## helper function to take most recent in a series of vars
## ordered chronologically
#take_most_recent <- function(df, selector, new_name) {
#  df <- df %>%
#    rowwise() %>%
#    mutate(!!new_name := last(na.omit(c_across({{selector}})))) %>%
#    ungroup()
#  
#  return(df)
#}


###########################################################################
## recipe step take_most_recent
##
## these functions define a recipe step that takes two arguments:
##  - a vector of column names (measurements of the same thing over time, in chronological order)
##  - a name for a new variable
##
## it then takes the most recent observed value for each row

## NOTE that you need a skip argument here, though this is not documented
## see:
## https://forum.posit.co/t/custom-recipes-step-fails-during-bake/176576/2

step_take_most_recent <- function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    new_var,
    # column names from which to take most recent; not determined
    # until prep() is called
    col_names = NULL) {
  
  add_step(
    recipe,
    step_take_most_recent_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      skip = skip,
      new_var,
      col_names = col_names)
    
  )
}

step_take_most_recent_new <-
  function(terms, role, trained, skip, new_var, col_names) {
    step(
      subclass = 'most_recent',
      terms = terms, 
      trained = trained,
      skip = skip,
      role = role,
      new_var = new_var,
      col_names = col_names
    )
  }

prep.step_most_recent <- function(x, training, info=NULL, ...) {
  
  col_names <- recipes_eval_select(x$terms, training, info)
  
  step_take_most_recent_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    new_var = x$new_var,
    col_names = col_names
  )
  
}


bake.step_most_recent <- function(object, new_data, ...) {
  
  col_names <- setNames(object$col_names, NULL)
  new_var <- object$new_var
  
  tmp <- new_data %>%
    rowwise() %>%
    mutate(.new = last(na.omit(c_across(all_of(col_names))))) %>%
    ungroup()
  
  new_data[[new_var]] <- tmp$.new
  
  return(new_data)
}
