# I think we might eventually want to increase the grid value
# to improve accuracy
get_grid_results <- function(aw, folds, grid=10, cores=8, seed=1503) {
  
  grid_ctrl <- control_grid(
    verbose=TRUE,
    #save_pred = TRUE,
    parallel_over = "everything"
    #save_workflow = TRUE
  )
  
  registerDoMC(cores = cores)
  
  res <- aw %>%
    workflow_map(
      seed = seed,
      resamples = folds,
      grid = grid,
      metrics = metric_set(accuracy, recall, precision, f_meas, roc_auc),
      control = grid_ctrl)
  
  registerDoSEQ()
  
  return(res)
}


# I think we might eventually want to increase the grid value
# to improve accuracy
get_race_results <- function(aw, folds, grid=10, cores=8, seed=1503) {
  
  race_ctrl <- control_race(
    verbose=TRUE,
    parallel_over = "everything",
    # these two are needed if we want to stack
    save_pred = TRUE,
    save_workflow = TRUE
  )
  
  #registerDoMC(cores = cores)
  
  res <- aw %>%
    workflow_map(
      fn = 'tune_race_anova',
      seed = seed,
      resamples = folds,
      grid = grid,
      metrics = metric_set(f_meas, accuracy, recall, precision, roc_auc),
      control = race_ctrl)
  
  #registerDoSEQ()
  
  return(res)
}
