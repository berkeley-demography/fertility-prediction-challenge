
tar_load(rec_v2)
tar_load(rec)
tar_load(spec_decision_tree)
tar_load(spec_glmnet)
tar_load(folds)
tar_load(df_train)

tmp <- rec %>%
  prep() %>%
  bake(df_train)

ws1 <- workflow_set(preproc=list(basic = rec),
                    #models = list(decision_tree = spec_decision_tree))
                    models = list(glmnet = spec_glmnet))
ws2 <- workflow_set(preproc=list(v2 = rec_v2),
                    #models = list(decision_tree = spec_decision_tree))
                    models = list(glmnet = spec_glmnet))

tmp_grid_ctrl <- control_grid(
  verbose=TRUE,
  #save_pred = TRUE,
  #parallel_over = "everything"
  #save_workflow = TRUE
)

#future::plan(multisession, workers=10)
#registerDoMC(cores = 4)

res1 <- ws1 %>%
  workflow_map(
    seed = 1503,
    resamples = folds,
    # TODO - I think we might eventually want to increase this
    # to improve accuracy
    grid = 10,
    metrics = metric_set(accuracy, recall, precision, f_meas),
    control = tmp_grid_ctrl)

res2 <- ws2 %>%
  workflow_map(
    seed = 1503,
    resamples = folds,
    # TODO - I think we might eventually want to increase this
    # to improve accuracy
    grid = 10,
    metrics = metric_set(accuracy, recall, precision, f_meas),
    control = tmp_grid_ctrl)



tmp <-
  workflow() %>%
  #add_model(spec_decision_tree) %>%
  add_model(spec_glmnet) %>%
  #add_recipe(rec_v2) %>%
  add_recipe(rec) %>%
  fit(df_train)
