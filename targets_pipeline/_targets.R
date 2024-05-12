# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(future)
library(quarto)
library(rlang)
library(doMC)

# Set target options:
tar_option_set(
  packages = c("here",
               "tidyverse",
               "tidymodels",
               "bundle",
               "future",
               "parallel",
               "doMC",
               "baguette",
               "finetune"
               ) # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #controller = crew::crew_controller_local(workers = 8)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multicore")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source('helper.R')
tar_source('targets_prep.R')
tar_source('targets_recipe.R')
tar_source('targets_workflowset.R')

#########################
## Targets related to prepping data
## (opening files up + making train/test split)
prep_data_targets <- list(
  tar_target(
    raw_data_dir,
    here("data")
  ),
  tar_target(
    prepped_data_dir,
    here("data-prepped")
  ), 
  # open up raw background data
  tar_target(
    background_data,
    get_background_data(raw_data_dir)
  ),
  # get hh membership from background data
  tar_target(
    hh_membership,
    get_hh_membership_data(background_data)
  ),
  # open up raw data + join in hh membership
  # (we need hh membership for train/test split)
  tar_target(
    raw_data,
    get_raw_data(raw_data_dir, hh_membership)
  ),
  # make train/test split
  tar_target(
    split_data,
    get_split(raw_data %>% select_relevant_cols(), 
              cur.seed = 10132019, 
              train.prop=0.8)
    # if this is cached, never need to re-run it
    # (trying to minimize the number of times we run it to avoid
    # risk of leakage)
    #cue = tar_cue("never")
  ),
  ## the test set
  tar_target(
    df_test,
    testing(split_data) %>%
      # let's be extra sure these are not in the training set!
      select(-any_of(c('any_new_child_in_hh', 'new_child')))
  ),
  ## the training set
  tar_target(
    df_train, 
    training(split_data)
  ),
  ## and cross-validation folds from the training set
  tar_target(
    folds,
    get_folds(df_train, cur.seed = 10132019, num.folds=10)
  ),
  ## save some files
  tar_target(
    save_df_test,
    command = saveRDS(df_test,
                      file.path(prepped_data_dir, 'test_split.rds')),
    format='file'),
  tar_target(
    save_df_train,
    command = saveRDS(df_train,
                      file.path(prepped_data_dir, 'train_split.rds')),
    format='file')
)


#############################
## targets for making recipe
recipe_targets <- list(
  ###############
  ## first recipe
  tar_target(
    rec,
    make_recipe(df_train)
  ),
  #tar_target(
  #  save_recipe,
  #  command = saveRDS(bundle(rec),
  #                    file=here(prepped_data_dir, 'recipe.rds')),
  #  format = 'file'
  #),
  ###############
  ## recipe v2
  tar_target(
    rec_v2,
    make_recipe_v2(df_train)
  ),
  #tar_target(
  #  save_recipe_v2,
  #  command = saveRDS(bundle(rec_v2),
  #                    file=here(prepped_data_dir, 'recipe_v2.rds')),
  #  format = 'file'
  #),
  #tar_target(
  #  test_v2,
  #  rec_v2 %>%
  #    prep() %>%
  #    bake(df_train)
  #)
  ###############
  ## recipe v3
  tar_target(
    rec_v3,
    make_recipe_v3(df_train)
  )
)


####################
## model defns
model_spec_targets <- list(
  tar_target(
    spec_logistic, 
    logistic_reg() %>% 
      set_engine('glm') %>% 
      set_mode('classification')
  ),
  tar_target(
    spec_glmnet,
    logistic_reg(penalty=tune(), 
                 mixture=tune()) %>%
      set_engine('glmnet') %>%
      set_mode('classification')
  ),
  tar_target(
    spec_bart,
    bart(trees = tune()) %>%
      set_engine('dbarts') %>%
      set_mode('classification')
  ),
  tar_target(
    spec_decision_tree,
    decision_tree(tree_depth=tune(),
                  min_n = tune(),
                  cost_complexity = tune()) %>%
      set_engine('rpart') %>%
      set_mode('classification')
  ),
  tar_target(
    spec_xgboost,
      boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
          loss_reduction = tune(), sample_size = tune()) %>% 
      set_mode("classification") %>% 
      set_engine("xgboost")   
  ),
  ## 
  ## would be interesting to try this out, but it was very slow in
  ## early tests
  tar_target(
    spec_bag_mars,
    bag_mars(num_terms = tune(), 
             prod_degree = tune(), 
             prune_method = 'backward') %>% 
      set_engine("earth", nfold=10) %>% 
      set_mode("classification")
      #set_engine("earth", nfold=10) %>% 
      #set_mode("classification", prune_method='cv')
  ),
  tar_target(
    spec_rf,
    rand_forest(mode = "classification",
                #trees = tune(),
                mtry=tune(),
                min_n=tune()) %>%
      set_engine('ranger',
                 importance='impurity')
  )
)


#############################
## targets for workflow sets
## (groups of models)
workflow_set_targets <- list(
  # uncomment to test bag_mars
  #tar_target(
  #  test_bag_mars,
  #  workflow() %>%
  #    add_model(spec_bag_mars) %>%
  #    add_recipe(rec_v2) %>%
  #    #fit(data = df_train)
  #    #fit_resamples(data = folds)
  #    tune_grid(resamples = folds)
  #),
  # uncomment to test the new recipe w/ model fitting
  #tar_target(
  #  test_fit_with_v2,
  #  workflow() %>%
  #    add_model(spec_logistic) %>%
  #    add_recipe(rec_v2) %>%
  #    fit(data = df_train)
  #),
  tar_target(
    basic_workflow_set,
    workflow_set(
      ## RECIPES TO TRY
      preproc = 
        list(
          v3 = rec_v3,
          v2 = rec_v2,
          basic=rec
        ),
      ## MODELS TO TRY
      models =  
        list(
          #logistic = spec_logistic,
          #bag_mars = spec_bag_mars,
          glmnet = spec_glmnet,
          bart = spec_bart,
          decision_tree = spec_decision_tree,
          xgboost = spec_xgboost,
          rf = spec_rf
        )
    )
  ),
  tar_target(
    all_workflows,
    bind_rows(
      # NB: eventually, this could contain other workflows too
      # (eg, if we had different recipes to prepare the data differently
      #  for different models)
      basic_workflow_set
      #v2_workflow_set  # eventually, could add this
    )
  ),
  # this tunes all the models using grid search
  #tar_target(
  #  tune_results,
  #  get_grid_results(all_workflows, folds)
  #),
  # this tunes all the models using a hopefully faster but approximate
  # race method
  tar_target(
    tune_results,
    get_race_results(all_workflows, folds)
  ),
  # get the best result
  tar_target(
    ranked_results,
    tune_results %>%
      rank_results("f_meas") %>%
      filter(.metric == 'f_meas')
  ),
  tar_target(
    best_result,
    tune_results %>% 
      extract_workflow_set_result(id=paste(ranked_results$wflow_id[1])) %>%
      select_best(metric = "f_meas") 
  ),
  tar_target(
    best_final_fit,
    tune_results %>%
      extract_workflow(paste(ranked_results$wflow_id[1])) %>%
      finalize_workflow(best_result) %>%
      last_fit(split_data,
               metrics=metric_set(accuracy, recall, precision, f_meas)
               )
  )
)

#################################
## make a notebook with some results when
## the pipeline is finished
notebook_targets <- list(
  tar_quarto(results_nb, "results.qmd")
  # this is not quite ready
  #tar_quarto(explainer_nb, "model_explainer.qmd")
)


list(
  prep_data_targets,
  model_spec_targets,
  recipe_targets,
  workflow_set_targets,
  notebook_targets
)








