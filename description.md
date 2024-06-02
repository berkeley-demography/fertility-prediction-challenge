# Description of submission

### Summary

Our team used the \texttt{tidymodels} package in R paired with the \texttt{targets} pipeline software to automate our model selection procedure. We tested several different modeling approaches (random forest, BART, XGBoost, MARS, logistic regression, etc.) and a variety of feature combinations. We preprocessed the predictor variables using the "recipes" framework within \texttt{tidymodels}. To tune model hyperparameters, we used 10 fold cross-validation with grid search, repeated 5 times. We then ranked the performance of each model using the average F1 score of the cross-validation folds and evaluated models with the highest scores on our held-out test set. The model we chose to submit is one of our best performing models, a random forest with hand-selected and carefully prepared variables.

### Variables

One of the benefits of using a \texttt{tidymodels} pipeline for model testing is that we were able to evaluate model performance using a variety of feature combinations. Two of our best performing sets of variables were 1) a large set of indiscriminate features chosen with minimal deliberation (around 250) and 2) a smaller set of theory-based, hand-selected variables (around 60). The hand-crafted set included questions about prior fertility and future intentions, education, partnership, housing, income, migration, gender, and age and mostly take values from the most recent survey wave. These features were selected on the basis of their established relevance to fertility as documented in the demography literature.  

Survey questions about fertility intentions (when planning to have more kids, how many more children wanted, etc.) tended to dominate feature importance plots for both sets of variables. Other influential variables in the hand-crafted set included age, income, domestic situation, number of children, and housing.

### Preprocessing Steps 

We prepared variables using \texttt{tidymodels} "recipes", which build a series of preprocessing steps that make training data on many different models easier. For the larger set of variables, we mostly treated all features the same way; we removed all zero variance predictors, imputed missing values using k-nearest neighbor imputation, converted all categorical features to factor variables, and centered and scaled most numeric predictors. In the hand-crafted recipe on the other hand, we were more intentional which each variable and started by taking data from the most recent year only. We then adjusted and cleaned features to ensure they contained as much useful information as possible. These cleaning steps included converting many categorical features to binary dummy variables, creating new variables to represent information from multiple questions more succinctly, and ensuring that all variables will be processed correctly by the model. Finally, we either created a missing category for dummy variables or imputed missing values using k-nearest neighbors imputation.

### Model 

We fit and tuned five types of models--random forest, BART, XGBoost, MARS, logistic regression--using grid search cross-validation to both select the best hyperparameters for each model and identify top performing models. Model performance was evaluated using all metrics, though we ultimately used the F1 score to select our final models. The winning model in terms of F1 score alone was a random forest using the indiscriminate variables. However, though this model performed best on our cross validation folds and test set, we found that it didn't generalize nearly as well to the competition holdout set (F1 score of ~0.93 on our test set vs. F1 score of ~0.61 on the holdout set). We thought using a model with our hand-crafted variables might generalize better. We decided to submit our second best performing model for the final Phase 1 submission instead, a random forest model using the hand-selected features. The test set F1 score for our hand-crafted model was also only around 0.002 lower than the indiscriminate model, inspiring confidence that it will likely perform at least as well as our last submission (and hopefully better!). 

We had some difficulty submitting our model using the \texttt{train_save_model} function because of the complexity of our model selection procedure. We also were not able to run the pipeline directly in our submission because of computation time limitations of the Docker container associated with the submission repository. We do include code for the entire pipeline in the repository, but we uploaded our chosen model directly to ensure it could successfully run in Docker.

### Next Steps 

Our pipeline-based approach and comprehensive model-testing are ideally suited for scaling to larger datasets. Feature preprocessing using recipes, 10-fold cross-validation and grid search for hyperparameter tuning, and automated model performance evaluation will translate effortlessly to the larger register data. This efficiency will enable us to quickly implement and test models with the new data without extensive debugging and alterations, giving us more time to thoughtfully refine our approach and make substantial improvements to our model.

We also employ powerful models capable of handling increased data volumes (e.g. random forest, BART). These methods can better extract information about variable interactions and subtle pattens when given more samples, leading to greater predictive ability with larger data.  





