# Description of submission

### Summary

Our team used the tidymodels package in R to automate much of our model selection procedure. We tested several different model types (random forest, BART, XGBoost, and logistic regression) and several different combinations of features. To tune model hyperparameters, we used 10 fold cross-validation (repeated 5 times). We then fit our best performing model (highest F1 score) on our held out test set.  

### Variables

We tried several different combinations of variables in our model pipeline. The two best performing sets were 1) a large set (around 250 variables) of indiscriminate variables and 2) a smaller set (how many?) of theory-based, hand-selected variables. The hand-crafted variables include a variety of questions about prior fertility and future intentions, education, partnership, housing, income, migration, gender, and age and mostly take values from the most recent survey wave. 

In both sets, a few variables held most of the predictive power. Two survey questions asking whether the respondent was planning to have more children in the future and in how many years they planned on having their next child dominated our feature importance plots. Other influential variables included age, domestic situation, income, and civil status. 

### Preprocessing Steps 

Variable preparation using tidymodels involves the creation of variable "recipes". These recipes build a series of preprocessing steps that make training data on many different models more effortless. In the indiscriminate variable recipe we remove all zero variance predictors, impute missing values using k-nearest neighbor imputation, convert all categorical features to factor variables, and center and scale most numeric predictors. In the hand-crafted recipe, we are more intentional which each variable and start by taking data from the most recent year only. We also adjust/clean features to ensure they contain as much information as possible (e.g. creating a new variable representing whether respondents have any children, cleaning estimate of time to next child so it is an integer value). We then impute missing values using k-nearest neighbors imputation.

### Model 

We fit and tuned many models using grid search cross validation to first, select the best hyperparameters for each model and second, to select our top performing models. Model performance was evaluated using all metrics, though we ultimately used the F1 score to select our final model. The winning model was a random forest using the indiscriminate variables. However, though this model performed best on our cross validation folds and test set, we found that it didn't generalize nearly as well to the competition holdout set (F1 score of ~0.93 on our test set vs. F1 score of ~0.61 on the holdout set). We thought using a model with our hand-crafted variables might generalize better so decided to submit our second best performing model instead, a BART model using the hand-selected features, for the final Phase 1 submission. 

# Next Steps 

Our pipeline-based approach and comprehensive model-testing are ideally suited for scaling to larger datasets. Feature preprocessing using recipes, 10-fold cross-validation and grid search for hyperparameter tuning, and automated model performance evaluation will translate effortlessly to the larger register data. This efficiency will enable us to quickly implement and test models with the new data without extensive debugging and alterations, allowing more time for thoughtfully refining our approach and making substantial improvements to our model.

We also employ powerful models capable of handling increased data volumes (e.g. random forest, BART). These methods can better extract information about variable interactions and subtle pattens when given more samples, leading to greater predictive ability with larger data.  



# Reflection (Optional)

To do: add something about how to have to choose a model to submit so that it runs in docker, but we do include our code for the whole pipeline. 






