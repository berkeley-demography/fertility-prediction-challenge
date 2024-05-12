## README

This folder has the `targets`-based pipeline.

## Wishlist

* we should add many more predictors and clean up the predictors we have
  - should have current number of children (parity)
  - should have time since last child (interval)
  - should have amount of time in partnership
  - i think there are also variables about fertility intentions
  - might also explore interacting vars for some models
    (again, read up on how best to prep data for most promising models)

* we should figure out how to incorporate Nick's data - this could help a lot, in my view


* we should go through and figure out how to tune models more carefully. I think this mostly involves taking the models one at a time and reading about how to best tune them. 

* ... note also that we're using grid search for parameter tuning now - but there are faster ways (slightly approximate) in the `finetune` package

* we could try other models