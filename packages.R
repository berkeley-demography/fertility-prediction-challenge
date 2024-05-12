#!/usr/bin/env Rscript

install.packages(c("dplyr",
		   "data.table",
		   "tidyr",
		   "tidyverse", 
		   "here", 
                   "tidymodels", 
		   "xgboost", 
		   "ranger", 
		   "bundle"), 
		 repos="https://cran.r-project.org", 
		 dependencies=TRUE)
