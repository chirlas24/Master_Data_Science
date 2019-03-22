# Multivariate Imputation by Chained Equations (mice)
library(finalfit)
library(dplyr)
library(mice)
explanatory = c("age", "sex.factor", 
                "nodes", "obstruct.factor")
dependent = "mort_5yr"

colon_s = read.csv("~/Descargas/Colon.txt", sep = "\t")

# exercise 0: what is the dataset all about?

#exercise 1: Using dplyr generate a fit_imputed table as follows:
# 1.1. select the columns in dependent and explanatory
# 1.2 retain only those individuals with information about 5Y survival and obstruct.factor
# 1.3 generate 10 imputed datasets using mice
# 1.4 run glm model on them. HINT: use with(glm(formula(ff_formula()),family="binomial"))
# 1.5 summarize the results of the 10 models using pool()
# 1.6 extract confidence intervals and exponentiate=T to obtain the OR

colon_s %>% 
  select(dependent, explanatory) %>% 
  filter(!is.na(mort_5yr) & !is.na(obstruct.factor)) %>% 
  mice(10) %>% 
  with(glm(formula(ff_formula(dependent,explanatory)), family = "binomial"))
  
  
