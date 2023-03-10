# LIBRARIES ----

library(tidyverse)  # Core
library(janitor)    # Clean names
library(tidymodels) # Modeling
library(DALEX)      # Explainer
library(modelDown)  # Explainable AI Report
library(caret)
#library(FSelector)
library(corrplot)
library(ggcorrplot)
#https://github.com/business-science/free_r_tips/blob/master/049_modelstudio/049_modelstudio.R
#Source:https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
#https://github.com/business-science/free_r_tips/blob/master/052_modeldown/052_modeldown.R
tbl_crops <- rio::import(here::here("data/tbl_ML.csv"))
tbl <- tbl_crops %>%
  clean_names() %>%
  mutate_if(is.character, as_factor) %>%
  mutate(production = round(log(production), digits = 2),
        arable_size = round(log(arable_size), digits = 2))

#Data preprocessing and feature enginerring

#MULTICOLLINEARITY TESTING THE CORRELATION AMONG VARIABLES
#_________________________________________
numericVarName <- names(which(sapply(tbl, is.numeric)))
corr <- cor(tbl[,numericVarName], use = 'pairwise.complete.obs')
ggcorrplot(corr, lab = TRUE, title = "CORRELATION AMONG CONTINUOUS VARIABLES")

# Automatic selection of impo var (Recursive Feature Elimination)


df2 <- tbl %>% select(-c(year, seasons, districts))
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      number= 10,
                      repeats = 5,
                      verbose = FALSE,
                      allowParallel = TRUE)
outcomeName<-'production'
predictors<-names(df2)[!names(df2) %in% outcomeName]
bcancer_Pred_Profile <- rfe(df2[,predictors], df2[,outcomeName],
                            rfeControl = control)
print(bcancer_Pred_Profile)
#_________
library(randomForest)

attribute.scores <- random.forest.importance(production ~ ., df2)
attribute.scores
Top_10_features<-cutoff.k(attribute.scores, k = 10) # Top 10 features
Top_10_features
#final data including top 10 features
#
tbl_ML<-tbl%>%
  dplyr::select(crop_type,arable_size,rainfall,ndvi,atm_pressure,evapo_trans,l_stemp,
                solar_rad, production)


# Data sampling

tbl_split <- initial_split(tbl_ML, prop = 0.8)
tbl_split

#To access the observations reserved for training, use the training() function. Similarly, use testing() to access the testing data.

#Acessing training set
tbl_split %>%
  training() %>%
  glimpse()
#Accessing Test set
tbl_split %>%
  testing() %>%
  glimpse()
#Reciping trainings set
tbl_recipe <- training(tbl_split) %>%
  recipe(production ~.) %>%
  #step_rm(year) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_corr(all_predictors(),-all_nominal_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

#Reciping test set the same as training

tbl_testing <- tbl_recipe %>%
  bake(testing(tbl_split))

glimpse(tbl_testing)

#Juicing training set
tbl_training <- juice(tbl_recipe)
glimpse(tbl_training)

#Model Training
tbl_ranger <- rand_forest(trees = 100,
                          mode = "regression") %>%
  set_engine("ranger") %>%
  fit(production ~ ., data = tbl_training)

tbl_rf <-  rand_forest(trees = 100, mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(production ~ ., data = tbl_training)

#Prediction

predict(tbl_ranger, tbl_testing)

#adding prediction to testing data
tbl_ranger %>%
  predict(tbl_testing) %>%
  bind_cols(tbl_testing) %>%
  glimpse()

#Model validation

#1. ranger model
tbl_ranger %>%
  predict(tbl_testing) %>%
  bind_cols(tbl_testing) %>%
  metrics(truth = production, estimate = .pred)
#2. rf model

tbl_rf %>%
  predict(tbl_testing) %>%
  bind_cols(tbl_testing) %>%
  metrics(truth = production, estimate = .pred)
# 1.Analyse the output
#2. Make it interactive
library(DALEX)

model_tree <- rpart(production ~ ., data = tbl_training)

exp_tree <- DALEX::explain(tbl_ranger, data = tbl_training %>% select(-production),
                    y = tbl_training$production, label = "Ranger", type = "regression")



library(caret)
model_forest <- train(production ~ ., data = tbl_training, method = "rf",
                      metric = "RMSE", mrty = 2, ntree = 300, maxnodes = 14)

exp_forest <- DALEX::explain(model_forest, tbl_training %>% select(-production),
                      y = tbl_training$production,
                      label = "Random forest", type = "regression")
modelDown::modelDown(
  exp_tree,
  modules = c("auditor", "drifter", "model_performance", "variable_importance",
                             "variable_response"),
  output_folder = here::here("output/modeldown")
)
modelStudio::modelStudio(exp_tree)

#Update rf Model
exp_forest_updated <- update_data(exp_forest, data = tbl_testing %>% select(-production),
                                  y = tbl_testing$production)
exp_tree_updated <- update_data(exp_tree, data = tbl_testing %>% select(-production),
                                  y = tbl_testing$production)
#Model Performance

perf_forest <- model_performance(exp_forest_updated)
perf_forest
perf_tree <- model_performance(exp_tree_updated)
perf_tree

#Performance Chart
plot(perf_tree, perf_forest, geom = "roc")

plot(perf_tree, perf_forest, geom = "boxplot")

#Prediction and var contribution

sh_forest <- predict_parts(exp_forest, tbl_testing, type = "shap", B = 1)
plot(sh_forest, show_boxplots = FALSE) +
  ggtitle("Shapley values for Model Testing","")
#Visualize feature attributions for testing prediction using break down values.

bd_forest <- predict_parts(exp_forest, tbl_testing, type = "break_down_interactions")
bd_forest

plot(bd_forest, show_boxplots = FALSE) +
  ggtitle("Break down values for model testing","")
#Feture Importance

mp_tree <- model_parts(exp_tree, type = "difference")
mp_forest<- model_parts(exp_forest, type = "difference")


plot(mp_tree %>% #filter(dropout_loss > mean(dropout_loss))%>%
       group_by(variable,label) %>%
       summarise(dropout_loss = mean(dropout_loss),.groups="drop") %>%
       arrange(desc(dropout_loss)),
     mp_forest%>% #filter(dropout_loss > mean(dropout_loss))%>%
       group_by(variable,label) %>%
       summarise(dropout_loss=mean(dropout_loss),
                 .groups="drop") %>%
       arrange(desc(dropout_loss)),
     show_boxplots = FALSE)#+facet_wrap(~label,ncol=2)


cp_forest <- predict_profile(exp_forest, tbl_testing)

plot(cp_forest, variables = c("ndvi", "rainfall"))
