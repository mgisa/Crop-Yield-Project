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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++Modelling with caret library

# Get list of all models:
all_model <- modelLookup()
# Get all models for regression and Classification:

regression_model <- all_model %>%
  filter(forClass == TRUE,
         forReg == TRUE,
         !duplicated(model))

# All packages will be used for training these models:

all_packages <- sapply(regression_model$model,
                       function(x) {
                         x %>% getModelInfo() %>% .[[1]] %>% .[["library"]]}) %>% unlist()

all_packages <- all_packages[!duplicated(all_packages)]

# All R package had being installed on your computer:

your_packages <- installed.packages() %>%
  as.data.frame() %>%
  pull(Package) %>%
  as.character()

#-----------------------------------
#  Simultaneously Train 5 Models
#-----------------------------------

# Split data:
#
set.seed(12345)
id <- createDataPartition(y = tbl_ML$production, p = 0.8, list = FALSE)
df_train_ml <- tbl_ML[id, ]
df_test_ml <- tbl_ML[-id, ]

# Set conditions for training model and cross-validation:

set.seed(12345)
number <- 3
repeats <- 2

control <- trainControl(method = "repeatedcv",
                        number = number ,
                        repeats = repeats,
                       # preProcess = c("center", "scale"),
                        classProbs = FALSE,
                        savePredictions = "final",
                        index = createResample(df_train_ml$production, repeats*number),
                        summaryFunction = defaultSummary,
                        timingSamps = 10,
                        verboseIter = FALSE,
                        allowParallel = TRUE)

# Use Parallel computing:
library(doParallel)
registerDoParallel(cores = detectCores() - 1)

# Simultaneously train some machine learning models:
library(caretEnsemble)
set.seed(12345)

# List all models that you want to train. For purpose of explanation
# I will only  use 5 models:
models_to_be_used <- c("knn","rpart","svmLinear2","glm","xgbLinear",
                       "gbm","lmStepAIC")

my_models <- regression_model %>%
              filter(model %in% models_to_be_used)
# Train these ML Models:

model_list1 <- caretList(production ~.,
                         data = df_train_ml,
                         trControl = control,
                         #metric = "RMSE",
                         methodList = models_to_be_used)

# Extract all results from ML models:

list_of_results <- lapply(models_to_be_used, function(x) {model_list1[[x]]$resample})

# Convert to data frame:
df_results <- do.call("bind_rows", list_of_results)
library(magrittr)
df_results %<>% mutate(Model = lapply(models_to_be_used, function(x) {rep(x, number*repeats)}) %>% unlist())

#RMSE
df_results %>%
  select(RMSE, Model) %>%
  #filter(!Model %in% "avNNet") %>%
  ggplot(aes(x = reorder(Model, RMSE), RMSE, fill = Model, color = Model)) +
  geom_boxplot(show.legend = FALSE, width=1,alpha=0.8,notch=TRUE) +
  theme_bw() +
  labs(x = "Predictive ML Regressors",
       y = "Regressors's Root Mean Square Error ("~~italic(RMSE)~~")  ",
       title = "Regressors Performance",
       caption = "BNR ML Lab")#+
  #coord_flip()

  # Rsquared

df_results %>%
  select(Rsquared, Model) %>%
  #filter(!Model %in% "avNNet") %>%
  ggplot(aes( x = reorder(Model, Rsquared),y =Rsquared, fill = Model, color = Model)) +
  geom_boxplot(show.legend = FALSE, width = 1,alpha=0.8) +
  theme_bw() +
  labs(x = "Predictive ML Regressors",
       y = "Regressors Fitness on the Test Set ("~~italic(R)^2~")",
       title = "Regressors Fitness",
       caption = "BNR ML Lab")#+
# Or use some statistics for comparing:
df_results %>%
  select(RMSE, Model) %>%
  #filter(!Model %in% "avNNet") %>%
  group_by(Model) %>%
  summarise(across(where(is.numeric),
    #.cols = is.numeric,
    .fns = list(Min = min, Max = max, Median = median, Mean = mean, SD=sd),
    na.rm = TRUE,
    .names = "{col}_{fn}")
    ) %>%
  arrange(RMSE_Mean) %>%
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>%
  as_tibble()

  knitr::kable()

##Go ahead and predict with both winner ML (glm and xgbLinear)

#1. glm model

glm_model <- train(production ~.,
                   data = df_train_ml,
                   preProcess = c("center", "scale"),
                   trControl = control,
                   method = "glm")
glm_model$results  #turn the result
glm_model$finalModel %>% broom::tidy()


# We use expand grid to create a table of all possible combinations
tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2),
                  interaction.depth = c(1, 3, 7, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500, 1000))

xgb_model <- train(production ~.,
                   data = df_train_ml,
                   preProcess = c("center", "scale"),
                   trControl = control,
                   tuneGrid =tg, verbose = FALSE,
                   method = "gbm")
xgb_model$results  #turn the result
xgb_model$bestTune #Select the best model
xgb_model$finalModel %>% broom::tidy()

#Compare the winning models

mods <- resamples(list(GLM = glm_model, BoostML = xgb_model))
summary(mods)

#Compare model to check is the difference is statiscally significant

compare_models(glm_model,xgb_model)

#It is seen that they difference is statistically significant, i.e each model is independent it can
#be used indepently on the prediction of production
#

#++++++++++++++INTERACTIVE DASHBOARD OF WINNING MACHINE

# create an explainer for the model
explainer <- explain(model = glm_model,
                     data = df_test_ml,
                     y = df_test_ml$production,
                     type = "regression",
                     label = "Generalized Linear Model")

# make a studio for the model
modelStudio::modelStudio(explainer)
