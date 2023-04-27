pacman::p_load(tidyverse, tidymodels)
pacman::p_load(textrecipes, stopwords)
setwd("G:/My Drive/Adelaide uni/Stats7022/Assignment 3")
jobs<-readRDS('jobs.rds')
View(jobs)
jobs1=jobs%>%select(state,job_type,sector,annual_salary,salary)
View(jobs1)
summary(jobs)

jobs_no_NA=jobs1%>%
  filter(!is.na(annual_salary))

View(jobs_no_NA)
summary(jobs_no_error)
?step_unknown()
View(jobs_no_error)
jobs_final=jobs_no_NA%>%
  filter(annual_salary>1000)
View(jobs_final)
jobs_hourly=jobs_no_NA%>%
  filter(annual_salary<=105 & annual_salary>2)

jobs_weekly=jobs_no_NA%>%
  filter(annual_salary>200 & annual_salary<1000)
View(jobs_hourly)
View(jobs_weekly)
jobs_hourly$annual_salary=jobs_hourly$annual_salary*1800
jobs_weekly$annual_salary=jobs_weekly$annual_salary*52
View(jobs_hourly)

Vi
jobs_final=rbind(jobs_hourly,jobs_weekly,jobs_final)

jobs_final=jobs_final%>%
  select(-salary)%>%
  mutate(sector=as.factor(replace_na(sector,'Other')),
         state=as.factor(replace_na(state,'Other')),
         job_type=as.factor(job_type))

jobs_final=jobs_final%>%
  mutate(sector=fct_lump_prop(sector,0.01),
         state=fct_lump_prop(state,0.01),
         job_type=fct_lump_prop(job_type,0.01))
summary(jobs_final)

set.seed(1)
jobs_split <- initial_split(jobs_final, strata = annual_salary)
jobs_train <- training(jobs_split)
jobs_test <- testing(jobs_split)

?initial_split
recipe_rf<- recipe(annual_salary~. ,data=jobs_train)
  


rf_model <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("ranger")

wf_model=workflow()%>%
  add_model(rf_model)%>%
  add_recipe(recipe_rf)
rf_grid <- grid_regular(mtry(c(1,ncol(jobs_final)-1)),
                          min_n(),
                          levels = 5)
rf_grid
folds <- vfold_cv(jobs_train, v = 5,  strata = annual_salary)


doParallel::registerDoParallel()
rf_tune <- tune_grid(wf_model,
                       resamples = folds,
                       grid = rf_grid)
write_rds(rf_tune,"jobs_tune.rds")
View(rf_tune)
tune_plot=rf_tune %>% autoplot()
wf_model <- wf_model %>%
  finalize_workflow(select_best(rf_tune, metric = "rmse"))
select_best(rf_tune, metric = "rmse")
select_best(rf_tune, metric = "rsq")

rf_fit <- wf_model %>% last_fit(split = jobs_split)
rf_fit %>% collect_metrics()
View(rf_fit)
rf_fi
rf_fit %>% collect_predictions() %>%
  ggplot(aes(annual_salary, .pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1)

?last_fit







#### with some natural language processing 


jobs2=jobs%>%
  select(state,job_type,sector,job_description,job_title,annual_salary,salary)%>%
  filter(!is.na(annual_salary))



jobs_final2=jobs2%>%
  filter(annual_salary>1000)

jobs_hourly=jobs2%>%
  filter(annual_salary<=105 & annual_salary>2)

jobs_weekly=jobs2%>%
  filter(annual_salary>200 & annual_salary<1000)

jobs_hourly$annual_salary=jobs_hourly$annual_salary*1800
jobs_weekly$annual_salary=jobs_weekly$annual_salary*52


jobs_final2=rbind(jobs_hourly,jobs_weekly,jobs_final2)
View(jobs_final2)
jobs_final2=jobs_final2%>%
  select(-salary)%>%
  mutate(sector=as.factor(replace_na(sector,'Other')),
         state=as.factor(replace_na(state,'Other')),
         job_type=as.factor(job_type))
jobs_final2%>%
  ggplot()+
  geom_histogram(aes(x=log(annual_salary)))


jobs_final2=jobs_final2%>%
  mutate(sector=fct_lump_prop(sector,0.01),
         state=fct_lump_prop(state,0.01),
         job_type=fct_lump_prop(job_type,0.01),
         annual_salary=log(annual_salary))
set.seed(2023)
jobs_split <- initial_split(jobs_final2, strata = annual_salary,
                            prop=0.8)
jobs_train <- training(jobs_split)
jobs_test <- testing(jobs_split)

View(jobs_final2)
jobs_recipe_no_title <- recipe(annual_salary ~ .,
                      data = jobs_train) %>%
  step_rm(job_title)%>%
  step_tokenize(job_description) %>%
  step_stopwords(job_description) %>%
  step_tokenfilter(job_description,
                   max_tokens = 40) %>%
  step_tfidf(job_description)%>%
  step_normalize(all_numeric_predictors())

jobs_recipe_with_title <- recipe(annual_salary ~ .,
                               data = jobs_train) %>%
  step_tokenize(job_description,job_title) %>%
  step_stopwords(job_description,job_title) %>%
  step_tokenfilter(job_description,max_tokens = 40) %>%
  step_tokenfilter(job_title,max_tokens =20)%>%
  step_tfidf(job_description,job_title)%>%
  step_normalize(all_numeric_predictors())

head(jobs_recipe_no_title%>%prep()%>%bake(new_data=NULL))
View(jobs_recipe_with_title%>%prep()%>%bake(new_data=NULL))

?step_tokenfilter
predictors_with_title=ncol(jobs_recipe_with_title%>%prep()%>%bake(new_data=NULL))-1
predictors_no_title=ncol(jobs_recipe_no_title%>%prep()%>%bake(new_data=NULL))-1
## the model
rf_model <- rand_forest(mtry = tune(),
                        min_n = tune(),
                        trees = 500) %>%
  set_mode("regression") %>%
  set_engine("ranger")
## folds
folds <- vfold_cv(jobs_train, v = 5, strata = annual_salary)
#tuning grid
rf_grid_no_title <- grid_regular(mtry(c(10,predictors_no_title)),
                        min_n(),
                        levels=5)
rf_grid_with_title <- grid_regular(mtry(c(10,predictors_with_title)),
                                 min_n(),
                                 levels=5)

rf_wf_with_title=workflow()%>%
  add_model(rf_model)%>%
  add_recipe(jobs_recipe_with_title)
doParallel::registerDoParallel()

rf_tune_with_title <- tune_grid(rf_wf_with_title,
                     resamples = folds,
                     grid = rf_grid_with_title)


rf_tune_with_title%>%autoplot()

write_rds(rf_tune_with_title,'rf_tune_with_title.rds')
rf_tune_with_title=readRDS('rf_tune_with_title.rds')

rf_wf_no_title=workflow()%>%
  add_model(rf_model)%>%
  add_recipe(jobs_recipe_no_title)
doParallel::registerDoParallel()
rf_tune_no_title <- tune_grid(rf_wf_no_title,
                                resamples = folds,
                                grid = rf_grid_no_title)


rf_tune_no_title%>%autoplot()
write_rds(rf_tune_no_title,'rf_tune_no_title.rds')
rf_tune_no_title=readRDS('rf_tune_no_title.rds')

rf_grid_no_title_2 <- grid_regular(mtry(c(1,10)),
                                   min_n(),
                                   levels=3)
rf_grid_with_title_2 <- grid_regular(mtry(c(1,10)),
                                    min_n(),
                                    levels=3)

rf_wf_with_title_2=workflow()%>%
  add_model(rf_model)%>%
  add_recipe(jobs_recipe_with_title)
doParallel::registerDoParallel()
rf_tune_with_title_2 <- tune_grid(rf_wf_with_title_2,
                                resamples = folds,
                                grid = rf_grid_with_title_2)


rf_tune_with_title_2%>%autoplot()

write_rds(rf_tune_with_title_2,'rf_tune_with_title_2.rds')
rf_tune_with_title_2=readRDS('rf_tune_with_title_2.rds')

rf_wf_no_title_2=workflow()%>%
  add_model(rf_model)%>%
  add_recipe(jobs_recipe_no_title)
doParallel::registerDoParallel()
rf_tune_no_title_2 <- tune_grid(rf_wf_no_title_2,
                              resamples = folds,
                              grid = rf_grid_no_title_2)


rf_tune_no_title_2%>%autoplot()
write_rds(rf_tune_no_title_2,'rf_tune_no_title_2.rds')
rf_tune_no_title_2=readRDS('rf_tune_no_title_2.rds')
## finalize the workflow with the best model from tuning
rf_wf_with_title_2 <- rf_wf_with_title_2 %>%
  finalize_workflow(select_best(rf_tune_with_title_2, metric = "rsq"))

## fitting the model again on all training data and use the model on
#the test data
rf_wf_with_title_2 <- rf_wf_with_title_2 %>% last_fit(split = jobs_split)

## final model performance
rf_wf_with_title_2 %>% collect_metrics()


rf_wf_no_title_2 <- rf_wf_no_title_2 %>%
  finalize_workflow(select_best(rf_tune_no_title_2, metric = "rsq"))

## fitting the model again on all training data and use the model on
#the test data
jobs_fit_rf_no_title <- rf_wf_no_title_2 %>% last_fit(split = jobs_split)

## final model performance
jobs_fit_rf_no_title %>% collect_metrics()



ridge_regression_model=linear_reg(
  mode='regression',mixture = 1,penalty = tune()
)%>%
  set_engine('glmnet')
jobs_recipe_no_title=jobs_recipe_no_title%>%
  step_dummy(all_nominal_predictors())
ridge_wf=workflow()%>%
  add_model(ridge_regression_model)%>%
  add_recipe(jobs_recipe_no_title)

my_grid <- tibble(penalty = seq(0.0001,1000,length.out=100))
doParallel::registerDoParallel()
ridge_tune<- tune_grid(wf2,
                       resamples = folds,
                       grid = my_grid)
ridge_tune%>%autoplot()

ridge_wf=ridge_wf %>%
  finalize_workflow(select_best(ridge_tune, metric = "rsq"))
ridge_final_fit <- ridge_wf %>% last_fit(split = jobs_split)

## final model performance
ridge_final_fit %>% collect_metrics()

knn_model<-nearest_neighbor( mode = "regression", 
                             neighbors = tune())%>%
  set_engine('kknn')
wf3=workflow()%>%
  add_model(knn_model)%>%
  add_recipe(jobs_recipe_no_title)

knn_grid <- tibble(neighbors = seq(1,100,1))
knn_tune<- tune_grid(wf3,
                       resamples = folds,
                       grid = knn_grid)
knn_tune%>%autoplot()
wf3 <- wf_model %>%
  finalize_workflow(select_best(knn_tune, metric = "rsq"))
knn_fit <- wf3 %>% last_fit(split = jobs_split)

## final model performance
knn_fit %>% collect_metrics()

