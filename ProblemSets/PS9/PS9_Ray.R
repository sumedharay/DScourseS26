# ============================================================
# Problem Set 9
# Econ 5253 - Spring 2026
# Sumedha Ray
# ============================================================

library(tidymodels)
library(glmnet)

# Loading UCI Boston Housing data 

housing <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv")
glimpse(housing)

set.seed(123456)

#  Train/test split 
housing_split <- initial_split(housing, prop = 0.75)
housing_train <- training(housing_split)   # ~380 rows
housing_test  <- testing(housing_split)    # ~126 rows

cat("Training rows:", nrow(housing_train), "\n")
cat("Test rows:", nrow(housing_test), "\n")
cat("Original columns:", ncol(housing), "\n")

#  Defining the recipe 
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                  ptratio:b:lstat:dis:nox) %>%
  step_poly(crim, zn, indus, rm, age, rad, tax,
            ptratio, b, lstat, dis, nox, degree = 6)


housing_prep         <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice()
housing_test_prepped  <- housing_prep %>% bake(new_data = housing_test)

# Separating X and y 
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y  <- housing_test_prepped  %>% select(medv)


cat("Columns in prepped training X:", ncol(housing_train_x), "\n")
cat("Original predictors:", ncol(housing) - 1, "\n")
cat("New predictors added:", ncol(housing_train_x) - (ncol(housing) - 1), "\n")

# ============================================================
# LASSO MODEL
# ============================================================

# Setting up 6-fold CV 
set.seed(123456)
folds <- vfold_cv(housing_train, v = 6)

# Define the LASSO model spec 
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Bundle recipe + model into a workflow 
lasso_workflow <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(lasso_spec)

# Create a grid of lambda values to search over 

lambda_grid <- grid_regular(penalty(range = c(-5, 1)), levels = 100)

#  Run cross-validation tuning 
lasso_tune <- tune_grid(
  lasso_workflow,
  resamples = folds,
  grid = lambda_grid,
  metrics = metric_set(rmse)   # evaluate using RMSE
)

#  The best lambda 
best_lasso_lambda <- select_best(lasso_tune, metric = "rmse")
cat("Best LASSO lambda:", best_lasso_lambda$penalty, "\n")

lasso_final <- lasso_workflow %>%
  finalize_workflow(best_lasso_lambda) %>%
  fit(data = housing_train)

# In-sample RMSE 
lasso_fit_engine <- lasso_final %>% extract_fit_parsnip()

lasso_train_preds <- predict(lasso_fit_engine, new_data = housing_train_x) %>%
  bind_cols(housing_train_y)

lasso_rmse_train <- lasso_train_preds %>%
  rmse(truth = medv, estimate = .pred)
cat("LASSO In-sample RMSE:", lasso_rmse_train$.estimate, "\n")

# Out-of-sample RMSE 
lasso_test_preds <- predict(lasso_fit_engine, new_data = housing_test_x) %>%
  bind_cols(housing_test_y)

lasso_rmse_test <- lasso_test_preds %>%
  rmse(truth = medv, estimate = .pred)
cat("LASSO Out-of-sample RMSE:", lasso_rmse_test$.estimate, "\n")


# ============================================================
# RIDGE REGRESSION MODEL
# ============================================================

ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

ridge_workflow <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(ridge_spec)

ridge_tune <- tune_grid(
  ridge_workflow,
  resamples = folds,
  grid = lambda_grid,
  metrics = metric_set(rmse)
)

best_ridge_lambda <- select_best(ridge_tune, metric = "rmse")
cat("Best Ridge lambda:", best_ridge_lambda$penalty, "\n")

ridge_final <- ridge_workflow %>%
  finalize_workflow(best_ridge_lambda) %>%
  fit(data = housing_train)

ridge_fit_engine <- ridge_final %>% extract_fit_parsnip()

ridge_test_preds <- predict(ridge_fit_engine, new_data = housing_test_x) %>%
  bind_cols(housing_test_y)

ridge_rmse_test <- ridge_test_preds %>%
  rmse(truth = medv, estimate = .pred)
cat("Ridge Out-of-sample RMSE:", ridge_rmse_test$.estimate, "\n")