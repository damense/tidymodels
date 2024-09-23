## Chapter 3----
library(tidyverse)

data(crickets, package = "modeldata")
names(crickets)

# Plot the temperature on the x-axis, the chirp rate on the y-axis. The plot
# elements will be colored differently for each species:
ggplot(crickets, 
       aes(x = temp, y = rate, color = species, pch = species, lty = species)) + 
  # Plot points for each data point and color by species
  geom_point(size = 2) + 
  # Show a simple linear model fit created separately for each species:
  geom_smooth(method = lm, se = FALSE, alpha = 0.5) + 
  scale_color_brewer(palette = "Paired") +
  labs(x = "Temperature (C)", y = "Chirp Rate (per minute)")+
  theme_bw()

lm(rate ~ (temp + species)^2, data = crickets) |> 
  plot()

# Null hypothesis is that there's no difference between models
anova(lm(rate ~ temp + species, data = crickets),
      lm(rate ~ (temp + species)^2, data = crickets))
#p-value=0.25 fails to reject the hypothesis

lm(rate ~ (temp + species), data = crickets) |> 
  plot()
## Chapter 4----
library(tidymodels)
data(ames)
ames <- ames |>  
  mutate(Sale_Price = log10(Sale_Price))
## Chapter 5----
library(tidymodels)
tidymodels_prefer()

# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(502)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, 
                            prop = 0.80,
                            strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

## Chapter 6 ----
library(tidymodels)
tidymodels_prefer()

lm_model <- 
  linear_reg() |> 
  set_engine("lm")

## Chapter 7----
lm_wflow <- workflow() |> 
  add_model(lm_model) |> 
  add_variables(outcome = Sale_Price, 
                predictors = c(Longitude, Latitude))

lm_fit <- fit(lm_wflow,
              ames_train)

## Chapter 8 ----
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) |>
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) |> 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <- workflow() |> 
  add_model(lm_model) |> 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow,
              ames_train)

## Chapter 9 ----
rf_model <- 
  rand_forest(trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_wflow <- 
  workflow() |> 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) |> 
  add_model(rf_model) 
## Chapter 10 ----
set.seed(1001)
ames_folds <- vfold_cv(ames_train, 
                       v = 10)

keep_pred <- control_resamples(save_pred = TRUE, 
                               save_workflow = TRUE)

set.seed(1003)
rf_res <- rf_wflow |>  
  fit_resamples(resamples = ames_folds, 
                control = keep_pred) 
## Chapter 11 ----
basic_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train)  |> 
  step_log(Gr_Liv_Area, base = 10) |> 
  step_other(Neighborhood, threshold = 0.01) |> 
  step_dummy(all_nominal_predictors())

interaction_rec <- 
  basic_rec |> 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 

spline_rec <- 
  interaction_rec |> 
  step_ns(Latitude, Longitude, deg_free = 50)

preproc <- 
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec
  )

lm_models <- workflow_set(preproc, 
                          list(lm = linear_reg()), 
                          cross = FALSE) |>
  bind_rows(as_workflow_set(random_forest = rf_res)) |> 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               seed = 1101, verbose = TRUE,
               # Options to `fit_resamples()`: 
               resamples = ames_folds, control = keep_pred)
library(ggrepel)
autoplot(lm_models, metric = "rsq") +
  geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none")

## Chapter 12----
rf_spec <- 
  rand_forest(mtry = tune()) %>% 
  set_engine("ranger", regularization.factor = tune("regularization")) %>%
  set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_spec)

pca_rec <- 
  recipe(Sale_Price ~ ., data = ames_train) %>% 
  # Select the square-footage predictors and extract their PCA components:
  step_normalize(contains("SF")) %>% 
  # Select the number of components needed to capture 95% of
  # the variance in the predictors. 
  step_pca(contains("SF"), threshold = .95)

updated_param <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(pca_rec) %>% 
  extract_parameter_set_dials() %>% 
  finalize(ames_train)

## Chapter 13 ----
library(usemodels)

use_xgboost(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
              Latitude + Longitude, 
            data = ames_train,
            # Add comments explaining some of the code:
            verbose = TRUE)

xgboost_recipe <- 
  recipe(formula = Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) |> 
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>  
  step_zv(all_predictors()) 

xgboost_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost") 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 

set.seed(3101)
xgboost_tune <-
  tune_grid(xgboost_workflow, resamples = stop("add your rsample object"), 
            grid = stop("add number of candidate points"))
