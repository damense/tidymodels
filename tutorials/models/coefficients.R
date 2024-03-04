library(tidymodels)
tidymodels_prefer()
theme_set(theme_bw())

data("Chicago")


lm_spec <- linear_reg()
lm_fit <- fit(lm_spec, 
              ridership ~ ., 
              data = Chicago |> 
                select(ridership,Clark_Lake,Austin, Harlem)
              )
set.seed(123)
bt <- bootstraps(Chicago|> 
                   select(ridership,Clark_Lake,Austin, Harlem),
                 times=5)
get_lm_coefs <- function(x) {
  x |> 
    extract_fit_engine() |> 
    tidy()
}
tidy_ctrl <- control_grid(extract = get_lm_coefs)
lm_spec |> 
  fit_resamples(ridership ~ ., 
                resamples = bt, 
                control = tidy_ctrl) |> 
  select(id, .extracts) |> 
  unnest(.extracts) |> 
  unnest(.extracts) |> 
  filter(term != "(Intercept)") |> 
  ggplot(aes(x=term,
             y=estimate,
             group =id,
             col=id))+
  geom_hline(yintercept=0, linetype =3)+
  geom_line(alpha=.3, linewidth = 1.2) + 
  labs(y ="Coefficient",
       x=NULL)


# glmnet 
glmnet_spec <- 
  linear_reg(penalty = 0.1, mixture = 0.95) %>% 
  set_engine("glmnet")

glmnet_wflow <- 
  workflow() %>% 
  add_model(glmnet_spec) %>% 
  add_formula(ridership ~ .)

glmnet_fit <- fit(glmnet_wflow,
                  Chicago |> 
                    select(ridership,Clark_Lake,Austin, Harlem)
                  )

glmnet_fit |> 
  extract_fit_engine() |> 
  tidy() |> 
  rename(penalty = lambda) |> 
  filter(term != "(Intercept)") 

tidy(glmnet_fit)
