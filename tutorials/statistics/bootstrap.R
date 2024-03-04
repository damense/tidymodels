"code coming from https://www.tidymodels.org/learn/statistics/bootstrap/"

library(tidymodels)
ggplot(mtcars,
       aes(mpg,wt))+
  geom_point()+
  theme_bw()
# Fit with nonlinear least squares (nls)

nlsfit <- nls(mpg ~ k / wt + b, 
              mtcars, 
              start = list(k = 1, 
                           b = 0))
summary(nlsfit)
ggplot(mtcars,
       aes(wt,mpg))+
  geom_point()+
  geom_line(aes(y = predict(nlsfit)))+
  theme_bw()

# Let's bootstrap
set.seed(31)
boots <- bootstraps(mtcars,
                    times=2000,
                    apparent = T)

fit_nls_on_bootstrap <- function(split) {
  nls(mpg ~ k / wt + b, 
      analysis(split), 
      start = list(k = 1, 
                   b = 0))
}

boot_models <- boots |> 
  mutate(model= map(splits, fit_nls_on_bootstrap),
         coef_info = map(model, tidy))

boot_coef <- boot_models |> 
  unnest(coef_info)

p_intervals <- int_pctl(boot_models, coef_info)

ggplot(boot_coef, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = p_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = p_intervals, col = "blue")

# All the models
boot_models |> 
  sample_n(1000) |> 
  mutate(augmented = map(model, augment)) |> 
  unnest(augmented) |> 
  ggplot(aes(wt, mpg))+
  geom_line(aes(y=.fitted,
                group=id),
            alpha=.2,
            col="blue")+
  geom_point()
