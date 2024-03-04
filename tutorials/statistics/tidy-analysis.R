"code coming from https://www.tidymodels.org/learn/statistics/tidy-analysis/"
library(tidymodels)

data("Orange")

Orange <- as_tibble(Orange)

ggplot(Orange, 
       aes(age, circumference, color = Tree)) +
  geom_line()+
  theme_bw()

Orange |> 
  group_by(Tree) |> 
  summarise(correlation = cor(age,
                              circumference))
# Correlation
ct <- cor.test(Orange$age,
               Orange$circumference)
tidy(ct)

Orange |> 
  nest(data = c(age,
                circumference)) |> 
  mutate(test = map(data,
                    ~cor.test(.x$age,
                              .x$circumference)),
         tidied = map(test,
                      tidy)) |> 
  unnest(cols = tidied) |> 
  select(-data, -test)

# Regression model 
lm(age ~circumference,
   data = Orange) |> 
  summary()

Orange |> 
  nest(data = c(-Tree)) |> 
  mutate(
    fit = map(data,
              ~ lm(age ~circumference,
                   data=.x)),
    tidied = map(fit, tidy)
  ) |> 
  unnest(tidied) |> 
  select(-data, -fit)

# now with mtcars

mtcars |> 
  as_tibble() |> 
  nest( data = c(-am)) |> 
  mutate(
    fit = map(data,
              ~lm(wt ~ mpg + qsec + gear,
                  data = .x)),
    tidied = map (fit, tidy),
    glanced = map(fit, glance),
    augmented = map (fit, augment)
  ) |> 
  select(augmented) |> unnest(augmented)
