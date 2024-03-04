"code coming from https://www.tidymodels.org/learn/statistics/infer/"
library(tidymodels)

data(gss)

dplyr::glimpse(gss)

# find the point estimate
point_estimate <- gss |> 
  specify(response = hours) |> 
  calculate(stat = "mean")

# generate a null distribution
null_dist <- gss |> 
  specify(response = hours) |> 
  hypothesize(null = "point", 
              mu = 40) |> 
  generate(reps = 5000, 
           type = "bootstrap") |> 
  calculate(stat = "mean")

null_dist |> 
  visualise()+
  shade_p_value(obs_stat = point_estimate, 
                direction = "two_sided")
null_dist |> 
  get_p_value(obs_stat = point_estimate, 
              direction = "two_sided")

null_dist |> 
  get_confidence_interval(point_estimate = point_estimate,
                          level=.95,
                          type="se")
