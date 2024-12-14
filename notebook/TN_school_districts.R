library(tidyverse)
#question 1
districts <- read_csv("../data/districts.csv")
#question 2
districts <- districts |> 
  slice(-1)
#question 3
proficiency_over_80 <- districts |> 
  filter((alg_1 >= 80) & (eng_1 >= 80))
proficiency_over_80 |> 
  tally()
#question 4
proficiency_under_50 <- districts |> 
  filter((alg_1 < 50) | (eng_1 < 50))
proficiency_under_50 |> 
  tally()
#question 5: district with lowest grad rate
lowest_grad_district <- districts |> 
  arrange(grad) |> 
  slice(1) |> 
  pull(system_name)
lowest_grad_district
#question 6: Within the Mid Cumberland region, which district has the highest ACT composite?
mid_cumb_highest_act <- districts |> 
  filter(region == "Mid Cumberland") |> 
  arrange(desc(act_composite)) |>   
  slice(1) |> 
  pull(system_name)
mid_cumb_highest_act
#question 7
p <- ggplot(districts |> drop_na(), aes(x=alg_1, y=alg_2)) + geom_point()
p
p + facet_grid(cols=vars(region))
#These scores seem to have a highly pos. correlation.
#After faceting, it becomes clear that some regions generally have higher scores than others, but the correlation remains the same.
#question 8
ggplot(districts, aes(x=))
