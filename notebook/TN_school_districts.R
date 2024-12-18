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
ggplot(districts, aes(x=region, y=enrollment)) + geom_col()
districts_na <- districts |> 
  filter(is.na(enrollment) == TRUE)
districts_na |> n_distinct("region")
#question 9
na_count_region <- districts |> 
  rowwise() |> 
  mutate(sum_na = sum(as.integer(is.na(c_across(alg_1:dropout))))) |> 
  group_by(region) |> 
  summarize(sum_na_region = sum(sum_na)) |> 
  arrange(desc(sum_na_region)) 
na_count_region
na_count_top_region <- na_count_region |> 
  slice(1) |> 
  pull(region)
na_count_top_region
#question 10
mean_grad_region <- districts |> 
  drop_na(grad) |> 
  group_by(region) |> 
  summarize(mean_grad = mean(grad)) |> 
  arrange(desc(mean_grad))
mean_grad_region
# regular mean doesn't take into consideration the size of certain districts
#question 11
weighted_mean_grad_region <- districts |> 
  drop_na(grad, enrollment) |> 
  group_by(region) |> 
  summarize(region_enrollment = sum(enrollment), mean_grad = mean(grad), weighted_mean_grad = mean(grad, region_enrollment)) |> 
  arrange(desc(weighted_mean_grad))
weighted_mean_grad_region
# now Mid Cumberland no longer has the lowest grad rate. This is because Mid Cumberland has
# such high enrollment, their size was contributing to the lower grad rate.
un_and_weighted_mean_grad_region <- districts |> 
  drop_na(grad, enrollment) |> 
  group_by(region) |> 
  summarize(region_enrollment = sum(enrollment), weighted_mean_grad = mean(grad, region_enrollment)) |> 
  arrange(desc(weighted_mean_grad))
un_and_weighted_mean_grad_region