install.packages(c("tidyverse","janitor"))
library(tidyverse)
library(janitor)

# first goal: make a yearly summary of each route over time
# we need to import all of our weekly summaries!
# R lets us import CSVs as a batch using the method below

# method via leerssej on stack overflow
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once

#creates a function to add a variable with the filename so we know what week it came from
read_plus <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm)
}

#makes a table of the route data from our directory using all CSVs
source_table <-
  list.files(path = "summaries/by_route/", pattern = ".*\\.csv$",
             full.names = T) %>%
  map_df(~read_plus(.))

# now we need to make that "mutate" column with the filename look nicer!
# here we do a few steps
#   use the filename to extract key info (e.g., janwk1 = first week of january)
#   generate a "date" using this info (e.g., janwk1 -> jan 1 -> 2023-01-01)
# remove the filename (no longer useful)

cleaner_table <- source_table %>%
  mutate(month = str_sub(source_table$filename,20,22), week = as.numeric(str_sub(source_table$filename,25,25))) %>%
  mutate(week_start = as.Date(paste0(month,(week*7-6),'.23'),format='%b%d.%y')) %>%
  select(-filename) %>%
  mutate(reliability2 = seconds_to_period(reliability)) %>%
  group_by(service_rte_num, weekday) %>%
  mutate(yrly_rel = mean(reliability)) %>% ungroup()

# now we want the top 10 least reliable (latest) routes for the year in order
reliability_list <- cleaner_table %>%
  group_by(service_rte_num, weekday) %>%
  summarize(yrly_rel = mean(reliability)) %>%
  arrange(yrly_rel)

write_csv(reliability_list, "reliability_list.csv")

# here we can select and view a single route's reliability over the year
table1 <- cleaner_table %>%
  filter(service_rte_num == 33)

# then we can plot that route's reliability for weekends/weekdays throughout the year
# this graph will also show the yearly average for weekends/weekdays
ggplot(table1, aes(x = week_start, y = reliability2, color = weekday)) +
  geom_line() +
  labs(title = "Reliability Over Time", x = "Week Of", y = "Average Lateness (seconds)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red")) +
  geom_hline(aes(yintercept = yrly_rel, color = weekday), linetype = "dashed")

# the above is an exploratory graph; now we need to make a more useful descriptive map for analysis
# top ten late bus routes showing weekend and weekday reliability
# removes routes without weekend service
table2 <- reliability_list %>% pivot_wider(names_from = weekday, values_from = yrly_rel) %>% 
  rename(route = service_rte_num, weekday_lateness = weekday, weekend_lateness = weekend) %>% na.omit

# listing top ten latest routes (that have both weekday and weekend service)
# also converting lateness into minutes for the sake of our graph being easier to understand
late10 <- table2 %>% filter(route %in% c(11, 208, 271, 8, 255, 62, 28, 43, 5, 132)) %>%
  mutate(weekday_lateness = weekday_lateness / 60, weekend_lateness = weekend_lateness / 60) %>%
  mutate(route=factor(route))

# making a nice cleveland dot plot to show weekend and weekday lateness!
# foundational code via r graph gallery (https://r-graph-gallery.com/303-lollipop-plot-with-2-values.html)
ggplot(late10) +
  geom_segment( aes(x=route, xend=route, y=weekday_lateness, yend=weekend_lateness), color="grey") +
  geom_point( aes(x=route, y=weekday_lateness), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=route, y=weekend_lateness), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme(
    legend.position = "top",
  ) +
  xlab("Route Number") +
  ylab("Lateness (minutes past scheduled time)")
