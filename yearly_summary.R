install.packages(c("tidyverse","janitor", "ggtext"))
library(tidyverse)
library(janitor)
library(ggtext)


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
#   remove lines with numbers over 700 - these are usually school routes or dial-a-ride transit
#   use the filename to extract key info (e.g., janwk1 = first week of january)
#   generate a "date" using this info (e.g., janwk1 -> jan 1 -> 2023-01-01)
# remove the filename (no longer useful)

cleaner_table <- source_table %>%
  mutate(month = substr(source_table$filename,20,22), week = as.numeric(substr(source_table$filename,25,25))) %>%
  mutate(week_start = as.Date(paste0(month,(week*7-6),'.23'),format='%b%d.%y')) %>%
  select(-filename) %>%
  filter(service_rte_num < 700) %>%
  mutate(reliability2 = seconds_to_period(reliability)) %>%
  group_by(service_rte_num, weekday) %>%
  mutate(week_rel = mean(reliability)) %>% ungroup() %>%
  group_by(service_rte_num) %>%
  mutate(yrly_rel = mean(reliability))

# now we want the top 10 least reliable (latest) routes for the year in order
reliability_list_weekday <- cleaner_table %>%
  group_by(service_rte_num, weekday) %>%
  summarize(week_rel = mean(reliability)) %>%
  arrange(week_rel)

reliability_list_overall <- cleaner_table %>%
  group_by(service_rte_num) %>%
  summarize(yrly_rel = mean(reliability)) %>%
  arrange(yrly_rel, descending = T)

reliability_list <- left_join(reliability_list_weekday, reliability_list_overall, by = "service_rte_num")

write_csv(reliability_list_weekday, "reliability_list_weekday.csv")
write_csv(reliability_list_overall, "reliability_list_overall.csv")

# here we can select and view a single route's reliability over the year
table1 <- cleaner_table %>%
  filter(service_rte_num == 33)

# then we can plot that route's reliability for weekends/weekdays throughout the year
# this graph will also show the yearly average for weekends/weekdays
ggplot(table1, aes(x = week_start, y = reliability2, color = weekday)) +
  geom_line() +
  labs(title = "Reliability Over Time", x = "Week Of", y = "Average Lateness (seconds)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red")) +
  geom_hline(aes(yintercept = week_rel, color = weekday), linetype = "dashed")

reliability_list_updated <- read_csv("reliability_list_updated.csv")

# the above is an exploratory graph; now we need to make a more useful descriptive map for analysis
# top ten late bus routes showing weekend and weekday reliability
# removes routes without weekend service
table2 <- reliability_list_updated %>% 
  pivot_wider(names_from = weekday, values_from = week_rel) %>% 
  rename(weekday_lateness = weekday, weekend_lateness = weekend, overall_lateness = yrly_rel) %>%
  arrange(overall_lateness)


# listing top ten latest routes (that have both weekday and weekend service)
# also converting lateness into minutes for the sake of our graph being easier to understand

late10 <- table2 %>% filter(route %in% c(193, 11, 17, 208, 271, 153, 162, 8, 111, 255)) %>%
  mutate(overall_lateness = overall_lateness/60, weekday_lateness = weekday_lateness/60, weekend_lateness = weekend_lateness/60 ) %>%
  mutate(route=factor(route))

# making a nice cleveland dot plot to show weekend and weekday lateness!
# foundational code via r graph gallery (https://r-graph-gallery.com/303-lollipop-plot-with-2-values.html)
# plotting three values: weekend, weekday, and overall lateness
# some routes only have weekday service so those show up as "overall" only
palette <- c(rgb(0.2,0.7,0.1,0.5), color=rgb(0.7,0.2,0.1,0.5)) 

late10 %>%
  mutate(route = fct_reorder(route, (overall_lateness))) %>%
  ggplot() +
  geom_segment( aes(x=route, xend=route, y=weekday_lateness, yend=weekend_lateness), color="grey")+
  geom_point( aes(x=route, y=weekday_lateness), color=palette[1], size=3 )+
  geom_point( aes(x=route, y=weekend_lateness), color=palette[2], size=3 ) +
  geom_point( aes(x=route, y=overall_lateness), color="black", size=3) +
  coord_flip()+
  theme_minimal() +
  theme(axis.ticks = element_blank()) +
  labs(title = "How Late are the 10 Latest Buses in Seattle?", subtitle = "Buses run later on weekends and earlier on weekdays", x="Route Number", y="Lateness (minutes past scheduled time)")

# note that this ggplot output does not include a legend (not sure why it never seems to work)
# legend and other styling is added in post using google slides