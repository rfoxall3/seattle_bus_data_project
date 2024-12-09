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
  list.files(path = "summaries/by_stop/", pattern = ".*\\.csv$",
             full.names = T) %>%
  map_df(~read_plus(.))

# now we need to make that "mutate" column with the filename look nicer!
# here we do a few steps
#   remove lines with numbers over 700 - these are usually school routes or dial-a-ride transit
#   use the filename to extract key info (e.g., janwk1 = first week of january)
#   generate a "date" using this info (e.g., janwk1 -> jan 1 -> 2023-01-01)
# remove the filename (no longer useful)

cleaner_table <- source_table %>%
  mutate(month = substr(source_table$filename,19,21), week = as.numeric(substr(source_table$filename,24,24))) %>%
  mutate(week_start = as.Date(paste0(month,(week*7-6),'.23'),format='%b%d.%y')) %>%
  select(-filename) %>%
  group_by(stop_id, weekday) %>%
  mutate(week_rel = mean(reliability)) %>% ungroup() %>%
  group_by(stop_id) %>%
  mutate(yrly_rel = mean(reliability))

reliability_list_overall <- cleaner_table %>%
  group_by(stop_id) %>%
  summarize(yrly_rel = mean(reliability)) %>%
  arrange(desc(yrly_rel))

write_csv(reliability_list_overall, "reliability_by_stop.csv")

reliability_list_weekday <- cleaner_table %>%
  group_by(stop_id, weekday) %>%
  summarize(week_rel = mean(reliability)) %>%
  arrange(desc(week_rel))

write_csv(reliability_list_weekday, "reliability_weekday_by_stop.csv")
