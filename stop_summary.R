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

# equivalent of seattle_routes for each stop -- get info about the stop by stop ID
# stop info provided via King County Metro transit stops spatial dataset
# https://gis-kingcounty.opendata.arcgis.com/datasets/kingcounty::transit-stops-for-king-county-metro-transitstop-point/about
stop_info <- read_csv("transit_stops_kcm.csv") %>% clean_names()

# only selecting info we're interested in (some kept here will be removed later)
stop_info <- stop_info %>% 
  select(stop_id, auth_code, auth_name, bearing_code, cf_cross_streetname, hastus_cross_street_name, in_service_flag,
         jurisdiction, kcm_managed_equipment, route_list, on_street_name, num_shelters)

# only want stops that are in service and have routes listed
stop_info <- stop_info %>%
  filter(in_service_flag == "Y") %>%
  filter(is.na(route_list) == F)

# join the stop info to the reliability list
# left join prioritizes stop info dataset (so we can see what's missing from the reliability data)
test <- left_join(stop_info, reliability_list_overall, by="stop_id")

# slightly more cleaning here -- first turning reliability into minutes rather than seconds
# also making a single variable called "stop name" that gives the cross-street info
stop_reliability <- test %>%
  mutate(yrly_rel_mins = yrly_rel/60) %>%
  arrange(desc(yrly_rel_mins)) %>%
  select(-hastus_cross_street_name, -in_service_flag, -auth_code) %>%
  mutate(stop_name = paste0(on_street_name, " & ", cf_cross_streetname)) %>%
  select(-cf_cross_streetname, -on_street_name)

write_csv(stop_reliability, "stop_reliability.csv")

# limited to stops in the city of Seattle
stop_reliability_seattle <- stop_reliability %>%
  filter(jurisdiction == "SEA")

write_csv(stop_reliability_seattle, "stop_reliability_seattle.csv")
