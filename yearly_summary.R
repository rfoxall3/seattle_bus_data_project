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
  select(-filename) 

# here we can select and view a single route's reliability over the year
table1 <- cleaner_table %>%
  filter(service_rte_num == 271)

ggplot(table1, aes(x = week_start, y = reliability, color = weekday)) +
  geom_line() +
  labs(title = "Reliability Over Time", x = "Week Of", y = "Average Lateness (minutes)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red"))
