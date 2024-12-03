install.packages(c("tidyverse","janitor"))
library(tidyverse)
library(janitor)

### WARNING
# THIS CODE SHOULD NOT BE RUN BY INSTRUCTORS
#
# instead, it serves as an illustration of how i processed the raw data.
# i was given 8.8 GB of raw bus schedule data from king county and cleaned it using the steps below
# the comments explain my process. i would not recommend testing this out with different data formats from other agencies.

########

# read all CSVs for one week
# cleaning:
#   clean names (all lower case)
#   select only the variables desired (date of operation, bus route, direction, stop, times, passenger load)
#   filter out weird outliers like scheduled time of 0
dec22 <- read_csv("December/results_for_2023-12-22.csv") %>% clean_names %>% 
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec23 <- read_csv("December/results_for_2023-12-23.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec24 <- read_csv("December/results_for_2023-12-24.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec25 <- read_csv("December/results_for_2023-12-25.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec26 <- read_csv("December/results_for_2023-12-26.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec27 <- read_csv("December/results_for_2023-12-27.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec28 <- read_csv("December/results_for_2023-12-28.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)

#make a large dataset for analysis by binding 7 datasets together
decwk4 <- rbind(dec22, dec23, dec24, dec25, dec26, dec27, dec28)

# lateness analysis
#   lateness = actual arrival time - scheduled arrival time
#   convert to period (hh:mm:ss format)
#   remove outliers from weird operation where a bus is over an hour late or over an hour early
# also add something to indicate weekend vs weekday service
decwk4 <- decwk4 %>%
  mutate(lateness = first_act_arr_time - sched_arr_time) %>%
  mutate(lateness = seconds_to_period(lateness)) %>%
  filter(lateness < seconds_to_period(as.period(hours(2)))) %>%
  filter(lateness > seconds_to_period(as.period(hours(-2)))) %>%
  mutate(weekday = if_else(weekdays(operation_date) %in% c("Saturday","Sunday"), "weekend", "weekday"))

#save the weekly csv with desired data
write_csv(decwk4,"decwk4.csv")

#make a summary of mean lateness (reliability) by weekday vs weekend service for each stop or route
decwk4_summary <- decwk4 %>% group_by(weekday, stop_id) %>% summarize(reliability = mean(lateness))
decwk4_byroute <- decwk4 %>% group_by(weekday, service_rte_num) %>% summarize(reliability = mean(lateness))

#save the summary csv just in case
write_csv(decwk4_summary, "decwk4_summary.csv")
write_csv(decwk4_byroute, "decwk4_byroute.csv")

#using the plot to check intuition - more buses should be late than early
#horizontal line shows the "on time" condition (0min average lateness)
#plot splits service by weekend vs weekday (not all routes run all the time)
ggplot(decwk4_byroute, aes(x = service_rte_num, y = reliability, color = weekday)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot", x = "Route Number", y = "Average Lateness (minutes)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red"))

## not all data fits neatly into 4 weeks! here's where i process the last few days of the month.
dec29 <- read_csv("December/results_for_2023-12-29.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec30 <- read_csv("December/results_for_2023-12-30.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
dec31 <- read_csv("December/results_for_2023-12-31.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)

#make a large dataset for analysis by binding 7 datasets together
decwk5 <- rbind(dec29, dec30, dec31)

# lateness analysis
#   lateness = actual arrival time - scheduled arrival time
#   convert to period (hh:mm:ss format)
#   remove outliers from weird operation where a bus is over an hour late or over an hour early
# also add something to indicate weekend vs weekday service
decwk5 <- decwk5 %>%
  mutate(lateness = first_act_arr_time - sched_arr_time) %>%
  mutate(lateness = seconds_to_period(lateness)) %>%
  filter(lateness < seconds_to_period(as.period(hours(2)))) %>%
  filter(lateness > seconds_to_period(as.period(hours(-2)))) %>%
  mutate(weekday = if_else(weekdays(operation_date) %in% c("Saturday","Sunday"), "weekend", "weekday"))

#save the weekly csv with desired data
write_csv(decwk5,"decwk5.csv")

#make a summary of mean lateness (reliability) by weekday vs weekend service for each stop or route
decwk5_summary <- decwk5 %>% group_by(weekday, stop_id) %>% summarize(reliability = mean(lateness))
decwk5_byroute <- decwk5 %>% group_by(weekday, service_rte_num) %>% summarize(reliability = mean(lateness))

#save the summary csv just in case
write_csv(decwk5_summary, "decwk5_summary.csv")
write_csv(decwk5_byroute, "decwk5_byroute.csv")

#exploratory plot not recommended for 2-3 days of excess data except to occasionally check work.