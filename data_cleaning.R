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
mar01 <- read_csv("March/results_for_2023-03-01.csv") %>% clean_names %>% 
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar02 <- read_csv("March/results_for_2023-03-02.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar03 <- read_csv("March/results_for_2023-03-03.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar04 <- read_csv("March/results_for_2023-03-04.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar05 <- read_csv("March/results_for_2023-03-05.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar06 <- read_csv("March/results_for_2023-03-06.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar07 <- read_csv("March/results_for_2023-03-07.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)

#make a large dataset for analysis by binding 7 datasets together
marwk1 <- rbind(mar01, mar02, mar03, mar04, mar05, mar06, mar07)

# lateness analysis
#   lateness = actual arrival time - scheduled arrival time
#   remove outliers from weird operation where a bus is over 1.5 hrs late or over an hour early
# add indication of weekend vs weekday service
# add information about number of stops/routes per day and avg passengers per day
marwk1 <- marwk1 %>%
  mutate(lateness = first_act_arr_time - sched_arr_time) %>%
  filter(lateness > -3600) %>%
  filter(lateness < 5400) %>%
  mutate(weekday = if_else(weekdays(operation_date) %in% c("Saturday","Sunday"), "weekend", "weekday")) %>%
  group_by(operation_date, stop_id) %>% mutate(dly_stops = n(), avg_psg_per_stop = mean(psngr_load)) %>%
  ungroup() %>% group_by(operation_date, service_rte_num) %>% mutate(dly_trips = n()) %>%
  select(-psngr_load, -first_act_dep_time, -first_act_arr_time, -last_act_arr_time, -last_act_dep_time, -sched_arr_time, -sched_dep_time)

#save the weekly csv with desired data
write_csv(marwk1,"03mar/marwk1.csv")

#make a summary of mean lateness (reliability) by weekday vs weekend service for each stop or route
marwk1_summary <- marwk1 %>% group_by(weekday, stop_id) %>% summarize(reliability = mean(lateness))
marwk1_byroute <- marwk1 %>% group_by(weekday, service_rte_num) %>% summarize(reliability = mean(lateness))

#save the summary csv just in case
write_csv(marwk1_summary, "summaries/by_stop/marwk1_summary.csv")
write_csv(marwk1_byroute, "summaries/by_route/marwk1_byroute.csv")

#using the plot to check intuition - more buses should be late than early
#horizontal line shows the "on time" condition (0min average lateness)
#tickmarks are separated by 30 second intervals
#plot splits service by weekend vs weekday (not all routes run all the time)
ggplot(marwk1_byroute, aes(x = service_rte_num, y = reliability, color = weekday)) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(floor(min(marwk1_byroute$reliability)/60)*60, ceiling(max(marwk1_byroute$reliability)/60)*60,by=60))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste0("Scatter Plot of March Week 1"), x = "Route Number", y = "Average Lateness (seconds)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red")) 

# read all CSVs for one week
# cleaning:
#   clean names (all lower case)
#   select only the variables desired (date of operation, bus route, direction, stop, times, passenger load)
#   filter out weird outliers like scheduled time of 0
mar08 <- read_csv("March/results_for_2023-03-08.csv") %>% clean_names %>% 
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar09 <- read_csv("March/results_for_2023-03-09.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar10 <- read_csv("March/results_for_2023-03-10.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar11 <- read_csv("March/results_for_2023-03-11.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar12 <- read_csv("March/results_for_2023-03-12.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar13 <- read_csv("March/results_for_2023-03-13.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar14 <- read_csv("March/results_for_2023-03-14.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)

#make a large dataset for analysis by binding 7 datasets together
marwk2 <- rbind(mar08, mar09, mar10, mar11, mar12, mar13, mar14)

# lateness analysis
#   lateness = actual arrival time - scheduled arrival time
#   remove outliers from weird operation where a bus is over 1.5 hrs late or over an hour early
# add indication of weekend vs weekday service
# add information about number of stops/routes per day and avg passengers per day
marwk2 <- marwk2 %>%
  mutate(lateness = first_act_arr_time - sched_arr_time) %>%
  filter(lateness > -3600) %>%
  filter(lateness < 5400) %>%
  mutate(weekday = if_else(weekdays(operation_date) %in% c("Saturday","Sunday"), "weekend", "weekday")) %>%
  group_by(operation_date, stop_id) %>% mutate(dly_stops = n(), avg_psg_per_stop = mean(psngr_load)) %>%
  ungroup() %>% group_by(operation_date, service_rte_num) %>% mutate(dly_trips = n()) %>%
  select(-psngr_load, -first_act_dep_time, -first_act_arr_time, -last_act_arr_time, -last_act_dep_time, -sched_arr_time, -sched_dep_time)

#save the weekly csv with desired data
write_csv(marwk2,"03mar/marwk2.csv")

#make a summary of mean lateness (reliability) by weekday vs weekend service for each stop or route
marwk2_summary <- marwk2 %>% group_by(weekday, stop_id) %>% summarize(reliability = mean(lateness))
marwk2_byroute <- marwk2 %>% group_by(weekday, service_rte_num) %>% summarize(reliability = mean(lateness))

#save the summary csv just in case
write_csv(marwk2_summary, "summaries/by_stop/marwk2_summary.csv")
write_csv(marwk2_byroute, "summaries/by_route/marwk2_byroute.csv")

#using the plot to check intuition - more buses should be late than early
#horizontal line shows the "on time" condition (0min average lateness)
#tickmarks are separated by 30 second intervals
#plot splits service by weekend vs weekday (not all routes run all the time)
ggplot(marwk2_byroute, aes(x = service_rte_num, y = reliability, color = weekday)) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(floor(min(marwk2_byroute$reliability)/60)*60, ceiling(max(marwk2_byroute$reliability)/60)*60,by=60))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot of March Week 2", x = "Route Number", y = "Average Lateness (seconds)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red")) 

# read all CSVs for one week
# cleaning:
#   clean names (all lower case)
#   select only the variables desired (date of operation, bus route, direction, stop, times, passenger load)
#   filter out weird outliers like scheduled time of 0
mar15 <- read_csv("March/results_for_2023-03-15.csv") %>% clean_names %>% 
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar16 <- read_csv("March/results_for_2023-03-16.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar17 <- read_csv("March/results_for_2023-03-17.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar18 <- read_csv("March/results_for_2023-03-18.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar19 <- read_csv("March/results_for_2023-03-19.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar20 <- read_csv("March/results_for_2023-03-20.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar21 <- read_csv("March/results_for_2023-03-21.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)

#make a large dataset for analysis by binding 7 datasets together
marwk3 <- rbind(mar15, mar16, mar17, mar18, mar19, mar20, mar21)

# lateness analysis
#   lateness = actual arrival time - scheduled arrival time
#   remove outliers from weird operation where a bus is over 1.5 hrs late or over an hour early
# add indication of weekend vs weekday service
# add information about number of stops/routes per day and avg passengers per day
marwk3 <- marwk3 %>%
  mutate(lateness = first_act_arr_time - sched_arr_time) %>%
  filter(lateness > -3600) %>%
  filter(lateness < 5400) %>%
  mutate(weekday = if_else(weekdays(operation_date) %in% c("Saturday","Sunday"), "weekend", "weekday")) %>%
  group_by(operation_date, stop_id) %>% mutate(dly_stops = n(), avg_psg_per_stop = mean(psngr_load)) %>%
  ungroup() %>% group_by(operation_date, service_rte_num) %>% mutate(dly_trips = n()) %>%
  select(-psngr_load, -first_act_dep_time, -first_act_arr_time, -last_act_arr_time, -last_act_dep_time, -sched_arr_time, -sched_dep_time)

#save the weekly csv with desired data
write_csv(marwk3,"03mar/marwk3.csv")

#make a summary of mean lateness (reliability) by weekday vs weekend service for each stop or route
marwk3_summary <- marwk3 %>% group_by(weekday, stop_id) %>% summarize(reliability = mean(lateness))
marwk3_byroute <- marwk3 %>% group_by(weekday, service_rte_num) %>% summarize(reliability = mean(lateness))

#save the summary csv just in case
write_csv(marwk3_summary, "summaries/by_stop/marwk3_summary.csv")
write_csv(marwk3_byroute, "summaries/by_route/marwk3_byroute.csv")

#using the plot to check intuition - more buses should be late than early
#horizontal line shows the "on time" condition (0min average lateness)
#tickmarks are separated by 30 second intervals
#plot splits service by weekend vs weekday (not all routes run all the time)
ggplot(marwk3_byroute, aes(x = service_rte_num, y = reliability, color = weekday)) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(floor(min(marwk3_byroute$reliability)/60)*60, ceiling(max(marwk3_byroute$reliability)/60)*60,by=60))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot of March Week 3", x = "Route Number", y = "Average Lateness (seconds)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red")) 

# read all CSVs for one week
# cleaning:
#   clean names (all lower case)
#   select only the variables desired (date of operation, bus route, direction, stop, times, passenger load)
#   filter out weird outliers like scheduled time of 0
mar22 <- read_csv("March/results_for_2023-03-22.csv") %>% clean_names %>% 
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar23 <- read_csv("March/results_for_2023-03-23.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar24 <- read_csv("March/results_for_2023-03-24.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar25 <- read_csv("March/results_for_2023-03-25.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar26 <- read_csv("March/results_for_2023-03-26.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar27 <- read_csv("March/results_for_2023-03-27.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar28 <- read_csv("March/results_for_2023-03-28.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)

#make a large dataset for analysis by binding 7 datasets together
marwk4 <- rbind(mar22, mar23, mar24, mar25, mar26, mar27, mar28)

# lateness analysis
#   lateness = actual arrival time - scheduled arrival time
#   remove outliers from weird operation where a bus is over 1.5 hrs late or over an hour early
# add indication of weekend vs weekday service
# add information about number of stops/routes per day and avg passengers per day
marwk4 <- marwk4 %>%
  mutate(lateness = first_act_arr_time - sched_arr_time) %>%
  filter(lateness > -3600) %>%
  filter(lateness < 5400) %>%
  mutate(weekday = if_else(weekdays(operation_date) %in% c("Saturday","Sunday"), "weekend", "weekday")) %>%
  group_by(operation_date, stop_id) %>% mutate(dly_stops = n(), avg_psg_per_stop = mean(psngr_load)) %>%
  ungroup() %>% group_by(operation_date, service_rte_num) %>% mutate(dly_trips = n()) %>%
  select(-psngr_load, -first_act_dep_time, -first_act_arr_time, -last_act_arr_time, -last_act_dep_time, -sched_arr_time, -sched_dep_time)

#save the weekly csv with desired data
write_csv(marwk4,"03mar/marwk4.csv")

#make a summary of mean lateness (reliability) by weekday vs weekend service for each stop or route
marwk4_summary <- marwk4 %>% group_by(weekday, stop_id) %>% summarize(reliability = mean(lateness))
marwk4_byroute <- marwk4 %>% group_by(weekday, service_rte_num) %>% summarize(reliability = mean(lateness))

#save the summary csv just in case
write_csv(marwk4_summary, "summaries/by_stop/marwk4_summary.csv")
write_csv(marwk4_byroute, "summaries/by_route/marwk4_byroute.csv")

#using the plot to check intuition - more buses should be late than early
#horizontal line shows the "on time" condition (0min average lateness)
#tickmarks are separated by 30 second intervals
#plot splits service by weekend vs weekday (not all routes run all the time)
ggplot(marwk4_byroute, aes(x = service_rte_num, y = reliability, color = weekday)) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(floor(min(marwk4_byroute$reliability)/60)*60, ceiling(max(marwk4_byroute$reliability)/60)*60,by=60))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot of March Week 4", x = "Route Number", y = "Average Lateness (seconds)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red")) 

## not all data fits neatly into 4 weeks! here's where i process the last few days of the month.
mar29 <- read_csv("March/results_for_2023-03-29.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar30 <- read_csv("March/results_for_2023-03-30.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)
mar31 <- read_csv("March/results_for_2023-03-31.csv") %>% clean_names %>%
  select(operation_date,service_rte_num,patt_direction,stop_id, 
         first_act_dep_time, first_act_arr_time, last_act_arr_time, last_act_dep_time,
         sched_arr_time, sched_dep_time, psngr_load) %>%
  filter(sched_arr_time > 0)

#make a large dataset for analysis by binding 7 datasets together
marwk5 <- rbind(mar29, mar30, mar31)

# lateness analysis
#   lateness = actual arrival time - scheduled arrival time
#   remove outliers from weird operation where a bus is over 1.5 hrs late or over an hour early
# add indication of weekend vs weekday service
# add information about number of stops/routes per day and avg passengers per day
marwk5 <- marwk5 %>%
  mutate(lateness = first_act_arr_time - sched_arr_time) %>%
  filter(lateness > -3600) %>%
  filter(lateness < 5400) %>%
  select(operation_date,service_rte_num,patt_direction,stop_id,psngr_load,lateness) %>%
  mutate(weekday = if_else(weekdays(operation_date) %in% c("Saturday","Sunday"), "weekend", "weekday")) %>%
  group_by(operation_date, stop_id) %>% mutate(dly_stops = n(), avg_psg_per_stop = mean(psngr_load)) %>%
  ungroup() %>% group_by(operation_date, service_rte_num) %>% mutate(dly_trips = n()) %>%
  select(-psngr_load)

#save the weekly csv with desired data
write_csv(marwk5,"03mar/marwk5.csv")

#make a summary of mean lateness (reliability) by weekday vs weekend service for each stop or route
marwk5_summary <- marwk5 %>% group_by(weekday, stop_id) %>% summarize(reliability = mean(lateness))
marwk5_byroute <- marwk5 %>% group_by(weekday, service_rte_num) %>% summarize(reliability = mean(lateness))

#save the summary csv 
write_csv(marwk5_summary, "summaries/by_stop/marwk5_summary.csv")
write_csv(marwk5_byroute, "summaries/by_route/marwk5_byroute.csv")

#exploratory plot not recommended for 2-3 days of excess data except to occasionally check work.
#using the plot to check intuition - more buses should be late than early
#horizontal line shows the "on time" condition (0min average lateness)
#tickmarks are separated by 30 second intervals
#plot splits service by weekend vs weekday (not all routes run all the time)
ggplot(marwk5_byroute, aes(x = service_rte_num, y = reliability, color = weekday)) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(floor(min(marwk5_byroute$reliability)/60)*60, ceiling(max(marwk5_byroute$reliability)/60)*60,by=60))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot of March Week 5", x = "Route Number", y = "Average Lateness (seconds)") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red")) 
