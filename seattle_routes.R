library(rvest)
library(dplyr)

# method via Rui Barradas on stack overflow (https://stackoverflow.com/questions/73696551/r-webscraping-error-arguments-imply-differing-number-of-rows)

# put in the url for the wikipedia page we want to access
# in this case, the list of all King County Metro bus routes
# then read the url as html format
url <- "https://en.wikipedia.org/wiki/List_of_King_County_Metro_bus_routes"
html <- read_html(url)

# this part i think is creating a variable called wikitables that's in a table format
# the html call picks up all tables in the wiki page
# note that these early parts all use list format instead of dataframes
html %>%
  html_elements(".wikitable") %>%
  html_table() -> wikitables

# here we pick up info about the names (variables) as headers and define them as such
html %>%
  html_elements(".wikitable") %>%
  html_element("caption") %>%
  html_text() %>%
  sub("\\n$", "", .) -> wikitables_names

names(wikitables) <- wikitables_names

# then this bit converts all that info into dataframe formats
html %>%
  html_elements(".wikitable") %>%
  html_table() %>%
  purrr::map(as.data.frame) -> wikitables

# if we select as wikitables[1] we would get the first table as a list of one dataframe
# a list is a pain to work with so the [[1]] syntax gives us an easy dataframe to work with instead of a list
# here we select the second table with routes 1-99 and remove the "links" variable which is useless
routes_1_99 <- wikitables[[2]] %>% select(-Links, -Type, -`Continues as`)
routes_100 <- wikitables[[4]] %>% select(-Links)
routes_200 <- wikitables[[5]] %>% select(-Links)
routes_300 <- wikitables[[6]] %>% select(-Links)
routes_500 <- wikitables[[7]] %>% select(-Links)
# the rapidride (BRT) routes have external letter names but internal numbers
# here we reassign according to internal numbers since we're matching to internal info
numbers <- c("671", "672", "673", "674", "675", "676", "677", "678")
routes_600 <- wikitables[[1]] %>% select(-Links) %>% mutate(Route = numbers) %>% mutate(`Off-Peak` = "Yes", Sat = "Yes", Sun = "Yes")


all_routes <- rbind (routes_1_99, routes_100, routes_200, routes_300, routes_500, routes_600)

# a bunch of these routes are designated local/express/etc.
# in order to join with reliability data these need to be numbers with no other info
# slightly complicated way to do it but basically just renaming routes so that they're:
#   1. just numbered names
#   2. designated in numeric format
all_routes <- all_routes %>% 
  filter(Route != "4 Shuttle") %>%
  mutate(Route = case_when(
    grepl("Express",Route) == T & nchar(Route) == 9 ~ as.numeric(substr(Route,1,1)),
    grepl("Express",Route) == T & nchar(Route) == 10 ~ as.numeric(substr(Route,1,2)),
    grepl("Express",Route) == T & nchar(Route) == 11 ~ as.numeric(substr(Route,1,3)),
    grepl("Local",Route) == T & nchar(Route) == 8 ~ as.numeric(substr(Route,1,2)),
    grepl("DART",Route) == T ~ as.numeric(substr(Route,1,3)),
    grepl("Community",Route) == T ~ as.numeric(substr(Route,1,3)),
    .default = as.numeric(Route)
  ))

# then we can join the route info with the reliability data by the route number!
# note: route 21 has local and express versions
# to keep info on both routes (for now) i used "many to many" matching
reliability_list <- read_csv("reliability_list.csv") %>% rename(route = service_rte_num) %>% filter(route < 800)
test <- left_join(reliability_list, all_routes, by=c("route" = "Route"), relationship = "many-to-many")


# scraping tutorial used: https://statsandr.com/blog/web-scraping-in-r/
# want to check for discontinued lines in current data
url2 <- "https://www.transit.wiki/index.php?title=Category:Discontinued_routes&pagefrom=IBM+Shuttle#mw-pages"
html2 <- read_html(url2)
els <- html2 %>%
  html_elements(css = "div.mw-category-group:nth-child(3) > ul:nth-child(2)")
texts <- html_text(els)

# silly method of writing the text info onto a temporary file 
# this way i can make a dataframe to sort through
# method via petermeissner on stack overflow: https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r
tmp <- tempfile()
write_lines(
  texts,
  tmp,
  sep = "\n")
texts <- as.data.frame(read_lines(tmp)) %>% rename(discontinued = `read_lines(tmp)`)

# then we need to massage this info into a form that we can compare to our reliability list
# of the K lines on the wiki we only want King County Metro lines
# most of those are normal lines but a few are special shuttles e.g. Seahawks shuttle
# also we have a weird / from a dual route so we remove then add both back separately
texts <- texts %>% filter(grepl("King County", discontinued) == T) %>%
  mutate(discontinued = substr(discontinued,19,nchar(discontinued))) %>%
  filter(grepl("Route", discontinued) == T) %>%
  mutate(discontinued = substr(discontinued,7,nchar(discontinued))) %>%
  rename(route = discontinued) %>%
  filter(route != "203/213") 

route <- c("203","213")
slash <- as.data.frame(route)
discontinued_buses <- rbind(texts, slash) %>% mutate(route = as.numeric(route)) %>%
  arrange(route)

# now to see which routes in our 2023 data have since been discontinued (if any)
discont_23 <- inner_join(discontinued_buses, reliability_list, by = "route")

# remove now-discontinued routes (since recommendations/analysis are no longer relevant)
reliability_list <- reliability_list %>%
  filter(!route %in% discont_23$route)

# making a list to export into a nice datawrapper table!
year_reliability_list <- reliability_list %>%
  select(route, yrly_rel) %>%
  distinct(route, .keep_all = TRUE)

year_routes_reliable <- left_join(year_reliability_list, all_routes, by = c("route"="Route"))

write_csv(year_routes_reliable, "reliability_list_cleaned.csv")
