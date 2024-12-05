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
routes_1_99 <- wikitables[[2]] %>% select(-Links)
