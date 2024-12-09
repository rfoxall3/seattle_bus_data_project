# seattle_bus_data_project
This repository contains my analysis of King County's bus data. Data was provided by King County Metro after a public records request (thanks!).
Project completed for Data Analysis, Mapping, and Storytelling at NYU Wagner, Fall 2024.

-----

## Directory:

#### data_cleaning.R

  DO NOT RUN (requires access to raw data).
  Processes 365 days of raw data into 4-5 individual weeks per month with summaries grouped by route and by stop ID. If you really, really want the raw data, it's available via Google Drive [here](https://drive.google.com/drive/folders/10L4VCDBqzbiX8hn5lLf72s7h0LZjxbg-?usp=sharing).

#### yearly_summary.R

  DO NOT RUN (requires access to weekly semi-cleaned data). Reads the weekly data and generates a "reliability list" indicating the yearly average reliability by route.
#### stop_summary.R

  Does the same as the yearly_summary.R file except it creates the "reliability list" by stop ID.

#### seattle_routes.R
  
  Scrapes information about routes by number. This info is then added to the reliability list to get a suitable output for a table of top 10 least reliable bus routes with their start/end points included, with discontinued routes removed from the list. Information about routes gathered via Wikipedia (route start/end points) and TransitWiki (for discontinued routes).
