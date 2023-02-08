Lab 05 - Data Wrangling
================

# Learning goals

-   Use the `merge()` function to join two datasets.
-   Deal with missings and impute data.
-   Identify relevant observations using `quantile()`.
-   Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

``` r
library(data.table)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(dtplyr)
library(leaflet)
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.5
    ## ✔ tibble  3.1.8     ✔ stringr 1.4.1
    ## ✔ tidyr   1.2.1     ✔ forcats 0.5.2
    ## ✔ readr   2.1.3     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.8-38. For overview type 'help("mgcv-package")'.

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
dat <- fread("https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz")

# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

``` r
met <- merge(
  # Data
  x = dat,
  y = stations,
  # List of variables to match
  by.x = "USAFID",
  by.y = "USAF",
  # Which obs to keep?
  all.x = TRUE,
  all.y = FALSE
 )

met_lz <- lazy_dt(met, immutable = FALSE)
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
# First compute the average temperature, wind speed, and atmospheric pressure for each weather station
met_avg_lz <- met_lz %>% 
  group_by(USAFID) %>%
  summarise(temp = mean(temp, na.rm = TRUE),
            wind.sp = mean(wind.sp, na.rm = TRUE),
            atm.press = mean(atm.press, na.rm = TRUE)) %>%
  as.data.frame()
```

``` r
# Get the median values
met_med_lz <- met_avg_lz %>%
  summarise(across(
    c(temp, wind.sp, atm.press),
    function(x) quantile(x, 0.5, na.rm = TRUE)
  ))

met_med_lz
```

    ##       temp  wind.sp atm.press
    ## 1 23.68406 2.461838  1014.691

``` r
# Get the stations
temp_us_id <- met_avg_lz %>%
  mutate(temp_diff = abs(temp - met_med_lz %>% pull(temp))) %>%
  arrange(temp_diff) %>%
  slice(1) %>%
  pull(USAFID)

wind.sp_us_id <- met_avg_lz %>%
  mutate(wind.sp_diff = abs(wind.sp - met_med_lz %>% pull(wind.sp))) %>%
  arrange(wind.sp_diff) %>%
  slice(1) %>%
  pull(USAFID)

atm.press_us_id <- met_avg_lz %>%
  mutate(atm.press_diff = abs(atm.press - met_med_lz %>% pull(atm.press))) %>%
  arrange(atm.press_diff) %>%
  slice(1) %>%
  pull(USAFID)

temp_us_id
```

    ## [1] 720458

``` r
wind.sp_us_id
```

    ## [1] 720929

``` r
atm.press_us_id
```

    ## [1] 723200

The three stations are 720458, 720929, and 723200 for temperature, wind
speed, and atmospheric pressure respectively. These three do not
coincide.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
# First compute the average temperature, wind speed, and atmospheric pressure for each weather station
met_means_state <- met_lz %>% 
  group_by(USAFID) %>%
  summarise(temp = mean(temp, na.rm = TRUE),
            wind.sp = mean(wind.sp, na.rm = TRUE),
            atm.press = mean(atm.press, na.rm = TRUE),
            state = STATE,
            lon = lon,
            lat = lat)
```

euclid distance between each variable and the median of the variable

``` r
# Compute the median temperature, wind speed, and atmospheric pressure for each state
met_med_state <- met_means_state %>% 
  group_by(state) %>%
  summarise(across(
    c(temp, wind.sp, atm.press),
    function(x) quantile(x, 0.5, na.rm = TRUE)
  ))
```

``` r
# Merge the two datasets
met_means_med_state <- merge(
  # Data
  x = met_means_state,
  y = met_med_state,
  # List of variables to match
  by.x = "state",
  by.y = "state",
  # Which obs to keep?
  all.x = TRUE,
  all.y = TRUE
 )
```

``` r
# Compute the euclidean distance of each weather station
euclidean <- function(a, b, c, d, e, f){
  sqrt((a -  b)^2 + (c -  d)^2 + (e - f)^2)
}

met_means_med_state$euclid <- euclidean(
  a = met_means_med_state$temp.x,
  b = met_means_med_state$temp.y,
  c = met_means_med_state$wind.sp.x,
  d = met_means_med_state$wind.sp.y,
  e = met_means_med_state$atm.press.x,
  f = met_means_med_state$atm.press.y)
```

``` r
result_state <- met_means_med_state %>% 
  group_by(state) %>%
  slice(which.min(euclid))

result_state
```

    ## # A tibble: 46 × 11
    ## # Groups:   state [46]
    ##    state USAFID temp.x wind.sp.x atm.press.x    lon   lat temp.y wind.…¹ atm.p…²
    ##    <chr>  <int>  <dbl>     <dbl>       <dbl>  <dbl> <dbl>  <dbl>   <dbl>   <dbl>
    ##  1 AL    722286   26.4      1.68       1015.  -87.6  33.2   26.2    1.54   1015.
    ##  2 AR    723407   25.9      2.21       1015.  -90.6  35.8   26.1    1.86   1015.
    ##  3 AZ    723740   27.2      3.68       1012. -111.   35.0   27.7    3.02   1010.
    ##  4 CA    722977   22.3      2.36       1013. -118.   33.7   22.3    2.52   1013.
    ##  5 CO    724665   20.8      3.95       1013. -104.   39.3   20.8    3.09   1013.
    ##  6 CT    725087   22.6      2.13       1015.  -72.7  41.7   22.4    2.08   1015.
    ##  7 DE    724180   24.6      2.75       1015.  -75.6  39.7   24.6    2.75   1015.
    ##  8 FL    722210   27.7      2.53       1015.  -86.5  30.5   27.5    2.53   1015.
    ##  9 GA    723160   26.6      1.68       1015.  -82.5  31.5   26.8    1.43   1015.
    ## 10 IA    725480   21.4      2.76       1015.  -92.4  42.6   21.4    2.63   1015.
    ## # … with 36 more rows, 1 more variable: euclid <dbl>, and abbreviated variable
    ## #   names ¹​wind.sp.y, ²​atm.press.y

``` r
# check for duplicates
length(unique(result_state$state)) == nrow(result_state)
```

    ## [1] TRUE

No duplicates

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
## Get the data necessary to answer the question 
met_means_loc <- met_lz %>% 
  group_by(USAFID) %>%
  summarise(lon = mean(lon, na.rm = TRUE),
            lat = mean(lat, na.rm = TRUE),
            state = STATE)
```

``` r
## Obtain the midpoint of the state using the avg of the lat and lon of the stations
met_med_loc <- met_means_loc %>% 
  group_by(state) %>%
  summarise(across(
    c(lat, lon),
    function(x) mean(x, na.rm = TRUE)
  ))
```

``` r
# Merge the two datasets
met_means_med_loc <- merge(
  # Data
  x = met_means_loc,
  y = met_med_loc,
  # List of variables to match
  by.x = "state",
  by.y = "state",
  # Which obs to keep?
  all.x = TRUE,
  all.y = TRUE
 )
```

``` r
# Compute the station closest to the midpoint
euclidean <- function(a, b, c, d){
  sqrt((a -  b)^2 + (c -  d)^2)
}

met_means_med_loc$euclid <- euclidean(
  a = met_means_med_loc$lon.x,
  b = met_means_med_loc$lon.y,
  c = met_means_med_loc$lat.x,
  d = met_means_med_loc$lat.y)

result_loc <- met_means_med_loc %>% 
  group_by(state) %>%
  slice(which.min(euclid))

result_loc
```

    ## # A tibble: 48 × 7
    ## # Groups:   state [48]
    ##    state USAFID  lon.x lat.x lat.y  lon.y euclid
    ##    <chr>  <int>  <dbl> <dbl> <dbl>  <dbl>  <dbl>
    ##  1 AL    722300  -86.8  33.2  32.8  -86.7 0.442 
    ##  2 AR    723429  -93.1  35.3  35.2  -92.7 0.412 
    ##  3 AZ    723745 -111.   34.3  33.9 -112.  0.396 
    ##  4 CA    723890 -120.   36.8  36.5 -120.  0.416 
    ##  5 CO    726396 -106.   39.1  39.1 -106.  0.202 
    ##  6 CT    725027  -72.8  41.5  41.5  -72.7 0.114 
    ##  7 DE    724088  -75.5  39.1  39.2  -75.5 0.0285
    ##  8 FL    722014  -82.5  28.5  28.3  -82.4 0.149 
    ##  9 GA    722175  -83.6  32.6  32.6  -83.3 0.276 
    ## 10 IA    725466  -93.6  41.7  41.9  -93.5 0.180 
    ## # … with 38 more rows

``` r
# Set color palette
pal <- colorFactor(c("blue", "red"), c("Q2 Representative Station", "Q3 Midpoint Station"))

# Make the plot
result_loc %>% leaflet() |>
  addProviderTiles('OpenStreetMap') |>
  addCircleMarkers(lng = ~lon.x, 
                   lat = ~lat.x, 
                   fillColor = 'red', 
                   weight = 0, 
                   fillOpacity = 0.7, 
                   radius = 10) |>
  addCircleMarkers(lng = ~result_state$lon, 
                   lat = ~result_state$lat, 
                   fillColor = 'blue', 
                   weight = 0, 
                   fillOpacity = 0.7, 
                   radius = 10) |>
  addLegend('bottomleft', 
            pal = pal, 
            values = c("Q2 Representative Station", "Q3 Midpoint Station"), 
            title = "Relative Humidity (%)")
```

<div class="leaflet html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-744aad7e11269737960d" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-744aad7e11269737960d">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircleMarkers","args":[[33.1779980392157,35.2580031347962,34.257,36.7781931187569,39.05,41.5099990825688,39.1329054054054,28.474,32.6332458296752,41.691,44.8890012437811,40.4827108433735,40.711,38.0650046816479,37.578,30.718,41.876,38.981,44.533,43.322,45.1411456215152,38.096,32.3202025316456,45.8054722474977,35.5820807424594,48.39,40.9615540935673,43.5672086956522,40.033,35.0029964747356,38.0510112923463,42.206297648013,40.28,35.4169039487727,42.6,40.849950395399,41.597,33.9646951983298,44.3810077071291,36.009,31.106,40.219,37.358,44.204,47.1038340767172,44.3590049261084,39,43.0643970117396],[-86.7820019607843,-93.0949937304075,-111.339,-119.718720310766,-105.510043030031,-72.8280009174312,-75.4669684684685,-82.4540026064292,-83.5997190517998,-93.566,-116.101,-88.9483614457831,-86.375,-97.861,-84.77,-91.479,-71.021,-76.922,-69.6672303618711,-84.688,-94.507,-92.5528783269962,-90.0789738924051,-108.540024567789,-79.101,-100.024,-98.3142660818714,-71.4325130434783,-74.3501562130178,-105.662009400705,-117.089996235885,-75.980301703163,-83.115,-97.3831921024546,-123.364,-77.849950395399,-71.412,-80.8000501043841,-100.285004816956,-86.52,-98.196,-111.723,-78.438,-72.562,-122.286834076717,-89.8370098522168,-80.274,-108.456947705443],10,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":0,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.7},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[33.212,35.831,35.028,33.68,39.275,41.736,39.674,30.483,31.536,42.554,44.523,41.45,40.971,39.551,37.087,32.516,41.91,39.472,44.45,42.126,45.543,37.152,33.65,45.788,36.047,41.764,43.205,39.366,35.042,40.902,42.643,41.338,35.852,42.07,41.333,41.533,34.498,45.45,35.593,30.3,40.778,38.137,44.534,43.212,39.643,41.317],[-87.616,-90.646,-110.72,-117.866,-103.666,-72.651,-75.606,-86.517,-82.507,-92.401,-114.215,-90.5,-85.206,-97.651,-84.077,-92.041,-70.729,-76.17,-68.367,-86.428,-94.051,-94.495,-88.45,-111.16,-79.477,-96.178,-71.503,-75.078,-106.615,-117.808,-77.056,-84.429,-97.414,-124.29,-75.717,-71.283,-82.71,-98.417,-88.917,-97.7,-111.969,-78.455,-72.614,-90.181,-79.916,-105.683],10,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":0,"opacity":0.5,"fill":true,"fillColor":"blue","fillOpacity":0.7},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#0000FF","#FF0000"],"labels":["Q2 Representative Station","Q3 Midpoint Station"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"bottomleft","type":"factor","title":"Relative Humidity (%)","extra":null,"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[28.474,48.39],"lng":[-124.29,-68.367]}},"evals":[],"jsHooks":[]}</script>

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

-   low: temp \< 20
-   Mid: temp \>= 20 and temp \< 25
-   High: temp \>= 25

``` r
# compute average temperature level
met_state_temp <- met_lz %>% 
  group_by(STATE) %>%
  summarise(na_temp = sum(is.na(temp)),
            temp = mean(temp, na.rm = TRUE),
            wind.sp = mean(wind.sp, na.rm = TRUE),
            atm.press = mean(atm.press, na.rm = TRUE),
            num_stations = length(unique(USAFID, na.rm = TRUE)),
            entries = n())

# 
met_state_temp$avg_temp_level <- case_when(
    met_state_temp %>% pull(temp)< 20 ~ "low",
    met_state_temp %>% pull(temp) >= 20 & met_state_temp %>% pull(temp) < 35 ~ "Mid",
    met_state_temp %>% pull(temp) >= 25 ~ "High"
  )

met_state_temp
```

    ## Source: local data table [48 x 7]
    ## Call:   `_DT1`[, .(na_temp = sum(is.na(temp)), temp = mean(temp, na.rm = TRUE), 
    ##     wind.sp = mean(wind.sp, na.rm = TRUE), atm.press = mean(atm.press, 
    ##         na.rm = TRUE), num_stations = length(unique(USAFID, na.rm = TRUE)), 
    ##     entries = .N), keyby = .(STATE)]
    ## 
    ##   STATE na_temp  temp wind.sp atm.press num_stations entries
    ##   <chr>   <int> <dbl>   <dbl>     <dbl>        <int>   <int>
    ## 1 AL       1400  26.2    1.57     1016.           32   44743
    ## 2 AR       1968  26.2    1.84     1015.           33   34829
    ## 3 AZ        937  28.8    2.98     1011.           32   34150
    ## 4 CA       5040  22.4    2.61     1013.          101  109392
    ## 5 CO       1645  19.5    3.08     1014.           54   78843
    ## 6 CT        305  22.3    2.19     1015.           10   11767
    ## # … with 42 more rows
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Once you are done with that, you can compute the following:

-   Number of entries (records),
-   Number of NA entries,
-   Number of stations,
-   Number of states included, and
-   Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

``` r
met_state_temp %>% 
  group_by(factor(met_state_temp$avg_temp_level)) %>%
  summarise(
    num_entries = sum(entries),
    num_NA = sum(na_temp),
    num_stations = sum(num_stations),
    num_states = length(unique(STATE)),
    mean_temp = mean(temp, na.rm = TRUE),
    mean_wind.sp = mean(wind.sp, na.rm = TRUE),
    mean_atm.press = mean(atm.press, na.rm = TRUE)
  )
```

    ## Source: local data table [2 x 8]
    ## Call:   `_DT1`[, .(na_temp = sum(is.na(temp)), temp = mean(temp, na.rm = TRUE), 
    ##     wind.sp = mean(wind.sp, na.rm = TRUE), atm.press = mean(atm.press, 
    ##         na.rm = TRUE), num_stations = length(unique(USAFID, na.rm = TRUE)), 
    ##     entries = .N), keyby = .(STATE)][, `:=`(`factor(met_state_temp$avg_temp_level)` = factor(..met_state_temp$avg_temp_level))][, 
    ##     .(num_entries = sum(entries), num_NA = sum(na_temp), num_stations = sum(num_stations), 
    ##         num_states = length(unique(STATE)), mean_temp = mean(temp, 
    ##             na.rm = TRUE), mean_wind.sp = mean(wind.sp, na.rm = TRUE), 
    ##         mean_atm.press = mean(atm.press, na.rm = TRUE)), keyby = .(`factor(met_state_temp$avg_temp_level)`)]
    ## 
    ##   factor(met_state_temp…¹ num_e…² num_NA num_s…³ num_s…⁴ mean_…⁵ mean_…⁶ mean_…⁷
    ##   <fct>                     <int>  <int>   <int>   <int>   <dbl>   <dbl>   <dbl>
    ## 1 low                      430794   7369     259      11    18.7    2.55   1014.
    ## 2 Mid                     1946549  52720    1336      37    24.0    2.40   1014.
    ## # … with abbreviated variable names ¹​`factor(met_state_temp$avg_temp_level)`,
    ## #   ²​num_entries, ³​num_stations, ⁴​num_states, ⁵​mean_temp, ⁶​mean_wind.sp,
    ## #   ⁷​mean_atm.press
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

-   using your data with the median values per station, examine the
    association between median temperature (y) and median wind speed
    (x). Create a scatterplot of the two variables using ggplot2. Add
    both a linear regression line and a smooth line.

``` r
# Data with median values per station
met_meds <- met_lz %>% 
  group_by(USAFID) %>%
  summarise(temp = quantile(temp, 0.5, na.rm = TRUE),
            wind.sp = quantile(wind.sp, 0.5, na.rm = TRUE),
            atm.press = quantile(atm.press, 0.5, na.rm = TRUE)) %>%
  as.data.frame()
```

``` r
# Scatterplot of median temperature (y) and median wind speed (x)
met_meds %>% filter(!is.na(temp), !is.na(wind.sp)) %>%
  ggplot( aes(x = wind.sp, y = temp)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red") + 
  geom_smooth()
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

-   fit both a linear model and a spline model (use `gam()` with a cubic
    regression spline on wind speed). Summarize and plot the results
    from the models and interpret which model is the best fit and why.

``` r
# Linear model
lmodel <- lm(met_meds$temp ~ met_meds$wind.sp)
summary(lmodel)
```

    ## 
    ## Call:
    ## lm(formula = met_meds$temp ~ met_meds$wind.sp)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -17.7243  -2.6518  -0.2309   2.7691  14.5052 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      22.23088    0.21779  102.08  < 2e-16 ***
    ## met_meds$wind.sp  0.48614    0.08212    5.92 3.94e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.849 on 1577 degrees of freedom
    ##   (16 observations deleted due to missingness)
    ## Multiple R-squared:  0.02174,    Adjusted R-squared:  0.02112 
    ## F-statistic: 35.05 on 1 and 1577 DF,  p-value: 3.941e-09

``` r
plot(lmodel)
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-20-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-20-4.png)<!-- -->

``` r
# Spline model
smodel <- gam(met_meds$temp~s(met_meds$wind.sp, bs = "cr", k = 10))
summary(smodel)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## met_meds$temp ~ s(met_meds$wind.sp, bs = "cr", k = 10)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 23.38566    0.09549   244.9   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                       edf Ref.df     F p-value    
    ## s(met_meds$wind.sp) 4.798  5.762 13.95  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0486   Deviance explained = 5.15%
    ## GCV = 14.451  Scale est. = 14.398    n = 1579

``` r
plot(smodel)
```

![](README_files/figure-gfm/unnamed-chunk-20-5.png)<!-- --> Notice that
the p-value for both fitted models are very small. However, for the
spline model, the p-value for the wind speed predictor is even smaller,
perhaps indicating more significance. Thus the spline model is better.
