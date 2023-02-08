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
```

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
df <- merge(
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
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
summary(df)
```

    ##      USAFID            WBAN            year          month        day    
    ##  Min.   :690150   Min.   :  116   Min.   :2019   Min.   :8   Min.   : 1  
    ##  1st Qu.:720928   1st Qu.: 3706   1st Qu.:2019   1st Qu.:8   1st Qu.: 8  
    ##  Median :722728   Median :13860   Median :2019   Median :8   Median :16  
    ##  Mean   :723100   Mean   :29539   Mean   :2019   Mean   :8   Mean   :16  
    ##  3rd Qu.:725090   3rd Qu.:54767   3rd Qu.:2019   3rd Qu.:8   3rd Qu.:24  
    ##  Max.   :726813   Max.   :94998   Max.   :2019   Max.   :8   Max.   :31  
    ##                                                                          
    ##       hour            min             lat             lon         
    ##  Min.   : 0.00   Min.   : 0.00   Min.   :24.55   Min.   :-124.29  
    ##  1st Qu.: 5.00   1st Qu.:23.00   1st Qu.:33.97   1st Qu.: -98.02  
    ##  Median :11.00   Median :50.00   Median :38.35   Median : -91.71  
    ##  Mean   :11.34   Mean   :39.56   Mean   :37.94   Mean   : -92.15  
    ##  3rd Qu.:17.00   3rd Qu.:55.00   3rd Qu.:41.94   3rd Qu.: -82.99  
    ##  Max.   :23.00   Max.   :59.00   Max.   :48.94   Max.   : -68.31  
    ##                                                                   
    ##       elev           wind.dir      wind.dir.qc        wind.type.code    
    ##  Min.   : -13.0   Min.   :  3      Length:2377343     Length:2377343    
    ##  1st Qu.: 101.0   1st Qu.:120      Class :character   Class :character  
    ##  Median : 252.0   Median :180      Mode  :character   Mode  :character  
    ##  Mean   : 415.8   Mean   :185                                           
    ##  3rd Qu.: 400.0   3rd Qu.:260                                           
    ##  Max.   :9999.0   Max.   :360                                           
    ##                   NA's   :785290                                        
    ##     wind.sp       wind.sp.qc          ceiling.ht     ceiling.ht.qc  
    ##  Min.   : 0.00   Length:2377343     Min.   :    0    Min.   :1.000  
    ##  1st Qu.: 0.00   Class :character   1st Qu.: 3048    1st Qu.:5.000  
    ##  Median : 2.10   Mode  :character   Median :22000    Median :5.000  
    ##  Mean   : 2.46                      Mean   :16166    Mean   :5.027  
    ##  3rd Qu.: 3.60                      3rd Qu.:22000    3rd Qu.:5.000  
    ##  Max.   :36.00                      Max.   :22000    Max.   :9.000  
    ##  NA's   :79693                      NA's   :121275                  
    ##  ceiling.ht.method    sky.cond            vis.dist      vis.dist.qc       
    ##  Length:2377343     Length:2377343     Min.   :     0   Length:2377343    
    ##  Class :character   Class :character   1st Qu.: 16093   Class :character  
    ##  Mode  :character   Mode  :character   Median : 16093   Mode  :character  
    ##                                        Mean   : 14921                     
    ##                                        3rd Qu.: 16093                     
    ##                                        Max.   :160000                     
    ##                                        NA's   :80956                      
    ##    vis.var           vis.var.qc             temp          temp.qc         
    ##  Length:2377343     Length:2377343     Min.   :-40.00   Length:2377343    
    ##  Class :character   Class :character   1st Qu.: 19.60   Class :character  
    ##  Mode  :character   Mode  :character   Median : 23.50   Mode  :character  
    ##                                        Mean   : 23.59                     
    ##                                        3rd Qu.: 27.80                     
    ##                                        Max.   : 56.00                     
    ##                                        NA's   :60089                      
    ##    dew.point      dew.point.qc         atm.press        atm.press.qc  
    ##  Min.   :-37.20   Length:2377343     Min.   : 960.5    Min.   :1.000  
    ##  1st Qu.: 13.80   Class :character   1st Qu.:1011.8    1st Qu.:5.000  
    ##  Median : 18.10   Mode  :character   Median :1014.1    Median :9.000  
    ##  Mean   : 17.02                      Mean   :1014.2    Mean   :7.728  
    ##  3rd Qu.: 21.70                      3rd Qu.:1016.4    3rd Qu.:9.000  
    ##  Max.   : 36.00                      Max.   :1059.9    Max.   :9.000  
    ##  NA's   :66288                       NA's   :1666274                  
    ##        rh             CTRY              STATE          
    ##  Min.   :  0.83   Length:2377343     Length:2377343    
    ##  1st Qu.: 55.79   Class :character   Class :character  
    ##  Median : 76.55   Mode  :character   Mode  :character  
    ##  Mean   : 71.64                                        
    ##  3rd Qu.: 90.63                                        
    ##  Max.   :100.00                                        
    ##  NA's   :66426

``` r
# First compute the average temperature, wind speed, and atmospheric pressure for each weather station
df_means <- df %>% 
  filter(! is.na(temp) & ! is.na(wind.sp) & ! is.na(atm.press)) %>%
  group_by(USAFID) %>%
  summarise(temperature = mean(temp),
            wind_speed = mean(wind.sp),
            pressure = mean(atm.press)) %>%
  as.data.frame()
```

``` r
## Get station with median temperature
median_temp_station <- df_means$USAFID[df_means$temp == quantile(df_means$temp, 0.5)]
median_temp_station
```

    ## [1] 724106

``` r
## Get station with median wind speed
median_wind_speed_station <- df_means$USAFID[df_means$wind_speed == quantile(df_means$wind_speed, 0.5)]
median_wind_speed_station
```

    ## [1] 726070

``` r
## Get station with median atmospheric pressure
median_pressure_station <- df_means$USAFID[df_means$pressure == quantile(df_means$pressure, 0.5)]
median_pressure_station
```

    ## [1] 726539

The three stations that best represent the temperature, wind speed, and
atmospheric pressure across continental US are 724106, 726070, and
726539 respectively.

These three do not coincide.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

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

Once you are done with that, you can compute the following:

-   Number of entries (records),
-   Number of NA entries,
-   Number of stations,
-   Number of states included, and
-   Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

-   using your data with the median values per station, examine the
    association between median temperature (y) and median wind speed
    (x). Create a scatterplot of the two variables using ggplot2. Add
    both a linear regression line and a smooth line.

-   fit both a linear model and a spline model (use `gam()` with a cubic
    regression spline on wind speed). Summarize and plot the results
    from the models and interpret which model is the best fit and why.
