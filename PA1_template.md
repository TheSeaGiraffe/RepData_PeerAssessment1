---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Prior to the analysis, we must first load the data and clean it.


```r
# Load the necessary packages
library(tidyverse)
library(magrittr)
library(zeallot)

# Load the data
activity_data <- read_csv('activity.csv')

# Glimpse the unprocessed data
activity_data %>% glimpse()
```

```
## Observations: 17,568
## Variables: 3
## $ steps    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ date     <date> 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01,…
## $ interval <dbl> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105, 110,…
```

Notice that the `date` column has already been converted into a `Datetime`
object by the `read_csv` function, so our data preprocessing has been handled
for us. We'll leave the `NA`s alone for now.

## What is mean total number of steps taken per day?

In order to obtain this value we need to obtain the total number of steps for
each day.


```r
# Get total number of steps per day
total_steps_per_day <- activity_data %>%
	group_by(date) %>%
	summarize(total_steps = sum(steps))

# Display the values
total_steps_per_day
```

```
## # A tibble: 61 x 2
##    date       total_steps
##    <date>           <dbl>
##  1 2012-10-01          NA
##  2 2012-10-02         126
##  3 2012-10-03       11352
##  4 2012-10-04       12116
##  5 2012-10-05       13294
##  6 2012-10-06       15420
##  7 2012-10-07       11015
##  8 2012-10-08          NA
##  9 2012-10-09       12811
## 10 2012-10-10        9900
## # … with 51 more rows
```

Note the `NA`s. We will be explicitly ignoring them in our calculation of the
mean and median total steps taken per day. By this it is meant that the `NA`
values will be removed from the calculation.


```r
# Get the mean and median total steps
total_steps_per_day %>%
	summarize(mean_total_steps = mean(total_steps, na.rm = T),
			  median_total_steps = median(total_steps, na.rm = T))
```

```
## # A tibble: 1 x 2
##   mean_total_steps median_total_steps
##              <dbl>              <dbl>
## 1           10766.              10765
```

In order to get a sense of the distribution of the total number of steps taken
each day, we'll create a histogram.


```r
# Histogram of the total steps taken per day
total_steps_per_day %>% ggplot(aes(total_steps)) +
	geom_histogram(color = 'black', fill = 'grey') + theme_bw() +
	labs(title = 'Histogram of the Total Number of Steps Per Day',
		 x = 'Steps', y = 'Frequency') +
	theme(plot.title = element_text(hjust = 0.5))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
