---
title: "Reproducible Research Peer Assessment"
output: 
  html_document:
    keep_md: true
---



## Code for Loading/Reading the data


```r
myfile <- read.csv(unz("activity.zip", "activity.csv"),header=T,)
summary(myfile)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## Code for Preprocessing the data
### Remove Missing values (NA)

```r
x <- as.data.frame(myfile[complete.cases(myfile), ])
summary(x)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```

```r
head(x)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## Histogram of the total number of steps taken each day

```r
require(magrittr)
```

```
## Loading required package: magrittr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps_sum<- x %>% group_by(date) %>% summarize(Total_Steps=sum(steps))
str(steps_sum)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	53 obs. of  2 variables:
##  $ date       : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ Total_Steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
hist(steps_sum$Total_Steps)
```

![](PA1_template1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is mean total number of steps taken per day?
## Mean and median number of steps taken each day

```r
steps_tally<-summary(steps_sum)
str(steps_tally)
```

```
##  'table' chr [1:7, 1:2] "2012-10-02: 1  " "2012-10-03: 1  " ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : chr [1:7] "" "" "" "" ...
##   ..$ : chr [1:2] "        date" " Total_Steps"
```

```r
paste(steps_tally[3,2],steps_tally[4,2],sep='    ')
```

```
## [1] "Median :10765      Mean   :10766  "
```

## What is the average daily activity pattern?
## Time series plot of the average number of steps taken

```r
require(magrittr)
require(dplyr)
require(plotly)
```

```
## Loading required package: plotly
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
require(lubridate)
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
steps_mean <- x %>% group_by(date) %>% summarize(mean_Steps=mean(steps))
str(steps_mean)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	53 obs. of  2 variables:
##  $ date      : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ mean_Steps: num  0.438 39.417 42.069 46.16 53.542 ...
```

```r
plot(steps_mean$mean_Steps~as.factor(substr(as.character(steps_mean$date),6,10)), type="p", xlab="Month-Date", ylab="Daily Average Steps")
lines(steps_mean$mean_Steps)
```

![](PA1_template1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
plot_ly(x = ~substr(as.character(steps_mean$date),6,10), y = ~steps_mean$mean_Steps, mode = 'lines', height=5, xlab="Month-Date", ylab="Daily Average Steps")
```

```
## No trace type specified:
##   Based on info supplied, a 'bar' trace seems appropriate.
##   Read more about this trace type -> https://plot.ly/r/reference/#bar
```

```
## Warning: 'bar' objects don't have these attributes: 'mode', 'xlab', 'ylab'
## Valid attributes include:
## 'type', 'visible', 'showlegend', 'legendgroup', 'opacity', 'name', 'uid', 'ids', 'customdata', 'hoverinfo', 'hoverlabel', 'stream', 'x', 'x0', 'dx', 'y', 'y0', 'dy', 'text', 'hovertext', 'textposition', 'textfont', 'insidetextfont', 'outsidetextfont', 'orientation', 'base', 'offset', 'width', 'marker', 'r', 't', 'error_y', 'error_x', '_deprecated', 'xaxis', 'yaxis', 'xcalendar', 'ycalendar', 'idssrc', 'customdatasrc', 'hoverinfosrc', 'xsrc', 'ysrc', 'textsrc', 'hovertextsrc', 'textpositionsrc', 'basesrc', 'offsetsrc', 'widthsrc', 'rsrc', 'tsrc', 'key', 'set', 'frame', 'transforms', '_isNestedKey', '_isSimpleKey', '_isGraticule'
```

<!--html_preserve--><div id="12847e81491b" style="width:672px;height:5px;" class="plotly html-widget"></div>
<script type="application/json" data-for="12847e81491b">{"x":{"visdat":{"128443463fe8":["function () ","plotlyVisDat"]},"cur_data":"128443463fe8","attrs":{"128443463fe8":{"x":{},"y":{},"mode":"lines","xlab":"Month-Date","ylab":"Daily Average Steps","alpha":1,"sizes":[10,100]}},"layout":{"height":5,"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"title":"substr(as.character(steps_mean$date), 6, 10)","type":"category","categoryorder":"array","categoryarray":["10-02","10-03","10-04","10-05","10-06","10-07","10-09","10-10","10-11","10-12","10-13","10-14","10-15","10-16","10-17","10-18","10-19","10-20","10-21","10-22","10-23","10-24","10-25","10-26","10-27","10-28","10-29","10-30","10-31","11-02","11-03","11-05","11-06","11-07","11-08","11-11","11-12","11-13","11-15","11-16","11-17","11-18","11-19","11-20","11-21","11-22","11-23","11-24","11-25","11-26","11-27","11-28","11-29"]},"yaxis":{"domain":[0,1],"title":"steps_mean$mean_Steps"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"x":["10-02","10-03","10-04","10-05","10-06","10-07","10-09","10-10","10-11","10-12","10-13","10-14","10-15","10-16","10-17","10-18","10-19","10-20","10-21","10-22","10-23","10-24","10-25","10-26","10-27","10-28","10-29","10-30","10-31","11-02","11-03","11-05","11-06","11-07","11-08","11-11","11-12","11-13","11-15","11-16","11-17","11-18","11-19","11-20","11-21","11-22","11-23","11-24","11-25","11-26","11-27","11-28","11-29"],"y":[0.4375,39.4166666666667,42.0694444444444,46.1597222222222,53.5416666666667,38.2465277777778,44.4826388888889,34.375,35.7777777777778,60.3541666666667,43.1458333333333,52.4236111111111,35.2048611111111,52.375,46.7083333333333,34.9166666666667,41.0729166666667,36.09375,30.6284722222222,46.7361111111111,30.9652777777778,29.0104166666667,8.65277777777778,23.5347222222222,35.1354166666667,39.7847222222222,17.4236111111111,34.09375,53.5208333333333,36.8055555555556,36.7048611111111,36.2465277777778,28.9375,44.7326388888889,11.1770833333333,43.7777777777778,37.3784722222222,25.4722222222222,0.142361111111111,18.8923611111111,49.7881944444444,52.4652777777778,30.6979166666667,15.5277777777778,44.3993055555556,70.9270833333333,73.5902777777778,50.2708333333333,41.0902777777778,38.7569444444444,47.3819444444444,35.3576388888889,24.46875],"mode":"lines","xlab":"Month-Date","ylab":"Daily Average Steps","type":"bar","marker":{"fillcolor":"rgba(31,119,180,1)","color":"rgba(31,119,180,1)","line":{"color":"transparent"}},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script><!--/html_preserve-->

```r
ts(steps_mean)
```

```
## Time Series:
## Start = 1 
## End = 53 
## Frequency = 1 
##    date mean_Steps
##  1    2  0.4375000
##  2    3 39.4166667
##  3    4 42.0694444
##  4    5 46.1597222
##  5    6 53.5416667
##  6    7 38.2465278
##  7    9 44.4826389
##  8   10 34.3750000
##  9   11 35.7777778
## 10   12 60.3541667
## 11   13 43.1458333
## 12   14 52.4236111
## 13   15 35.2048611
## 14   16 52.3750000
## 15   17 46.7083333
## 16   18 34.9166667
## 17   19 41.0729167
## 18   20 36.0937500
## 19   21 30.6284722
## 20   22 46.7361111
## 21   23 30.9652778
## 22   24 29.0104167
## 23   25  8.6527778
## 24   26 23.5347222
## 25   27 35.1354167
## 26   28 39.7847222
## 27   29 17.4236111
## 28   30 34.0937500
## 29   31 53.5208333
## 30   33 36.8055556
## 31   34 36.7048611
## 32   36 36.2465278
## 33   37 28.9375000
## 34   38 44.7326389
## 35   39 11.1770833
## 36   42 43.7777778
## 37   43 37.3784722
## 38   44 25.4722222
## 39   46  0.1423611
## 40   47 18.8923611
## 41   48 49.7881944
## 42   49 52.4652778
## 43   50 30.6979167
## 44   51 15.5277778
## 45   52 44.3993056
## 46   53 70.9270833
## 47   54 73.5902778
## 48   55 50.2708333
## 49   56 41.0902778
## 50   57 38.7569444
## 51   58 47.3819444
## 52   59 35.3576389
## 53   60 24.4687500
```

```r
hist(steps_sum$Total_Steps)
```

![](PA1_template1_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
steps_tally<-summary(steps_sum)
str(steps_tally)
```

```
##  'table' chr [1:7, 1:2] "2012-10-02: 1  " "2012-10-03: 1  " ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : chr [1:7] "" "" "" "" ...
##   ..$ : chr [1:2] "        date" " Total_Steps"
```

```r
paste(steps_tally[3,2],steps_tally[4,2],sep='    ')
```

```
## [1] "Median :10765      Mean   :10766  "
```

The 5-minute interval that, on average, contains the maximum number of steps
Histogram of the total number of steps taken each day after missing values are imputed
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

All of the R code needed to reproduce the results (numbers, plots, etc.) in the report






Code to describe and show a strategy for imputing missing data
## Imputing missing values



