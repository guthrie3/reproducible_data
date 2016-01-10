# Reproducible Research: Peer Assessment 1
Charles Guthrie  
January 9, 2016  

#Overview
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.  

##Loading and preprocessing the data

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  

The data was downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip on to a local drive for processing and analyses using the statistical program R (version 3.2.1).  The dataset consists of 17,568 observations and three variables--steps (number of steps taking in a 5-minute interval with missing values are coded as NA), date (the date on which the measurement was taken in YYYY-MM-DD format), and interval (identifier for the 5-minute interval in which measurement was taken).


```r
###code for loading and inital processing of dataset
##packages used: 
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggvis)
```

```
## Warning: package 'ggvis' was built under R version 3.2.3
```

```r
##load data 
wk1_data <- read.csv(file = "activity.csv", header = T, stringsAsFactors = F) 
wk1_data <- tbl_df(wk1_data)
wk1_data <- wk1_data %>% mutate(date = as.Date(date)) #convert character data type to date data type 
str(wk1_data)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(wk1_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

#Questions
The present dataset was used to answer the questions in the following sections.  

##What is mean total number of steps taken per day?
  


```r
##what is the mean total of steps taken per day?
steps_daily <- wk1_data %>% group_by(date) %>% summarise(steps_daily = sum(steps, na.rm =T))
steps_daily %>% ggvis(~steps_daily) %>% layer_histograms() %>% add_axis("x", title = "Graph 1: Step Daily")
```

```
## Guessing width = 1000 # range / 22
```

<!--html_preserve--><div id="plot_id210416625-container" class="ggvis-output-container">
<div id="plot_id210416625" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id210416625_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id210416625" data-renderer="svg">SVG</a>
 | 
<a id="plot_id210416625_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id210416625" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id210416625_download" class="ggvis-download" data-plot-id="plot_id210416625">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id210416625_spec = {
  "data": [
    {
      "name": ".0/bin1/stack2",
      "format": {
        "type": "csv",
        "parse": {
          "xmin_": "number",
          "xmax_": "number",
          "stack_upr_": "number",
          "stack_lwr_": "number"
        }
      },
      "values": "\"xmin_\",\"xmax_\",\"stack_upr_\",\"stack_lwr_\"\n-500,500,10,0\n500,1500,0,0\n1500,2500,1,0\n2500,3500,1,0\n3500,4500,1,0\n4500,5500,2,0\n5500,6500,0,0\n6500,7500,3,0\n7500,8500,2,0\n8500,9500,3,0\n9500,10500,9,0\n10500,11500,7,0\n11500,12500,4,0\n12500,13500,7,0\n13500,14500,3,0\n14500,15500,5,0\n15500,16500,0,0\n16500,17500,1,0\n17500,18500,0,0\n18500,19500,0,0\n19500,20500,1,0\n20500,21500,1,0"
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-1600\n22600"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n0\n10.5"
    }
  ],
  "scales": [
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "rect",
      "properties": {
        "update": {
          "stroke": {
            "value": "#000000"
          },
          "fill": {
            "value": "#333333"
          },
          "x": {
            "scale": "x",
            "field": "data.xmin_"
          },
          "x2": {
            "scale": "x",
            "field": "data.xmax_"
          },
          "y": {
            "scale": "y",
            "field": "data.stack_upr_"
          },
          "y2": {
            "scale": "y",
            "field": "data.stack_lwr_"
          }
        },
        "ggvis": {
          "data": {
            "value": ".0/bin1/stack2"
          }
        }
      },
      "from": {
        "data": ".0/bin1/stack2"
      }
    }
  ],
  "legends": [],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "title": "Graph 1: Step Daily",
      "layer": "back",
      "grid": true
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "count"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id210416625").parseSpec(plot_id210416625_spec);
</script><!--/html_preserve-->

```r
summary_steps_daily <- steps_daily %>% summarise(avg = mean(steps_daily, na.rm = T), median = median(steps_daily, na.rm =T))
summary_steps_daily
```

```
## Source: local data frame [1 x 2]
## 
##       avg median
##     (dbl)  (int)
## 1 9354.23  10395
```

The dataset was grouped by unique dates and each date's recorded steps were summed (graph 1 shows the distribution of the total steps).  The mean (9354.2295082) and median (10395) were also calculated.


##What is the average daily activity pattern?

```r
##what is the average daily activity pattern? 
avg_daily <- wk1_data %>% group_by(interval) %>% summarise(avg = mean(steps, na.rm = T))
avg_daily %>% ggvis(~interval, ~avg) %>% layer_lines()
```

<!--html_preserve--><div id="plot_id979734680-container" class="ggvis-output-container">
<div id="plot_id979734680" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id979734680_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id979734680" data-renderer="svg">SVG</a>
 | 
<a id="plot_id979734680_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id979734680" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id979734680_download" class="ggvis-download" data-plot-id="plot_id979734680">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id979734680_spec = {
  "data": [
    {
      "name": ".0/arrange1",
      "format": {
        "type": "csv",
        "parse": {
          "interval": "number",
          "avg": "number"
        }
      },
      "values": "\"interval\",\"avg\"\n0,1.71698113207547\n5,0.339622641509434\n10,0.132075471698113\n15,0.150943396226415\n20,0.0754716981132075\n25,2.09433962264151\n30,0.528301886792453\n35,0.867924528301887\n40,0\n45,1.47169811320755\n50,0.30188679245283\n55,0.132075471698113\n100,0.320754716981132\n105,0.679245283018868\n110,0.150943396226415\n115,0.339622641509434\n120,0\n125,1.11320754716981\n130,1.83018867924528\n135,0.169811320754717\n140,0.169811320754717\n145,0.377358490566038\n150,0.264150943396226\n155,0\n200,0\n205,0\n210,1.13207547169811\n215,0\n220,0\n225,0.132075471698113\n230,0\n235,0.226415094339623\n240,0\n245,0\n250,1.54716981132075\n255,0.943396226415094\n300,0\n305,0\n310,0\n315,0\n320,0.207547169811321\n325,0.622641509433962\n330,1.62264150943396\n335,0.584905660377358\n340,0.490566037735849\n345,0.0754716981132075\n350,0\n355,0\n400,1.18867924528302\n405,0.943396226415094\n410,2.56603773584906\n415,0\n420,0.339622641509434\n425,0.358490566037736\n430,4.11320754716981\n435,0.660377358490566\n440,3.49056603773585\n445,0.830188679245283\n450,3.11320754716981\n455,1.11320754716981\n500,0\n505,1.56603773584906\n510,3\n515,2.24528301886792\n520,3.32075471698113\n525,2.9622641509434\n530,2.09433962264151\n535,6.05660377358491\n540,16.0188679245283\n545,18.3396226415094\n550,39.4528301886792\n555,44.4905660377358\n600,31.4905660377358\n605,49.2641509433962\n610,53.7735849056604\n615,63.4528301886792\n620,49.9622641509434\n625,47.0754716981132\n630,52.1509433962264\n635,39.3396226415094\n640,44.0188679245283\n645,44.1698113207547\n650,37.3584905660377\n655,49.0377358490566\n700,43.811320754717\n705,44.377358490566\n710,50.5094339622642\n715,54.5094339622642\n720,49.9245283018868\n725,50.9811320754717\n730,55.6792452830189\n735,44.3207547169811\n740,52.2641509433962\n745,69.5471698113208\n750,57.8490566037736\n755,56.1509433962264\n800,73.377358490566\n805,68.2075471698113\n810,129.433962264151\n815,157.528301886792\n820,171.150943396226\n825,155.396226415094\n830,177.301886792453\n835,206.169811320755\n840,195.924528301887\n845,179.566037735849\n850,183.396226415094\n855,167.018867924528\n900,143.452830188679\n905,124.037735849057\n910,109.11320754717\n915,108.11320754717\n920,103.716981132075\n925,95.9622641509434\n930,66.2075471698113\n935,45.2264150943396\n940,24.7924528301887\n945,38.7547169811321\n950,34.9811320754717\n955,21.0566037735849\n1000,40.5660377358491\n1005,26.9811320754717\n1010,42.4150943396226\n1015,52.6603773584906\n1020,38.9245283018868\n1025,50.7924528301887\n1030,44.2830188679245\n1035,37.4150943396226\n1040,34.6981132075472\n1045,28.3396226415094\n1050,25.0943396226415\n1055,31.9433962264151\n1100,31.3584905660377\n1105,29.6792452830189\n1110,21.3207547169811\n1115,25.5471698113208\n1120,28.377358490566\n1125,26.4716981132075\n1130,33.4339622641509\n1135,49.9811320754717\n1140,42.0377358490566\n1145,44.6037735849057\n1150,46.0377358490566\n1155,59.188679245283\n1200,63.8679245283019\n1205,87.6981132075472\n1210,94.8490566037736\n1215,92.7735849056604\n1220,63.3962264150943\n1225,50.1698113207547\n1230,54.4716981132075\n1235,32.4150943396226\n1240,26.5283018867925\n1245,37.7358490566038\n1250,45.0566037735849\n1255,67.2830188679245\n1300,42.3396226415094\n1305,39.8867924528302\n1310,43.2641509433962\n1315,40.9811320754717\n1320,46.2452830188679\n1325,56.4339622641509\n1330,42.7547169811321\n1335,25.1320754716981\n1340,39.9622641509434\n1345,53.5471698113208\n1350,47.3207547169811\n1355,60.811320754717\n1400,55.7547169811321\n1405,51.9622641509434\n1410,43.5849056603774\n1415,48.6981132075472\n1420,35.4716981132075\n1425,37.5471698113208\n1430,41.8490566037736\n1435,27.5094339622642\n1440,17.1132075471698\n1445,26.0754716981132\n1450,43.622641509434\n1455,43.7735849056604\n1500,30.0188679245283\n1505,36.0754716981132\n1510,35.4905660377358\n1515,38.8490566037736\n1520,45.9622641509434\n1525,47.7547169811321\n1530,48.1320754716981\n1535,65.3207547169811\n1540,82.9056603773585\n1545,98.6603773584906\n1550,102.11320754717\n1555,83.9622641509434\n1600,62.1320754716981\n1605,64.1320754716981\n1610,74.5471698113208\n1615,63.1698113207547\n1620,56.9056603773585\n1625,59.7735849056604\n1630,43.8679245283019\n1635,38.5660377358491\n1640,44.6603773584906\n1645,45.4528301886792\n1650,46.2075471698113\n1655,43.6792452830189\n1700,46.622641509434\n1705,56.3018867924528\n1710,50.7169811320755\n1715,61.2264150943396\n1720,72.7169811320755\n1725,78.9433962264151\n1730,68.9433962264151\n1735,59.6603773584906\n1740,75.0943396226415\n1745,56.5094339622642\n1750,34.7735849056604\n1755,37.4528301886792\n1800,40.6792452830189\n1805,58.0188679245283\n1810,74.6981132075472\n1815,85.3207547169811\n1820,59.2641509433962\n1825,67.7735849056604\n1830,77.6981132075472\n1835,74.2452830188679\n1840,85.3396226415094\n1845,99.4528301886792\n1850,86.5849056603774\n1855,85.6037735849057\n1900,84.8679245283019\n1905,77.8301886792453\n1910,58.0377358490566\n1915,53.3584905660377\n1920,36.3207547169811\n1925,20.7169811320755\n1930,27.3962264150943\n1935,40.0188679245283\n1940,30.2075471698113\n1945,25.5471698113208\n1950,45.6603773584906\n1955,33.5283018867925\n2000,19.622641509434\n2005,19.0188679245283\n2010,19.3396226415094\n2015,33.3396226415094\n2020,26.811320754717\n2025,21.1698113207547\n2030,27.3018867924528\n2035,21.3396226415094\n2040,19.5471698113208\n2045,21.3207547169811\n2050,32.3018867924528\n2055,20.1509433962264\n2100,15.9433962264151\n2105,17.2264150943396\n2110,23.4528301886792\n2115,19.2452830188679\n2120,12.4528301886792\n2125,8.0188679245283\n2130,14.6603773584906\n2135,16.3018867924528\n2140,8.67924528301887\n2145,7.79245283018868\n2150,8.13207547169811\n2155,2.62264150943396\n2200,1.45283018867925\n2205,3.67924528301887\n2210,4.81132075471698\n2215,8.50943396226415\n2220,7.07547169811321\n2225,8.69811320754717\n2230,9.75471698113208\n2235,2.20754716981132\n2240,0.320754716981132\n2245,0.113207547169811\n2250,1.60377358490566\n2255,4.60377358490566\n2300,3.30188679245283\n2305,2.84905660377358\n2310,0\n2315,0.830188679245283\n2320,0.962264150943396\n2325,1.58490566037736\n2330,2.60377358490566\n2335,4.69811320754717\n2340,3.30188679245283\n2345,0.641509433962264\n2350,0.226415094339623\n2355,1.07547169811321"
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-117.75\n2472.75"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-10.3084905660377\n216.478301886792"
    }
  ],
  "scales": [
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "line",
      "properties": {
        "update": {
          "stroke": {
            "value": "#000000"
          },
          "x": {
            "scale": "x",
            "field": "data.interval"
          },
          "y": {
            "scale": "y",
            "field": "data.avg"
          }
        },
        "ggvis": {
          "data": {
            "value": ".0/arrange1"
          }
        }
      },
      "from": {
        "data": ".0/arrange1"
      }
    }
  ],
  "legends": [],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "layer": "back",
      "grid": true,
      "title": "interval"
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "avg"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id979734680").parseSpec(plot_id979734680_spec);
</script><!--/html_preserve-->

```r
#Calculate and report the mean and median of the total number of steps taken per day
max_interval <- avg_daily[max(avg_daily$avg),]
```

A time series plot was created to display the average number of steps taken across the five-minute intervals (see Graph 2).  The five minute interval, on average across all days in the data, that contained the maxim number of steps was found to be 1705 at an average of 56.3018868 steps. 

##Imputing missing values
A brief missing data analysis was conducted: the table below shows the counts of missing data (NA) by variable in the data. 


```r
##Imputing missing values and answer previous questions.
wk1_data %>% summarise(NAs_steps = sum(is.na(steps)), NA_date = sum(is.na(date)), NA_interval = sum(is.na(interval))) #number of NAs by variable
```

```
## Source: local data frame [1 x 3]
## 
##   NAs_steps NA_date NA_interval
##       (int)   (int)       (int)
## 1      2304       0           0
```

   


```r
#aggregated averages by unique interval number 
means_by_interval <- wk1_data %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm = T)) %>% as.data.frame()
#function to impute missing data with mean of corresponding interval group 
impute_data <- function(steps, interval){
  imputed_result <- NA
  if(!is.na(steps)){
    imputed_result <- c(steps)
  }else{
    imputed_result <- (means_by_interval[means_by_interval$interval==interval, "steps"])
  }
  return(imputed_result)
}
wk1_data_imputed <- wk1_data
wk1_data_imputed$steps <- mapply(impute_data, wk1_data_imputed$steps, wk1_data_imputed$interval)
wk1_data_imputed
```

```
## Source: local data frame [17,568 x 3]
## 
##        steps       date interval
##        (dbl)     (date)    (int)
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
## ..       ...        ...      ...
```

```r
#
steps_daily_imputed <- wk1_data_imputed %>% group_by(date) %>% summarise(steps_daily_imputed = sum(steps, na.rm = T))
steps_daily_imputed %>% ggvis(~steps_daily_imputed) %>% layer_histograms() %>% add_axis("x", title = "Graph 3: Imputed Daily Steps Data")
```

```
## Guessing width = 1000 # range / 22
```

<!--html_preserve--><div id="plot_id440570508-container" class="ggvis-output-container">
<div id="plot_id440570508" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id440570508_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id440570508" data-renderer="svg">SVG</a>
 | 
<a id="plot_id440570508_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id440570508" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id440570508_download" class="ggvis-download" data-plot-id="plot_id440570508">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id440570508_spec = {
  "data": [
    {
      "name": ".0/bin1/stack2",
      "format": {
        "type": "csv",
        "parse": {
          "xmin_": "number",
          "xmax_": "number",
          "stack_upr_": "number",
          "stack_lwr_": "number"
        }
      },
      "values": "\"xmin_\",\"xmax_\",\"stack_upr_\",\"stack_lwr_\"\n-500,500,2,0\n500,1500,0,0\n1500,2500,1,0\n2500,3500,1,0\n3500,4500,1,0\n4500,5500,2,0\n5500,6500,0,0\n6500,7500,3,0\n7500,8500,2,0\n8500,9500,3,0\n9500,10500,9,0\n10500,11500,15,0\n11500,12500,4,0\n12500,13500,7,0\n13500,14500,3,0\n14500,15500,5,0\n15500,16500,0,0\n16500,17500,1,0\n17500,18500,0,0\n18500,19500,0,0\n19500,20500,1,0\n20500,21500,1,0"
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-1600\n22600"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n0\n15.75"
    }
  ],
  "scales": [
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "rect",
      "properties": {
        "update": {
          "stroke": {
            "value": "#000000"
          },
          "fill": {
            "value": "#333333"
          },
          "x": {
            "scale": "x",
            "field": "data.xmin_"
          },
          "x2": {
            "scale": "x",
            "field": "data.xmax_"
          },
          "y": {
            "scale": "y",
            "field": "data.stack_upr_"
          },
          "y2": {
            "scale": "y",
            "field": "data.stack_lwr_"
          }
        },
        "ggvis": {
          "data": {
            "value": ".0/bin1/stack2"
          }
        }
      },
      "from": {
        "data": ".0/bin1/stack2"
      }
    }
  ],
  "legends": [],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "title": "Graph 3: Imputed Daily Steps Data",
      "layer": "back",
      "grid": true
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "count"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id440570508").parseSpec(plot_id440570508_spec);
</script><!--/html_preserve-->

```r
summary_steps_daily_imputed <- steps_daily_imputed %>% summarise(avg = mean(steps_daily_imputed, na.rm = T), median = median(steps_daily_imputed, na.rm = T))
summary_steps_daily_imputed
```

```
## Source: local data frame [1 x 2]
## 
##        avg   median
##      (dbl)    (dbl)
## 1 10766.19 10766.19
```

Missing data was imputed by replacing missing values (i.e. NA) with the mean value from the observations aggregate interval group.  The previous questions were addressed with the imputed data. Graph 3 shows the distribution the total daily steps and the mean (1.0766189\times 10^{4}) and median (1.0766189\times 10^{4}) were calculated for the imputed data.  Imputing the data with the chosen method made the distribution of the steps conform to a more normal distribution and may distort the interpretation of the data. 

##Are there differences in activity patterns between weekdays and weekends?

A line graph was created to explore the differences in activity patterns between weekdays and weekends (Graph 4).  The graph indicates that there are larger averages of steps during the early intervals during the weekdays when compared to weekends.  However, step averages are more consistent across intervals during the weekends.  


```r
##Are there differences in activity patterns between weekdays and weekends?
#function to convert date variable into two-level factor variable
working_for_the_weekend <- function(date) {
  day <- weekdays(date)
  if(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
    return("weekday")
  }else if(day %in% c("Saturday", "Sunday")){
    return("weekend")
  }
}

wk1_data$day_factor <- sapply(wk1_data$date, FUN=working_for_the_weekend)
wk1_data <- wk1_data %>% mutate(day_factor = factor(day_factor))
wk1_data$day_factor %>% table()
```

```
## .
## weekday weekend 
##   12960    4608
```

```r
activity <- wk1_data %>% group_by(interval, day_factor) %>% summarise(avg = mean(steps, na.rm = T))
activity %>% ggvis(~interval, ~avg, fill = ~day_factor, fillOpacity := .5) %>% group_by(day_factor) %>% layer_lines() %>% add_axis("x", title = "Graph 4: Weekend and Weekday Intervals")
```

<!--html_preserve--><div id="plot_id998601371-container" class="ggvis-output-container">
<div id="plot_id998601371" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id998601371_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id998601371" data-renderer="svg">SVG</a>
 | 
<a id="plot_id998601371_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id998601371" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id998601371_download" class="ggvis-download" data-plot-id="plot_id998601371">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id998601371_spec = {
  "data": [
    {
      "name": ".0/group_by1/group_by2/arrange3_flat",
      "format": {
        "type": "csv",
        "parse": {
          "interval": "number",
          "avg": "number"
        }
      },
      "values": "\"day_factor\",\"interval\",\"avg\"\n\"weekday\",0,2.33333333333333\n\"weekday\",5,0.461538461538462\n\"weekday\",10,0.179487179487179\n\"weekday\",15,0.205128205128205\n\"weekday\",20,0.102564102564103\n\"weekday\",25,1.51282051282051\n\"weekday\",30,0.717948717948718\n\"weekday\",35,1.17948717948718\n\"weekday\",40,0\n\"weekday\",45,1.84615384615385\n\"weekday\",50,0.41025641025641\n\"weekday\",55,0\n\"weekday\",100,0.435897435897436\n\"weekday\",105,0\n\"weekday\",110,0.205128205128205\n\"weekday\",115,0.461538461538462\n\"weekday\",120,0\n\"weekday\",125,1.51282051282051\n\"weekday\",130,2.28205128205128\n\"weekday\",135,0\n\"weekday\",140,0.230769230769231\n\"weekday\",145,0.230769230769231\n\"weekday\",150,0.358974358974359\n\"weekday\",155,0\n\"weekday\",200,0\n\"weekday\",205,0\n\"weekday\",210,1.43589743589744\n\"weekday\",215,0\n\"weekday\",220,0\n\"weekday\",225,0.179487179487179\n\"weekday\",230,0\n\"weekday\",235,0.307692307692308\n\"weekday\",240,0\n\"weekday\",245,0\n\"weekday\",250,2.1025641025641\n\"weekday\",255,1.28205128205128\n\"weekday\",300,0\n\"weekday\",305,0\n\"weekday\",310,0\n\"weekday\",315,0\n\"weekday\",320,0\n\"weekday\",325,0.846153846153846\n\"weekday\",330,1.17948717948718\n\"weekday\",335,0.512820512820513\n\"weekday\",340,0.41025641025641\n\"weekday\",345,0.102564102564103\n\"weekday\",350,0\n\"weekday\",355,0\n\"weekday\",400,0.128205128205128\n\"weekday\",405,1.28205128205128\n\"weekday\",410,2.17948717948718\n\"weekday\",415,0\n\"weekday\",420,0.461538461538462\n\"weekday\",425,0\n\"weekday\",430,3.25641025641026\n\"weekday\",435,0.153846153846154\n\"weekday\",440,3.82051282051282\n\"weekday\",445,0.897435897435897\n\"weekday\",450,2.23076923076923\n\"weekday\",455,0.666666666666667\n\"weekday\",500,0\n\"weekday\",505,2.12820512820513\n\"weekday\",510,4.07692307692308\n\"weekday\",515,2.17948717948718\n\"weekday\",520,4.35897435897436\n\"weekday\",525,2.66666666666667\n\"weekday\",530,2.84615384615385\n\"weekday\",535,8.23076923076923\n\"weekday\",540,21.0769230769231\n\"weekday\",545,24.4615384615385\n\"weekday\",550,52.025641025641\n\"weekday\",555,58.0769230769231\n\"weekday\",600,42.7948717948718\n\"weekday\",605,66.9487179487179\n\"weekday\",610,72.5897435897436\n\"weekday\",615,79.2564102564103\n\"weekday\",620,66.0769230769231\n\"weekday\",625,62.025641025641\n\"weekday\",630,68.6410256410256\n\"weekday\",635,49.3076923076923\n\"weekday\",640,57.4615384615385\n\"weekday\",645,56.5128205128205\n\"weekday\",650,48.5384615384615\n\"weekday\",655,62.1794871794872\n\"weekday\",700,51.6410256410256\n\"weekday\",705,51.8205128205128\n\"weekday\",710,63.7948717948718\n\"weekday\",715,71.6153846153846\n\"weekday\",720,65.1282051282051\n\"weekday\",725,60.3589743589744\n\"weekday\",730,67.8461538461538\n\"weekday\",735,55.8974358974359\n\"weekday\",740,64.3333333333333\n\"weekday\",745,85.5128205128205\n\"weekday\",750,69.2564102564103\n\"weekday\",755,68.1794871794872\n\"weekday\",800,84.1538461538462\n\"weekday\",805,72.5384615384615\n\"weekday\",810,146.25641025641\n\"weekday\",815,185.74358974359\n\"weekday\",820,205.102564102564\n\"weekday\",825,187.948717948718\n\"weekday\",830,202.205128205128\n\"weekday\",835,234.102564102564\n\"weekday\",840,222.435897435897\n\"weekday\",845,186.589743589744\n\"weekday\",850,192.435897435897\n\"weekday\",855,178.641025641026\n\"weekday\",900,171.384615384615\n\"weekday\",905,126.051282051282\n\"weekday\",910,91.6153846153846\n\"weekday\",915,84.1025641025641\n\"weekday\",920,103.512820512821\n\"weekday\",925,91.9230769230769\n\"weekday\",930,57.3333333333333\n\"weekday\",935,34.4102564102564\n\"weekday\",940,27.8717948717949\n\"weekday\",945,41.1794871794872\n\"weekday\",950,39.7692307692308\n\"weekday\",955,17.1025641025641\n\"weekday\",1000,37.4615384615385\n\"weekday\",1005,16.8717948717949\n\"weekday\",1010,38.5641025641026\n\"weekday\",1015,47.0769230769231\n\"weekday\",1020,29.025641025641\n\"weekday\",1025,32.7435897435897\n\"weekday\",1030,31.4102564102564\n\"weekday\",1035,22.2307692307692\n\"weekday\",1040,21.7948717948718\n\"weekday\",1045,25.5384615384615\n\"weekday\",1050,21.5641025641026\n\"weekday\",1055,21.9230769230769\n\"weekday\",1100,20.2051282051282\n\"weekday\",1105,24.3846153846154\n\"weekday\",1110,10.2051282051282\n\"weekday\",1115,14.8461538461538\n\"weekday\",1120,23.5384615384615\n\"weekday\",1125,23.3076923076923\n\"weekday\",1130,32.6666666666667\n\"weekday\",1135,50.2307692307692\n\"weekday\",1140,44.9487179487179\n\"weekday\",1145,48.4358974358974\n\"weekday\",1150,50.7435897435897\n\"weekday\",1155,55.6666666666667\n\"weekday\",1200,54.4615384615385\n\"weekday\",1205,70.5641025641026\n\"weekday\",1210,81.9230769230769\n\"weekday\",1215,72.5897435897436\n\"weekday\",1220,46.4615384615385\n\"weekday\",1225,46.3076923076923\n\"weekday\",1230,63.8205128205128\n\"weekday\",1235,30.4871794871795\n\"weekday\",1240,21.2820512820513\n\"weekday\",1245,28.025641025641\n\"weekday\",1250,30.8974358974359\n\"weekday\",1255,54.9487179487179\n\"weekday\",1300,21.8717948717949\n\"weekday\",1305,23.5641025641026\n\"weekday\",1310,21.6923076923077\n\"weekday\",1315,11.7435897435897\n\"weekday\",1320,34\n\"weekday\",1325,43.0769230769231\n\"weekday\",1330,30.0769230769231\n\"weekday\",1335,23.025641025641\n\"weekday\",1340,22.974358974359\n\"weekday\",1345,38.1282051282051\n\"weekday\",1350,22.2307692307692\n\"weekday\",1355,32.5641025641026\n\"weekday\",1400,45.5641025641026\n\"weekday\",1405,37.6410256410256\n\"weekday\",1410,30.3589743589744\n\"weekday\",1415,44.4871794871795\n\"weekday\",1420,26.2564102564103\n\"weekday\",1425,29.7179487179487\n\"weekday\",1430,29.8974358974359\n\"weekday\",1435,12.5128205128205\n\"weekday\",1440,10.6923076923077\n\"weekday\",1445,21.3589743589744\n\"weekday\",1450,41.5897435897436\n\"weekday\",1455,37.4358974358974\n\"weekday\",1500,31\n\"weekday\",1505,34.8974358974359\n\"weekday\",1510,29.1025641025641\n\"weekday\",1515,30.8461538461538\n\"weekday\",1520,38.9230769230769\n\"weekday\",1525,35.7435897435897\n\"weekday\",1530,41.2051282051282\n\"weekday\",1535,48.7179487179487\n\"weekday\",1540,91.7435897435897\n\"weekday\",1545,95.4358974358974\n\"weekday\",1550,92.6923076923077\n\"weekday\",1555,68.2051282051282\n\"weekday\",1600,44.5384615384615\n\"weekday\",1605,42.2820512820513\n\"weekday\",1610,53.8461538461538\n\"weekday\",1615,31.974358974359\n\"weekday\",1620,22.1794871794872\n\"weekday\",1625,24.8717948717949\n\"weekday\",1630,19.2307692307692\n\"weekday\",1635,19.2564102564103\n\"weekday\",1640,22.974358974359\n\"weekday\",1645,29.9230769230769\n\"weekday\",1650,24.7692307692308\n\"weekday\",1655,30.6923076923077\n\"weekday\",1700,20.025641025641\n\"weekday\",1705,43.2051282051282\n\"weekday\",1710,31.6410256410256\n\"weekday\",1715,46.0512820512821\n\"weekday\",1720,58.1794871794872\n\"weekday\",1725,71.3589743589744\n\"weekday\",1730,54.1794871794872\n\"weekday\",1735,66.7692307692308\n\"weekday\",1740,84.0769230769231\n\"weekday\",1745,59.7692307692308\n\"weekday\",1750,34.4615384615385\n\"weekday\",1755,37.6153846153846\n\"weekday\",1800,24.4871794871795\n\"weekday\",1805,44.8717948717949\n\"weekday\",1810,66.0769230769231\n\"weekday\",1815,82.2307692307692\n\"weekday\",1820,61.7179487179487\n\"weekday\",1825,74.3333333333333\n\"weekday\",1830,79.4615384615385\n\"weekday\",1835,82.6153846153846\n\"weekday\",1840,92.6923076923077\n\"weekday\",1845,117.923076923077\n\"weekday\",1850,103.564102564103\n\"weekday\",1855,91.3589743589744\n\"weekday\",1900,87.974358974359\n\"weekday\",1905,77.1282051282051\n\"weekday\",1910,63.0512820512821\n\"weekday\",1915,54.5384615384615\n\"weekday\",1920,38.1282051282051\n\"weekday\",1925,20.5384615384615\n\"weekday\",1930,29.3589743589744\n\"weekday\",1935,46.8974358974359\n\"weekday\",1940,30.025641025641\n\"weekday\",1945,17.5128205128205\n\"weekday\",1950,44.0512820512821\n\"weekday\",1955,26.3333333333333\n\"weekday\",2000,12.4358974358974\n\"weekday\",2005,3.48717948717949\n\"weekday\",2010,4.8974358974359\n\"weekday\",2015,11.1538461538462\n\"weekday\",2020,5.92307692307692\n\"weekday\",2025,3.33333333333333\n\"weekday\",2030,7.07692307692308\n\"weekday\",2035,4.97435897435897\n\"weekday\",2040,7.33333333333333\n\"weekday\",2045,11.8461538461538\n\"weekday\",2050,25\n\"weekday\",2055,16.8717948717949\n\"weekday\",2100,10.6666666666667\n\"weekday\",2105,19.1538461538462\n\"weekday\",2110,29.2820512820513\n\"weekday\",2115,18.8974358974359\n\"weekday\",2120,14.5641025641026\n\"weekday\",2125,8.05128205128205\n\"weekday\",2130,12.5128205128205\n\"weekday\",2135,16.5384615384615\n\"weekday\",2140,6.8974358974359\n\"weekday\",2145,7.56410256410256\n\"weekday\",2150,8.28205128205128\n\"weekday\",2155,3.56410256410256\n\"weekday\",2200,1.53846153846154\n\"weekday\",2205,4.53846153846154\n\"weekday\",2210,6.53846153846154\n\"weekday\",2215,11.5641025641026\n\"weekday\",2220,9.61538461538461\n\"weekday\",2225,11.1794871794872\n\"weekday\",2230,13.2564102564103\n\"weekday\",2235,3\n\"weekday\",2240,0\n\"weekday\",2245,0.153846153846154\n\"weekday\",2250,1.94871794871795\n\"weekday\",2255,1.61538461538462\n\"weekday\",2300,3.58974358974359\n\"weekday\",2305,3.87179487179487\n\"weekday\",2310,0\n\"weekday\",2315,1.12820512820513\n\"weekday\",2320,1.30769230769231\n\"weekday\",2325,1.92307692307692\n\"weekday\",2330,3.1025641025641\n\"weekday\",2335,1.87179487179487\n\"weekday\",2340,2.07692307692308\n\"weekday\",2345,0.205128205128205\n\"weekday\",2350,0.307692307692308\n\"weekday\",2355,1.46153846153846\n\"weekend\",0,0\n\"weekend\",5,0\n\"weekend\",10,0\n\"weekend\",15,0\n\"weekend\",20,0\n\"weekend\",25,3.71428571428571\n\"weekend\",30,0\n\"weekend\",35,0\n\"weekend\",40,0\n\"weekend\",45,0.428571428571429\n\"weekend\",50,0\n\"weekend\",55,0.5\n\"weekend\",100,0\n\"weekend\",105,2.57142857142857\n\"weekend\",110,0\n\"weekend\",115,0\n\"weekend\",120,0\n\"weekend\",125,0\n\"weekend\",130,0.571428571428571\n\"weekend\",135,0.642857142857143\n\"weekend\",140,0\n\"weekend\",145,0.785714285714286\n\"weekend\",150,0\n\"weekend\",155,0\n\"weekend\",200,0\n\"weekend\",205,0\n\"weekend\",210,0.285714285714286\n\"weekend\",215,0\n\"weekend\",220,0\n\"weekend\",225,0\n\"weekend\",230,0\n\"weekend\",235,0\n\"weekend\",240,0\n\"weekend\",245,0\n\"weekend\",250,0\n\"weekend\",255,0\n\"weekend\",300,0\n\"weekend\",305,0\n\"weekend\",310,0\n\"weekend\",315,0\n\"weekend\",320,0.785714285714286\n\"weekend\",325,0\n\"weekend\",330,2.85714285714286\n\"weekend\",335,0.785714285714286\n\"weekend\",340,0.714285714285714\n\"weekend\",345,0\n\"weekend\",350,0\n\"weekend\",355,0\n\"weekend\",400,4.14285714285714\n\"weekend\",405,0\n\"weekend\",410,3.64285714285714\n\"weekend\",415,0\n\"weekend\",420,0\n\"weekend\",425,1.35714285714286\n\"weekend\",430,6.5\n\"weekend\",435,2.07142857142857\n\"weekend\",440,2.57142857142857\n\"weekend\",445,0.642857142857143\n\"weekend\",450,5.57142857142857\n\"weekend\",455,2.35714285714286\n\"weekend\",500,0\n\"weekend\",505,0\n\"weekend\",510,0\n\"weekend\",515,2.42857142857143\n\"weekend\",520,0.428571428571429\n\"weekend\",525,3.78571428571429\n\"weekend\",530,0\n\"weekend\",535,0\n\"weekend\",540,1.92857142857143\n\"weekend\",545,1.28571428571429\n\"weekend\",550,4.42857142857143\n\"weekend\",555,6.64285714285714\n\"weekend\",600,0\n\"weekend\",605,0\n\"weekend\",610,1.35714285714286\n\"weekend\",615,19.4285714285714\n\"weekend\",620,5.07142857142857\n\"weekend\",625,5.42857142857143\n\"weekend\",630,6.21428571428571\n\"weekend\",635,11.5714285714286\n\"weekend\",640,6.57142857142857\n\"weekend\",645,9.78571428571429\n\"weekend\",650,6.21428571428571\n\"weekend\",655,12.4285714285714\n\"weekend\",700,22\n\"weekend\",705,23.6428571428571\n\"weekend\",710,13.5\n\"weekend\",715,6.85714285714286\n\"weekend\",720,7.57142857142857\n\"weekend\",725,24.8571428571429\n\"weekend\",730,21.7857142857143\n\"weekend\",735,12.0714285714286\n\"weekend\",740,18.6428571428571\n\"weekend\",745,25.0714285714286\n\"weekend\",750,26.0714285714286\n\"weekend\",755,22.6428571428571\n\"weekend\",800,43.3571428571429\n\"weekend\",805,56.1428571428571\n\"weekend\",810,82.5714285714286\n\"weekend\",815,78.9285714285714\n\"weekend\",820,76.5714285714286\n\"weekend\",825,64.7142857142857\n\"weekend\",830,107.928571428571\n\"weekend\",835,128.357142857143\n\"weekend\",840,122.071428571429\n\"weekend\",845,160\n\"weekend\",850,158.214285714286\n\"weekend\",855,134.642857142857\n\"weekend\",900,65.6428571428571\n\"weekend\",905,118.428571428571\n\"weekend\",910,157.857142857143\n\"weekend\",915,175\n\"weekend\",920,104.285714285714\n\"weekend\",925,107.214285714286\n\"weekend\",930,90.9285714285714\n\"weekend\",935,75.3571428571429\n\"weekend\",940,16.2142857142857\n\"weekend\",945,32\n\"weekend\",950,21.6428571428571\n\"weekend\",955,32.0714285714286\n\"weekend\",1000,49.2142857142857\n\"weekend\",1005,55.1428571428571\n\"weekend\",1010,53.1428571428571\n\"weekend\",1015,68.2142857142857\n\"weekend\",1020,66.5\n\"weekend\",1025,101.071428571429\n\"weekend\",1030,80.1428571428571\n\"weekend\",1035,79.7142857142857\n\"weekend\",1040,70.6428571428571\n\"weekend\",1045,36.1428571428571\n\"weekend\",1050,34.9285714285714\n\"weekend\",1055,59.8571428571429\n\"weekend\",1100,62.4285714285714\n\"weekend\",1105,44.4285714285714\n\"weekend\",1110,52.2857142857143\n\"weekend\",1115,55.3571428571429\n\"weekend\",1120,41.8571428571429\n\"weekend\",1125,35.2857142857143\n\"weekend\",1130,35.5714285714286\n\"weekend\",1135,49.2857142857143\n\"weekend\",1140,33.9285714285714\n\"weekend\",1145,33.9285714285714\n\"weekend\",1150,32.9285714285714\n\"weekend\",1155,69\n\"weekend\",1200,90.0714285714286\n\"weekend\",1205,135.428571428571\n\"weekend\",1210,130.857142857143\n\"weekend\",1215,149\n\"weekend\",1220,110.571428571429\n\"weekend\",1225,60.9285714285714\n\"weekend\",1230,28.4285714285714\n\"weekend\",1235,37.7857142857143\n\"weekend\",1240,41.1428571428571\n\"weekend\",1245,64.7857142857143\n\"weekend\",1250,84.5\n\"weekend\",1255,101.642857142857\n\"weekend\",1300,99.3571428571429\n\"weekend\",1305,85.3571428571429\n\"weekend\",1310,103.357142857143\n\"weekend\",1315,122.428571428571\n\"weekend\",1320,80.3571428571429\n\"weekend\",1325,93.6428571428571\n\"weekend\",1330,78.0714285714286\n\"weekend\",1335,31\n\"weekend\",1340,87.2857142857143\n\"weekend\",1345,96.5\n\"weekend\",1350,117.214285714286\n\"weekend\",1355,139.5\n\"weekend\",1400,84.1428571428571\n\"weekend\",1405,91.8571428571429\n\"weekend\",1410,80.4285714285714\n\"weekend\",1415,60.4285714285714\n\"weekend\",1420,61.1428571428571\n\"weekend\",1425,59.3571428571429\n\"weekend\",1430,75.1428571428571\n\"weekend\",1435,69.2857142857143\n\"weekend\",1440,35\n\"weekend\",1445,39.2142857142857\n\"weekend\",1450,49.2857142857143\n\"weekend\",1455,61.4285714285714\n\"weekend\",1500,27.2857142857143\n\"weekend\",1505,39.3571428571429\n\"weekend\",1510,53.2857142857143\n\"weekend\",1515,61.1428571428571\n\"weekend\",1520,65.5714285714286\n\"weekend\",1525,81.2142857142857\n\"weekend\",1530,67.4285714285714\n\"weekend\",1535,111.571428571429\n\"weekend\",1540,58.2857142857143\n\"weekend\",1545,107.642857142857\n\"weekend\",1550,128.357142857143\n\"weekend\",1555,127.857142857143\n\"weekend\",1600,111.142857142857\n\"weekend\",1605,125\n\"weekend\",1610,132.214285714286\n\"weekend\",1615,150.071428571429\n\"weekend\",1620,153.642857142857\n\"weekend\",1625,157\n\"weekend\",1630,112.5\n\"weekend\",1635,92.3571428571429\n\"weekend\",1640,105.071428571429\n\"weekend\",1645,88.7142857142857\n\"weekend\",1650,105.928571428571\n\"weekend\",1655,79.8571428571429\n\"weekend\",1700,120.714285714286\n\"weekend\",1705,92.7857142857143\n\"weekend\",1710,103.857142857143\n\"weekend\",1715,103.5\n\"weekend\",1720,113.214285714286\n\"weekend\",1725,100.071428571429\n\"weekend\",1730,110.071428571429\n\"weekend\",1735,39.8571428571429\n\"weekend\",1740,50.0714285714286\n\"weekend\",1745,47.4285714285714\n\"weekend\",1750,35.6428571428571\n\"weekend\",1755,37\n\"weekend\",1800,85.7857142857143\n\"weekend\",1805,94.6428571428571\n\"weekend\",1810,98.7142857142857\n\"weekend\",1815,93.9285714285714\n\"weekend\",1820,52.4285714285714\n\"weekend\",1825,49.5\n\"weekend\",1830,72.7857142857143\n\"weekend\",1835,50.9285714285714\n\"weekend\",1840,64.8571428571429\n\"weekend\",1845,48\n\"weekend\",1850,39.2857142857143\n\"weekend\",1855,69.5714285714286\n\"weekend\",1900,76.2142857142857\n\"weekend\",1905,79.7857142857143\n\"weekend\",1910,44.0714285714286\n\"weekend\",1915,50.0714285714286\n\"weekend\",1920,31.2857142857143\n\"weekend\",1925,21.2142857142857\n\"weekend\",1930,21.9285714285714\n\"weekend\",1935,20.8571428571429\n\"weekend\",1940,30.7142857142857\n\"weekend\",1945,47.9285714285714\n\"weekend\",1950,50.1428571428571\n\"weekend\",1955,53.5714285714286\n\"weekend\",2000,39.6428571428571\n\"weekend\",2005,62.2857142857143\n\"weekend\",2010,59.5714285714286\n\"weekend\",2015,95.1428571428571\n\"weekend\",2020,85\n\"weekend\",2025,70.8571428571429\n\"weekend\",2030,83.6428571428571\n\"weekend\",2035,66.9285714285714\n\"weekend\",2040,53.5714285714286\n\"weekend\",2045,47.7142857142857\n\"weekend\",2050,52.6428571428571\n\"weekend\",2055,29.2857142857143\n\"weekend\",2100,30.6428571428571\n\"weekend\",2105,11.8571428571429\n\"weekend\",2110,7.21428571428571\n\"weekend\",2115,20.2142857142857\n\"weekend\",2120,6.57142857142857\n\"weekend\",2125,7.92857142857143\n\"weekend\",2130,20.6428571428571\n\"weekend\",2135,15.6428571428571\n\"weekend\",2140,13.6428571428571\n\"weekend\",2145,8.42857142857143\n\"weekend\",2150,7.71428571428571\n\"weekend\",2155,0\n\"weekend\",2200,1.21428571428571\n\"weekend\",2205,1.28571428571429\n\"weekend\",2210,0\n\"weekend\",2215,0\n\"weekend\",2220,0\n\"weekend\",2225,1.78571428571429\n\"weekend\",2230,0\n\"weekend\",2235,0\n\"weekend\",2240,1.21428571428571\n\"weekend\",2245,0\n\"weekend\",2250,0.642857142857143\n\"weekend\",2255,12.9285714285714\n\"weekend\",2300,2.5\n\"weekend\",2305,0\n\"weekend\",2310,0\n\"weekend\",2315,0\n\"weekend\",2320,0\n\"weekend\",2325,0.642857142857143\n\"weekend\",2330,1.21428571428571\n\"weekend\",2335,12.5714285714286\n\"weekend\",2340,6.71428571428571\n\"weekend\",2345,1.85714285714286\n\"weekend\",2350,0\n\"weekend\",2355,0"
    },
    {
      "name": ".0/group_by1/group_by2/arrange3",
      "source": ".0/group_by1/group_by2/arrange3_flat",
      "transform": [
        {
          "type": "treefacet",
          "keys": [
            "data.day_factor"
          ]
        }
      ]
    },
    {
      "name": "scale/fill",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"weekday\"\n\"weekend\""
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-117.75\n2472.75"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n-11.7051282051282\n245.807692307692"
    }
  ],
  "scales": [
    {
      "name": "fill",
      "type": "ordinal",
      "domain": {
        "data": "scale/fill",
        "field": "data.domain"
      },
      "points": true,
      "sort": false,
      "range": "category10"
    },
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "group",
      "from": {
        "data": ".0/group_by1/group_by2/arrange3"
      },
      "marks": [
        {
          "type": "line",
          "properties": {
            "update": {
              "stroke": {
                "value": "#000000"
              },
              "fill": {
                "scale": "fill",
                "field": "data.day_factor"
              },
              "fillOpacity": {
                "value": 0.5
              },
              "x": {
                "scale": "x",
                "field": "data.interval"
              },
              "y": {
                "scale": "y",
                "field": "data.avg"
              }
            },
            "ggvis": {
              "data": {
                "value": ".0/group_by1/group_by2/arrange3"
              }
            }
          }
        }
      ]
    }
  ],
  "legends": [
    {
      "orient": "right",
      "fill": "fill",
      "title": "day_factor"
    }
  ],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "title": "Graph 4: Weekend and Weekday Intervals",
      "layer": "back",
      "grid": true
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "layer": "back",
      "grid": true,
      "title": "avg"
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id998601371").parseSpec(plot_id998601371_spec);
</script><!--/html_preserve-->

