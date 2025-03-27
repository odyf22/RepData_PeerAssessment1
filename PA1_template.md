## Loading and preprocessing the data

``` r
# Load dataset
activity <- read.csv("activity.csv")

# Convert 'date' to Date class
activity$date <- as.Date(activity$date)

# View basic structure
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

## What is mean total number of steps taken per day?

``` r
# Sum steps per day
total_steps_per_day <- activity %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

# Plot histogram
ggplot(total_steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black") +
  labs(title = "Total Steps per Day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# Mean and median
mean_steps <- mean(total_steps_per_day$total_steps, na.rm = TRUE)
median_steps <- median(total_steps_per_day$total_steps, na.rm = TRUE)

mean_steps
```

    ## [1] 9354.23

``` r
median_steps
```

    ## [1] 10395

## What is the average daily activity pattern?

``` r
# Average steps per interval
avg_interval <- activity %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

# Plot time series
ggplot(avg_interval, aes(x = interval, y = avg_steps)) +
  geom_line(color = "darkgreen") +
  labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Average Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Interval with maximum steps
max_interval <- avg_interval[which.max(avg_interval$avg_steps), ]
max_interval
```

    ## # A tibble: 1 × 2
    ##   interval avg_steps
    ##      <int>     <dbl>
    ## 1      835      206.

## Imputing missing values

``` r
# Total NA values
total_na <- sum(is.na(activity$steps))
total_na
```

    ## [1] 2304

``` r
# Replace NA with mean for that interval
imputed_activity <- activity %>%
  left_join(avg_interval, by = "interval") %>%
  mutate(steps = ifelse(is.na(steps), avg_steps, steps)) %>%
  select(date, interval, steps)
```

``` r
# New histogram after imputation
total_steps_imputed <- imputed_activity %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

ggplot(total_steps_imputed, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "tomato", color = "black") +
  labs(title = "Total Steps per Day (Imputed)", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
# Mean and median after imputation
mean_imputed <- mean(total_steps_imputed$total_steps)
median_imputed <- median(total_steps_imputed$total_steps)

mean_imputed
```

    ## [1] 10766.19

``` r
median_imputed
```

    ## [1] 10766.19

## Are there differences in activity patterns between weekdays and weekends?

``` r
# Add weekday/weekend
imputed_activity$day_type <- ifelse(weekdays(imputed_activity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
imputed_activity$day_type <- factor(imputed_activity$day_type, levels = c("weekday", "weekend"))

# Average by interval and day_type
avg_interval_daytype <- imputed_activity %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps), .groups = "drop")

# Panel plot
ggplot(avg_interval_daytype, aes(x = interval, y = avg_steps)) +
  geom_line(color = "purple") +
  facet_wrap(~ day_type, ncol = 1) +
  labs(title = "Average Steps by Interval: Weekday vs Weekend", x = "Interval", y = "Average Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)
