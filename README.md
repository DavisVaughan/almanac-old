
<!-- README.md is generated from README.Rmd. Please edit that file -->

# almanac

<!-- badges: start -->

<!-- badges: end -->

almanac provides tools for working with dates relative to business
calendars. Specifically, it provides utilities to:

  - Create calendar objects with predefined holidays / weekends.

  - Extend or tweak calendar objects with custom holidays.

  - Shift dates forwards or backwards relative to a calendar, skipping
    over weekends and holidays.

  - Generate date sequences relative to a calendar, skipping over
    weekends and holidays.

## Installation

You can NOT install almanac from [CRAN](https://CRAN.R-project.org).

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/almanac")
```

## Examples

``` r
library(almanac)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
set.seed(123)
```

Imagine you have a sequence of business dates indexing some value, like
a business cost.

``` r
costs <- tibble(
  cost = rnorm(5, mean = 10000, sd = 500),
  dates = as.Date("2019-02-13") + c(0:2, 6:7)
)

costs
#> # A tibble: 5 x 2
#>     cost dates     
#>    <dbl> <date>    
#> 1  9720. 2019-02-13
#> 2  9885. 2019-02-14
#> 3 10779. 2019-02-15
#> 4 10035. 2019-02-19
#> 5 10065. 2019-02-20
```

Importantly, these are business dates that follow a business calendar of
some kind. Generally, that means that there won’t be any weekend values,
and holidays are observed and also won’t generate any values. In this
data set, `02/16 - 02/17` is a weekend, and `02/18` is Valentine’s Day.

To perform any kind of rolling calculation correctly, you might need to
“look backwards” by some number of business days. This is different
from just looking forward “1 day”, because when you are at Friday, you
really want to look forward to Monday. But if Monday is a holiday (as it
is here\!), you really want that to actually shift to Tuesday, the
actual next business day.

almanac provides these tools.

Notice that in the example below, `"2019-02-15"` is shifted to
`"2019-02-19"` when we request a 1 day shift (using a
`lubridate::days()` period object). It skips over the weekend, and
Valentine’s Day to locate the next business day.

Additionally, in the 5 day shift, `"2019-02-19"`, a Tuesday, is shifted
all the way to `"2019-02-26"`, the next Tuesday, which corresponds to a
“business week” and skips over the weekend.

``` r
# Construct a calendar object, the default is a US calendar
cal <- calendar()
cal
#> <Calendar: united_states>

# Use the new calendar object alongside `cal_shift()` to shift 
# by a number of business days.
costs %>%
  mutate(
    dates_wday = wday(dates, label = TRUE),
    one_day = cal_shift(dates, days(1), calendar = cal),
    one_wday = wday(one_day, label = TRUE),
    five_days = cal_shift(dates, days(5), calendar = cal),
    five_wday = wday(five_days, label = TRUE),
  )
#> # A tibble: 5 x 7
#>     cost dates      dates_wday one_day    one_wday five_days  five_wday
#>    <dbl> <date>     <ord>      <date>     <ord>    <date>     <ord>    
#> 1  9720. 2019-02-13 Wed        2019-02-14 Thu      2019-02-21 Thu      
#> 2  9885. 2019-02-14 Thu        2019-02-15 Fri      2019-02-22 Fri      
#> 3 10779. 2019-02-15 Fri        2019-02-19 Tue      2019-02-25 Mon      
#> 4 10035. 2019-02-19 Tue        2019-02-20 Wed      2019-02-26 Tue      
#> 5 10065. 2019-02-20 Wed        2019-02-21 Thu      2019-02-27 Wed
```

You can tweak the calendar by adding new holidays, or removing existing
ones. Here we add Thursday the 21st as a holiday, but remove Valentine’s
Day for that year.

``` r
cal_tweaked <- cal %>%
  holidays_add("2019-02-21") %>%
  holidays_remove("2019-02-18")

cal_tweaked
#> <Calendar: united_states>
#> Added holidays: 
#>    -  2019-02-21
#> Removed holidays: 
#>    -  2019-02-18
```

Running the same code as before with the updated calendar has different
results. `"2019-02-15"` now shifts forward over the weekend but lands on
`"2019-02-18"` as being 1 business day forward, since we removed
Valentine’s Day as a holiday. Shifting `"2019-02-19"` 5 business days
forwards now no longer lands on another Tuesday since `"2019-02-21"` is
now an additional holiday. Instead, it lands one day further on a
Wednesday.

``` r
costs %>%
  mutate(
    dates_wday = wday(dates, label = TRUE),
    one_day = cal_shift(dates, days(1), calendar = cal_tweaked),
    one_wday = wday(one_day, label = TRUE),
    five_days = cal_shift(dates, days(5), calendar = cal_tweaked),
    five_wday = wday(five_days, label = TRUE),
  )
#> # A tibble: 5 x 7
#>     cost dates      dates_wday one_day    one_wday five_days  five_wday
#>    <dbl> <date>     <ord>      <date>     <ord>    <date>     <ord>    
#> 1  9720. 2019-02-13 Wed        2019-02-14 Thu      2019-02-20 Wed      
#> 2  9885. 2019-02-14 Thu        2019-02-15 Fri      2019-02-22 Fri      
#> 3 10779. 2019-02-15 Fri        2019-02-18 Mon      2019-02-25 Mon      
#> 4 10035. 2019-02-19 Tue        2019-02-20 Wed      2019-02-27 Wed      
#> 5 10065. 2019-02-20 Wed        2019-02-22 Fri      2019-02-28 Thu
```
