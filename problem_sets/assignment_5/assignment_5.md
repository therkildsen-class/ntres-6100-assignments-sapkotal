# NTRES 6100: Assignment \#5


First, load all the required packages with the following code. Install
them if they are not installed yet.

``` r
library(tidyverse)
library(knitr)
library(gapminder)
```

## **Exercise 1. Trends in land value**

This excercise uses a dataset that describes the trends in land value
(`Land.Value`), among other variables, in different states in the US
1975-2013. The states are grouped into four different regions, under the
variable `region`. This dataset was obtained from the Data Science
Services of Harvard University.

``` r
housing <- read_csv("https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/landdata_states.csv")
housing %>%
  head() %>% 
  kable()
```

| State | region | Date | Home.Value | Structure.Cost | Land.Value | Land.Share..Pct. | Home.Price.Index | Land.Price.Index | Year | Qrtr |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| AK | West | 2010.25 | 224952 | 160599 | 64352 | 28.6 | 1.481 | 1.552 | 2010 | 1 |
| AK | West | 2010.50 | 225511 | 160252 | 65259 | 28.9 | 1.484 | 1.576 | 2010 | 2 |
| AK | West | 2009.75 | 225820 | 163791 | 62029 | 27.5 | 1.486 | 1.494 | 2009 | 3 |
| AK | West | 2010.00 | 224994 | 161787 | 63207 | 28.1 | 1.481 | 1.524 | 2009 | 4 |
| AK | West | 2008.00 | 234590 | 155400 | 79190 | 33.8 | 1.544 | 1.885 | 2007 | 4 |
| AK | West | 2008.25 | 233714 | 157458 | 76256 | 32.6 | 1.538 | 1.817 | 2008 | 1 |

#### **1.1 Washington DC was not assigned to a region in this dataset. According to the United States Census Bureau, however, DC is part of the South region. Here:**

``` r
housing_dc <- housing |>
  mutate(region = if_else(State == "DC", "South", region)) |>
  filter(State == "DC") |>
  select(region, State, Date, Land.Value)

housing_dc |>
  head(6) |>
  knitr::kable()
```

| region | State |    Date | Land.Value |
|:-------|:------|--------:|-----------:|
| South  | DC    | 2003.00 |     290522 |
| South  | DC    | 2003.25 |     305673 |
| South  | DC    | 2003.50 |     323078 |
| South  | DC    | 2003.75 |     342010 |
| South  | DC    | 2004.00 |     361999 |
| South  | DC    | 2004.25 |     382792 |

Answer: As indicated by dimension (153 X4 ) of tibble when DC was
filtered (not shown in the code output above), there are 153 records
from DC.

#### **1.2 Generate a tibble/dataframe that summarizes the mean land value of each region at each time point and show its first 6 lines.**

``` r
housing_mv <- housing |>
  mutate(region = if_else(State == "DC", "South", region)) |>
  group_by(region, Date) |>
  summarise(
    mean_land_value = mean(Land.Value, na.rm = TRUE),
    .groups = "drop"
  )

kable(head(housing_mv, 6))
```

| region  |    Date | mean_land_value |
|:--------|--------:|----------------:|
| Midwest | 1975.25 |        2452.167 |
| Midwest | 1975.50 |        2498.917 |
| Midwest | 1975.75 |        2608.167 |
| Midwest | 1976.00 |        2780.000 |
| Midwest | 1976.25 |        2967.333 |
| Midwest | 1976.50 |        3212.833 |

#### **1.3 Using the tibble/dataframe from 1.2, plot the trend in mean land value of each region through time.**

``` r
ggplot(housing_mv) +
  geom_line(aes(x = Date, y = mean_land_value, color = region))
```

![](assignment_5_files/figure-commonmark/unnamed-chunk-5-1.png)

## **Exercise 2. Life expectancy and GDP per capita 1952-2007**

This exercise uses the `gapminder` dataset from the `gapminder` package.
It describes the life expectancy (`lifeExp`), GDP per capita
(`gdpPercap`), and population (`pop`) of 142 countries from 1952 to
2007. These countries can be grouped into 5 continents. As a
reminder, **reproduce the following plots exactly as shown**.

``` r
gapminder %>% 
  head() %>% 
  kable()
```

| country     | continent | year | lifeExp |      pop | gdpPercap |
|:------------|:----------|-----:|--------:|---------:|----------:|
| Afghanistan | Asia      | 1952 |  28.801 |  8425333 |  779.4453 |
| Afghanistan | Asia      | 1957 |  30.332 |  9240934 |  820.8530 |
| Afghanistan | Asia      | 1962 |  31.997 | 10267083 |  853.1007 |
| Afghanistan | Asia      | 1967 |  34.020 | 11537966 |  836.1971 |
| Afghanistan | Asia      | 1972 |  36.088 | 13079460 |  739.9811 |
| Afghanistan | Asia      | 1977 |  38.438 | 14880372 |  786.1134 |

#### **2.1 Use a scatterplot to explore the relationship between per capita GDP (`gdpPercap`) and life expectancy (`lifeExp`) in the year 2007.**

``` r
gapminder |> 
  filter((gdpPercap < 50000), year == "2007") |> 
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp))
```

![](assignment_5_files/figure-commonmark/unnamed-chunk-7-1.png)

#### **2.2 Add a smoothing line to the previous plot.**

``` r
gapminder |> 
  filter((gdpPercap < 50000), year == "2007") |> 
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp)) + 
  geom_smooth(aes(x = gdpPercap, y = lifeExp))
```

![](assignment_5_files/figure-commonmark/unnamed-chunk-8-1.png)

#### **2.3 Exclude Oceania from the previous plot, show each continent in a different color, and fit a separate smoothing line to each continent to identify differences in this relationship between continents. Turn off the confidence intervals.**

``` r
gapminder |> 
  filter((gdpPercap < 50000), year == "2007", continent != "Oceania") |> 
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_smooth(aes(x = gdpPercap, y = lifeExp, color = continent), se= FALSE)
```

![](assignment_5_files/figure-commonmark/unnamed-chunk-9-1.png)

#### **2.4 Use faceting to solve the same problem. Include the confidence intervals in this plot.**

``` r
gapminder |> 
  filter((gdpPercap < 50000), year == "2007", continent != "Oceania") |> 
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_smooth(aes(x = gdpPercap, y = lifeExp, color = continent))+
  facet_wrap(~continent)
```

![](assignment_5_files/figure-commonmark/unnamed-chunk-10-1.png)

#### **2.5 Explore the trend in life expectancy through time in each continent. Color by continent.**

``` r
ggplot(gapminder) +
  geom_line(aes(x = year, y = lifeExp, group = country, color = continent)) + 
  facet_wrap(~continent, nrow = 2)
```

![](assignment_5_files/figure-commonmark/unnamed-chunk-11-1.png)

#### **2.6 From the previous plot, we see some abnormal trends in Asia and Africa, where the the life expectancy in some countries sharply dropped at certain time periods. Here, we look into what happened in Asia in more detail. First, create a new dataset by filtering only the Asian countries. Show the first 6 lines of this filtered dataset.**

``` r
gapminder_asia <- gapminder |> 
  filter(continent == "Asia")
kable(head(gapminder_asia, 6))
```

| country     | continent | year | lifeExp |      pop | gdpPercap |
|:------------|:----------|-----:|--------:|---------:|----------:|
| Afghanistan | Asia      | 1952 |  28.801 |  8425333 |  779.4453 |
| Afghanistan | Asia      | 1957 |  30.332 |  9240934 |  820.8530 |
| Afghanistan | Asia      | 1962 |  31.997 | 10267083 |  853.1007 |
| Afghanistan | Asia      | 1967 |  34.020 | 11537966 |  836.1971 |
| Afghanistan | Asia      | 1972 |  36.088 | 13079460 |  739.9811 |
| Afghanistan | Asia      | 1977 |  38.438 | 14880372 |  786.1134 |

#### **2.7 Using the filtered dataset, identify the countries that had abnormal trends in life expectancy by plotting, and discuss historical events possibly explaining these trends. (Hint: facet by country)**

``` r
ggplot(gapminder_asia) +
  geom_line(aes(x = year, y = lifeExp, group = country, color = country)) + 
  facet_wrap(~country, nrow = 6)
```

![](assignment_5_files/figure-commonmark/unnamed-chunk-13-1.png)

Answer: Particularly, China saw drop in population around the 1960s,
while Cambodia did so a decade later. The region for such war might be
the war and famine in those countries. Similarly, other countries also
experienced less severe but abnormal trends, such as slowing down of
population growth in Afghanistan and slight reduction in population,
possibly due to wars, in Iraq and Democratic Republic of Korea.
