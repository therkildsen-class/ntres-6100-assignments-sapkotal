# NTRES 6100 Assignment \#9


Loading the packages

``` r
library(tidyverse)
library(knitr)
```

## **Exercise 1: Unit converter**

**1.1 Write a function that can convert Fahrenheit to Celsius**, based
on the following fomula: `C = (F - 32) * 5 / 9`

Take your function for a spin, does it return the correct values?

32 F = 0 C

``` r
f_to_c <- function(F){
  C <-  (F - 32) * 5 / 9
  return (C)
}
```

32 F = 0 C

``` r
f_to_c(32)
```

    [1] 0

50 F = 10 C

``` r
f_to_c(50)
```

    [1] 10

212 F = 100 C

``` r
f_to_c(212)
```

    [1] 100

**1.2** A student came from ‘tropical Canada’. She doesn’t like the cold
but she really didn’t like it when it’s hot. Although she wanted to know
what the temperature is in Celsius when the US weather channel reported
it in Fahrenheit, there are certain points at which it was just too cold
or too hot for to to care about the exact value. **Modify the f_to_c
function below to print the following**, and check if your function
works properly using the input of **-10 F, 60 F, and 90 F**.

- If the temperature is less than -20 C, print “Don’t bother going out.”

- If the temperature is greater than 30 C, print “I’m moving back to
  Canada.”

- If the temperature is between -20 C and 30 C, print the temperature in
  Celsius.

``` r
f_to_c_message <- function(F){
  C <-  (F - 32) * 5 / 9
  if(C < -20) {
    print("Don’t bother going out.")
  } else if(C > 30){
    print("I’m moving back to Canada.")
  } else {
    print(C)
  }
}
```

``` r
f_to_c_message(-10)
```

    [1] "Don’t bother going out."

``` r
f_to_c_message(60)
```

    [1] 15.55556

``` r
f_to_c_message(90)
```

    [1] "I’m moving back to Canada."

## **Exercise 2: Set operation**

The R function `setdiff(x,y)` returns the elements of a vector x that
are not in y. For example, say x is a list of singers who can reach very
high registers, and y is a list of musicians who died in 2016.
`setdiff(x, y)` would return a list of singers with high voices who did
not die in 2016.

``` r
x <- c("prince", "mj", "sam cook", "whitney", "dolly")
y <- c("sharon jones", "prince", "bowie", "leonard cohen", "phife dawg")
setdiff(x,y)
```

    [1] "mj"       "sam cook" "whitney"  "dolly"   

However, sometimes you want a function to return the elements that are
in x or y but not both. Here, **write such a function**, and use your
custom function to **find the names of singers who either have high
voices, but did not die in 2016, OR who died in 2016 but do not have
high voices.**

Your function will need to work for any vectors, not just the ones in
this example. The output should be a single vector, not two vectors.

``` r
x <- c("prince", "mj", "sam cook", "whitney", "dolly")
y <- c("sharon jones", "prince", "bowie", "leonard cohen", "phife dawg")
```

Creating function

``` r
unique_element <- function(x, y){
  x_unique <- setdiff(x, intersect(x, y))
  y_unique <- setdiff(y, intersect(x, y))
  unique <- union(x_unique, y_unique)
  return(unique)
}
```

Running the function

``` r
unique_element(x,y)
```

    [1] "mj"            "sam cook"      "whitney"       "dolly"        
    [5] "sharon jones"  "bowie"         "leonard cohen" "phife dawg"   

## **Exercise 3: Fuel Efficiency of Car Models**

This exercise uses the `mtcars` dataset, which is available in base R.
It was extracted from the 1974 Motor Trend US magazine, and comprises
fuel consumption and 10 aspects of automobile design and performance for
32 automobiles (1973–74 models). Let’s first convert `mtcars` to tibble
format such that the row names become a column named “model”.

``` r
mtcars_tbl <- as_tibble(mtcars, rownames = "model")
mtcars_tbl  |> 
  head(10)  |> 
  kable()
```

| model             |  mpg | cyl |  disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:------------------|-----:|----:|------:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4         | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag     | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710        | 22.8 |   4 | 108.0 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive    | 21.4 |   6 | 258.0 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout | 18.7 |   8 | 360.0 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |
| Valiant           | 18.1 |   6 | 225.0 | 105 | 2.76 | 3.460 | 20.22 |   1 |   0 |    3 |    1 |
| Duster 360        | 14.3 |   8 | 360.0 | 245 | 3.21 | 3.570 | 15.84 |   0 |   0 |    3 |    4 |
| Merc 240D         | 24.4 |   4 | 146.7 |  62 | 3.69 | 3.190 | 20.00 |   1 |   0 |    4 |    2 |
| Merc 230          | 22.8 |   4 | 140.8 |  95 | 3.92 | 3.150 | 22.90 |   1 |   0 |    4 |    2 |
| Merc 280          | 19.2 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.30 |   1 |   0 |    4 |    4 |

From `mtcars_tbl`, reproduce the following plot, which shows the miles
per gallon (`mpg`) of car models on the x axis (see hints below).
Different models are ordered on the y axis according to their `mpg` and
their names are shown next to the data points. Also, the size of each
data point maps to its horse power (`hp`), and the color maps to number
of cylinders (`cyl`).

``` r
mtcars_tbl <- as_tibble(mtcars, rownames = "model")

mtcars_tbl <- mtcars_tbl |> arrange(mpg)
mtcars_tbl$cyl <- as.factor(mtcars_tbl$cyl)

ggplot(data = mtcars_tbl, 
       mapping = aes(x = mpg, y = reorder(model, mpg), color = cyl, size = hp)) +
  geom_point() +
  geom_text(aes(label = model), 
            hjust = -0.1,        
            size = 3, 
            color = "black") +
  scale_color_manual(
    values = c("8" = "red", "6" = "green", "4" = "blue"),
    name = "Number of cylinders"
  ) +
  scale_size_continuous(
    name = "Horsepower") +
  labs(
    x = "Miles per gallon fuel consumption",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlim(10, 42)
```

![](assignment_9_files/figure-commonmark/unnamed-chunk-15-1.png)

#### **Reflection Questions:**

#### **In this assignment, you wrote functions that operate on vectors (Exercises 1 and 2) and manipulated a data frame to create a plot (Exercise 3). Explain the difference between a simple character vector (like the one for singers) and a factor (like the model column). Why was it necessary to treat the model column as a factor and reorder its levels to create the final plot?**

The simple character vector does not have values from other columns
(variables) associated with it; however variable as factor (as in model
column) has values linked to each of the observation to other variables.
Similarly, converting a column as a factor suggests R to not stretch the
analysis beyond those values (or use only the given values) and do not
assume they fall in any scale or follow any ordered (unless otherwise
directed). Likewise, reordering the levels makes the plot less messy
(otherwise the observations are scattered and difficult to read and they
become more messy to label).

#### **Identify one of the three exercises where you used an AI assistant to help generate or debug a chunk of code. Provide the specific prompt you used. Then, show the code before you finalized it (e.g., the AI’s initial suggestion or your first attempt) and the final, working code. In your commentary, explain what was missing or incorrect in the “before” version and what you learned by testing and correcting it to produce the final version.**

I used chatgpt to check the issue with the code for reordering the
levels. The way I had written the code was suggesting to create subset
rows, which I was not trying do and chatgpt flagged that, helping me to
revise the code.
