Analysis of the last 30 years of cereal prices
================

### **Quick introduction**

As we know, cereals have been present in our lives since the beginning
of the agrarian era. Over the centuries, we have developed agriculture,
which has brought us into more modern times. At the moment the prices of
cereals fluctuate year by year and the stock exchange reacts to many
situations in the world by modifying the prices. In the following short
analysis you can see how grain prices have developed over the last 30
years.

The analysis includes:

-   Average price of three cereal types (corn, rice, wheat) over 30
    year - excluding year 2022
-   Percentage change in prices year on year for the last 29 years
-   Visualisations of the above parameters
-   The programming language used for analysis and visualisation: R

Dataset:

-   Data sourced from kaggle.com. You can find them under this
    [link](https://www.kaggle.com/datasets/timmofeyy/-cerial-prices-changes-within-last-30-years).

Enjoy!

``` r
library(tidyverse)
library(lubridate)
library(ggthemr)
ggthemr("fresh", layout = "scientific", spacing = 1.6, text_size = 12, line_weight = 0.3)
library(knitr)
```

###### Let’s load and look at the data

``` r
# let's see the data frame
rice_wheat_corn_prices <- read_csv('rice_wheat_corn_prices.csv')
```

    ## Rows: 360 Columns: 9
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): Month
    ## dbl (8): Year, Price_wheat_ton, Price_rice_ton, Price_corn_ton, Inflation_ra...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
kable(head(rice_wheat_corn_prices)) # kable() for better better data frame output
```

| Year | Month | Price\_wheat\_ton | Price\_rice\_ton | Price\_corn\_ton | Inflation\_rate | Price\_wheat\_ton\_infl | Price\_rice\_ton\_infl | Price\_corn\_ton\_infl |
|-----:|:------|------------------:|-----------------:|-----------------:|----------------:|------------------------:|-----------------------:|-----------------------:|
| 1992 | Feb   |            170.12 |           278.25 |           113.62 |           89.59 |                  322.53 |                 527.53 |                 215.41 |
| 1992 | Mar   |            161.44 |           277.20 |           117.00 |           89.59 |                  306.07 |                 525.54 |                 221.82 |
| 1992 | Apr   |            153.07 |           278.00 |           108.52 |           89.59 |                  290.21 |                 527.06 |                 205.74 |
| 1992 | May   |            139.72 |           274.00 |           109.64 |           89.59 |                  264.90 |                 519.48 |                 207.87 |
| 1992 | Jun   |            140.36 |           268.80 |           110.90 |           89.59 |                  266.11 |                 509.62 |                 210.26 |
| 1992 | Jul   |            129.93 |           278.50 |           102.75 |           89.59 |                  246.33 |                 528.01 |                 194.80 |

``` r
kable(tail(rice_wheat_corn_prices)) 
```

| Year | Month | Price\_wheat\_ton | Price\_rice\_ton | Price\_corn\_ton | Inflation\_rate | Price\_wheat\_ton\_infl | Price\_rice\_ton\_infl | Price\_corn\_ton\_infl |
|-----:|:------|------------------:|-----------------:|-----------------:|----------------:|------------------------:|-----------------------:|-----------------------:|
| 2021 | Aug   |            276.18 |              403 |           256.61 |           -1.29 |                  272.62 |                 397.80 |                 253.30 |
| 2021 | Sep   |            263.60 |              400 |           235.62 |           -1.29 |                  260.20 |                 394.84 |                 232.58 |
| 2021 | Oct   |            334.50 |              401 |           239.65 |           -1.29 |                  330.18 |                 395.83 |                 236.56 |
| 2021 | Nov   |            327.82 |              400 |           248.72 |           -1.29 |                  323.59 |                 394.84 |                 245.51 |
| 2021 | Dec   |            332.06 |              400 |           264.54 |           -1.29 |                  327.78 |                 394.84 |                 261.13 |
| 2022 | Jan   |                NA |              427 |           276.62 |              NA |                      NA |                     NA |                     NA |

###### Before we start the analysis, let’s look at the data structure and change a few things to get to the next steps.

``` r
glimpse(rice_wheat_corn_prices)
```

    ## Rows: 360
    ## Columns: 9
    ## $ Year                 <dbl> 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1~
    ## $ Month                <chr> "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", ~
    ## $ Price_wheat_ton      <dbl> 170.12, 161.44, 153.07, 139.72, 140.36, 129.93, 1~
    ## $ Price_rice_ton       <dbl> 278.25, 277.20, 278.00, 274.00, 268.80, 278.50, 2~
    ## $ Price_corn_ton       <dbl> 113.62, 117.00, 108.52, 109.64, 110.90, 102.75, 9~
    ## $ Inflation_rate       <dbl> 89.59, 89.59, 89.59, 89.59, 89.59, 89.59, 89.59, ~
    ## $ Price_wheat_ton_infl <dbl> 322.53, 306.07, 290.21, 264.90, 266.11, 246.33, 2~
    ## $ Price_rice_ton_infl  <dbl> 527.53, 525.54, 527.06, 519.48, 509.62, 528.01, 5~
    ## $ Price_corn_ton_infl  <dbl> 215.41, 221.82, 205.74, 207.87, 210.26, 194.80, 1~

###### Using the match() function I change the values in the column from “Jan” to “1” etc. This may be helpful for further analysis. R sometimes does not recognise abbreviations.

``` r
rice_wheat_corn_prices$Month <-  match(rice_wheat_corn_prices$Month, month.abb)
glimpse(rice_wheat_corn_prices)
```

    ## Rows: 360
    ## Columns: 9
    ## $ Year                 <dbl> 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1~
    ## $ Month                <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5~
    ## $ Price_wheat_ton      <dbl> 170.12, 161.44, 153.07, 139.72, 140.36, 129.93, 1~
    ## $ Price_rice_ton       <dbl> 278.25, 277.20, 278.00, 274.00, 268.80, 278.50, 2~
    ## $ Price_corn_ton       <dbl> 113.62, 117.00, 108.52, 109.64, 110.90, 102.75, 9~
    ## $ Inflation_rate       <dbl> 89.59, 89.59, 89.59, 89.59, 89.59, 89.59, 89.59, ~
    ## $ Price_wheat_ton_infl <dbl> 322.53, 306.07, 290.21, 264.90, 266.11, 246.33, 2~
    ## $ Price_rice_ton_infl  <dbl> 527.53, 525.54, 527.06, 519.48, 509.62, 528.01, 5~
    ## $ Price_corn_ton_infl  <dbl> 215.41, 221.82, 205.74, 207.87, 210.26, 194.80, 1~

###### Let’s look for the missing values

``` r
# any missing data?
sum(is.na(rice_wheat_corn_prices))
```

    ## [1] 5

###### As you can see we have 5 NA’s. Let’s do a quick summary of the available variables to find out which columns contain the missing values.

``` r
# check the numbers
summary(rice_wheat_corn_prices)
```

    ##       Year          Month       Price_wheat_ton Price_rice_ton 
    ##  Min.   :1992   Min.   : 1.00   Min.   : 85.3   Min.   :163.8  
    ##  1st Qu.:1999   1st Qu.: 3.75   1st Qu.:137.3   1st Qu.:261.6  
    ##  Median :2007   Median : 6.50   Median :175.3   Median :344.7  
    ##  Mean   :2007   Mean   : 6.50   Mean   :185.3   Mean   :364.1  
    ##  3rd Qu.:2014   3rd Qu.: 9.25   3rd Qu.:220.3   3rd Qu.:444.2  
    ##  Max.   :2022   Max.   :12.00   Max.   :419.6   Max.   :907.0  
    ##                                 NA's   :1                      
    ##  Price_corn_ton   Inflation_rate  Price_wheat_ton_infl Price_rice_ton_infl
    ##  Min.   : 75.27   Min.   :-1.29   Min.   :136.2        Min.   : 246.0     
    ##  1st Qu.:104.19   1st Qu.:12.37   1st Qu.:193.0        1st Qu.: 397.5     
    ##  Median :149.75   Median :28.30   Median :228.3        Median : 455.5     
    ##  Mean   :155.50   Mean   :36.32   Mean   :241.7        Mean   : 474.0     
    ##  3rd Qu.:176.65   3rd Qu.:59.70   3rd Qu.:275.8        3rd Qu.: 540.2     
    ##  Max.   :333.05   Max.   :89.59   Max.   :518.5        Max.   :1120.7     
    ##                   NA's   :1       NA's   :1            NA's   :1          
    ##  Price_corn_ton_infl
    ##  Min.   :116.3      
    ##  1st Qu.:159.0      
    ##  Median :186.0      
    ##  Mean   :201.2      
    ##  3rd Qu.:220.9      
    ##  Max.   :385.9      
    ##  NA's   :1

###### Scrolling up to the data table, we see that our NA’s are in the row for 2022. Luckily, we didn’t want to include this date in the analysis so the missing values will be automatically filtered out.

### **Annual average price for each cereal type**

``` r
yearly_price <- rice_wheat_corn_prices %>%
  filter(Year != 2022) %>%
  group_by(Year) %>% 
  summarise_at(vars('Price_wheat_ton', 'Price_rice_ton', 'Price_corn_ton'), mean)

kable(yearly_price)
```

| Year | Price\_wheat\_ton | Price\_rice\_ton | Price\_corn\_ton |
|-----:|------------------:|-----------------:|-----------------:|
| 1992 |         143.13091 |         267.4000 |        103.78545 |
| 1993 |         134.77417 |         235.4125 |        102.09000 |
| 1994 |         138.55500 |         267.5917 |        107.55000 |
| 1995 |         167.35833 |         320.9583 |        123.48750 |
| 1996 |         187.41917 |         338.8750 |        165.80917 |
| 1997 |         143.64833 |         303.5100 |        117.09167 |
| 1998 |         111.51500 |         304.1583 |        101.98667 |
| 1999 |          96.27833 |         248.4167 |         90.21667 |
| 2000 |          98.91417 |         202.3958 |         88.53417 |
| 2001 |         107.72417 |         172.8383 |         89.64083 |
| 2002 |         129.95750 |         191.8733 |         99.27083 |
| 2003 |         138.57500 |         197.6167 |        105.37083 |
| 2004 |         144.44333 |         237.6667 |        111.80417 |
| 2005 |         135.72000 |         286.2708 |         98.67167 |
| 2006 |         158.96750 |         304.8767 |        121.85167 |
| 2007 |         238.58833 |         326.4325 |        163.66417 |
| 2008 |         271.52000 |         650.1875 |        223.11750 |
| 2009 |         185.95333 |         554.9933 |        165.51083 |
| 2010 |         229.67583 |         488.9067 |        185.91417 |
| 2011 |         285.91250 |         543.0292 |        291.68417 |
| 2012 |         295.36583 |         562.9833 |        298.41667 |
| 2013 |         276.73417 |         505.8917 |        259.39000 |
| 2014 |         245.21167 |         422.8333 |        192.88000 |
| 2015 |         206.37750 |         386.0000 |        169.75083 |
| 2016 |         176.30333 |         396.1667 |        159.16167 |
| 2017 |         178.18167 |         398.9167 |        154.53083 |
| 2018 |         203.89000 |         420.6667 |        164.41500 |
| 2019 |         211.27667 |         418.0000 |        170.07000 |
| 2020 |         227.73917 |         496.7500 |        165.46667 |
| 2021 |         285.86083 |         458.2500 |        259.54583 |

###### I modify the columns using pivot\_longer() so that I can visualise the results more easily.

``` r
yearly_price_pivoted <- yearly_price %>% 
  pivot_longer(2:4, names_to = 'Cerial_type', values_to = "Avg_Price")
```

###### Price visualisation over the years.

``` r
# Viz it
ggplot(yearly_price_pivoted, aes(Year, Avg_Price, group = Cerial_type)) +
  geom_line(aes(color = Cerial_type)) +
  geom_point(aes(color = Cerial_type))+
  scale_y_continuous(breaks = seq(0, 800, by = 40))+
  scale_x_continuous(breaks = seq(1992, 2021, by = 1)) +
  geom_vline(xintercept = 2008 , linetype="dashed", color = "grey60") +
  annotate(geom ="text",x = 2009, y = 355, label="Global Financial \n Crisis 2007-2008", size = 2, color = "grey60") +

  labs(x = "Year", y = "Average price per year", title = "Average price of three cereal types over 30 years")+
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(margin = margin(t = 10), size = 15,),
        axis.title.y = element_text(margin = margin(r = 10), size = 15),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_manual(name = "Cereal types", labels = c("Corn", "Rice", "Wheat"), values = c("gold2", "gray30", "darkolivegreen"))
```

![Average price of three cereal types over 30 years](https://user-images.githubusercontent.com/101715443/159247515-38ac44e4-931a-4c2f-a65e-addc6e75cea5.png)

``` r
ggsave('Average price of three cereal types over 30 years', device = "png",
         width = 10, height = 6, units = "in", dpi = 600, type = "cairo-png")
```

    ## Warning: Using ragg device as default. Ignoring `type` and `antialias` arguments

### **Year-on-year percentage change**

###### Let’s see how prices change year on year

``` r
# get the year on year percentage change
yearly_price_percentage_change <-  yearly_price_pivoted %>% 
  group_by(Cerial_type) %>% 
  mutate(
    Year_on_year = (Avg_Price - lag(Avg_Price)) / lag(Avg_Price)
  )
  # round the numbers
yearly_price_percentage_change <- yearly_price_percentage_change %>% 
  mutate(
    Year_on_year = round(Year_on_year * 100, 2)
  )

kable(tail(yearly_price_percentage_change, 15)) # last 3 years
```

| Year | Cerial\_type      | Avg\_Price | Year\_on\_year |
|-----:|:------------------|-----------:|---------------:|
| 2017 | Price\_wheat\_ton |   178.1817 |           1.07 |
| 2017 | Price\_rice\_ton  |   398.9167 |           0.69 |
| 2017 | Price\_corn\_ton  |   154.5308 |          -2.91 |
| 2018 | Price\_wheat\_ton |   203.8900 |          14.43 |
| 2018 | Price\_rice\_ton  |   420.6667 |           5.45 |
| 2018 | Price\_corn\_ton  |   164.4150 |           6.40 |
| 2019 | Price\_wheat\_ton |   211.2767 |           3.62 |
| 2019 | Price\_rice\_ton  |   418.0000 |          -0.63 |
| 2019 | Price\_corn\_ton  |   170.0700 |           3.44 |
| 2020 | Price\_wheat\_ton |   227.7392 |           7.79 |
| 2020 | Price\_rice\_ton  |   496.7500 |          18.84 |
| 2020 | Price\_corn\_ton  |   165.4667 |          -2.71 |
| 2021 | Price\_wheat\_ton |   285.8608 |          25.52 |
| 2021 | Price\_rice\_ton  |   458.2500 |          -7.75 |
| 2021 | Price\_corn\_ton  |   259.5458 |          56.86 |

###### Vizualize it.

``` r
ggplot(yearly_price_percentage_change, aes(Year, Year_on_year, fill = Cerial_type)) +
    geom_col(position = "dodge") +
    scale_x_continuous(breaks = seq(1992, 2021, by = 1))+
    scale_y_continuous(breaks = seq(-40, 100, by = 10)) + 
    facet_grid(rows = vars(Cerial_type)) +
    geom_hline(yintercept = 0 , linetype="dashed", color = "gray80") +
    theme_bw() +
    labs(x = "Year", y = "Percentage change", title = "Percentage change in prices year on year for the last 28 years") +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_text(margin = margin(t = 10), size = 15,),
          axis.title.y = element_text(margin = margin(r = 10), size = 15),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
          axis.text = element_text(size = 12),
          strip.text.y = element_blank()) +
    scale_fill_manual(name = "Cereal types", labels=c("Corn","Rice","Wheat"), values = c("gold2", "grey", "darkolivegreen"))
```

    ## Warning: Removed 3 rows containing missing values (geom_col).

![Percentage change in prices year on year for the last 28 years](https://user-images.githubusercontent.com/101715443/159247537-e53fccf9-64fb-4942-a1a9-f388a1ed8572.png)

``` r
ggsave('Percentage change in prices year on year for the last 28 years', device = "png",
         width = 10, height = 8, units = "in", dpi = 600, type = "cairo-png")
```

    ## Warning: Using ragg device as default. Ignoring `type` and `antialias` arguments

    ## Warning: Removed 3 rows containing missing values (geom_col).

### **Conclusions:**

The price of cereals is not stable and is highly dependent on world
crises. This can be clearly seen in the graph showing price changes over
the years. The year 2008 and the crisis that occurred then caused an
increase in the prices of all three
