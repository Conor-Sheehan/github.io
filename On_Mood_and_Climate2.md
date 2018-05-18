Mood & Climate
================
Conor Sheehan

Studying Mood and Climate to learn Data Science Fundamentals
------------------------------------------------------------

This tutorial is a tutorial for fundamental Data Science practices, while discovering new insights on the relationship between mood within different climates in the United States. Before we dive into the code portion, I'm going to layout exactly what we'll be studying in this tutorial.

#### Mood

Since there are many ways to think about Mood I'm going to break it into three main categories: Personal Well-Being, Social Well-Being, and Financial well-being. Personal well-being is the overall emotional, mental and physical wellness that people feel as individuals. While, social wellness is based on the strength of social ties as well as the amount of time that people spend together. Financial well-being is the overall quality of career satisfaction as well as income to support a comfortable lifestlye. All three kinds of well being are essential for the balance of a fulfilled life.

#### Climate

Climate is the overall trend of weather in a region over a long period of time. Climate is created through multiple variables creating distinct patterns over time. These environmental variables include longitude, latitude, terrain, altitude, and nearby bodies of water. All of these variables contribute to long term weather trends in a region and therefore create a specialized climate. Here's an interesting link to NASA's page on climate:

<https://www.nasa.gov/mission_pages/noaa-n/climate/climate_weather.html>

#### My Thoughts

Due to all of these variable, climates can be extremely varied and difficult to directly compare. We can't just define climate by a single variable such as average temperature, average humidity, etc. So, in this study we will explore whether there is a correlation between certain aspects of climates and whether that contributes to the mood of the population in a given region (state). I hypothesize that there will be a correlation between certain types of climate and moods because ancestrally we evolved in certain climates. We'll explore and see if we can find any answers on these questions.

Let's get started!
------------------

To get started we first need to collect our data. In this tutorial we'll focus on scraping data from websites because it's such a versatile practice.We'll start by learning basic R pipelines for scraping data and parsing it into neat data frames. This first example will be scraping a website that contains data on the climate for 100 cities across the United States.

#### Basic Scraping Practice

To scrape the data we'll first need to look at the source code of the website and find the data table that we're looking for. In this case it's easy because ther's only one table on this page. So, we find it marked by the indentifier "table" We, use the library RVEST to select the CSS for the table and then create an easy to use R Data Frame with custon column names.

#### Basic Tidying

Afterwards, I noticed that the columns were parsed in all as character vectors, but we want to work the average temperatures and precipitations as numeric values, so I use the as.numeric function to convert character vectors columns to numeric columns.

Here's the link:
<https://www.infoplease.com/science-health/weather/climate-100-selected-us-cities>

``` r
library(tidyr)
library(rvest)
library(magrittr)
library(dplyr)
library(readr)
library(tidyverse)

# Store the URL into a variable we can use later
Climate_URL <- "https://www.infoplease.com/science-health/weather/climate-100-selected-us-cities"


# Here is our first major pipeline for scraping data off websites
climate_table <- Climate_URL %>%
  read_html() %>%
  html_node("table") %>% #This line acts as a css selector to locate the table in the web page
  html_table(fill = TRUE) %>%
  set_colnames(c("City","Avg_Temp_January","Avg_Temp_April","Avg_Temp_July","Avg_Temp_October", 
                 "Precipitation_inches", "Precipitation_days", "Snowfall", "Observed_Days")) %>%
  as_data_frame ## Converting to easy to use R data frame


Climate_table_cleaned <- climate_table[-c(1, 2),]

Climate_table_cleaned$Avg_Temp_January = as.numeric(as.character(Climate_table_cleaned$Avg_Temp_January))
Climate_table_cleaned$Avg_Temp_April = as.numeric(as.character(Climate_table_cleaned$Avg_Temp_April))
Climate_table_cleaned$Avg_Temp_October = as.numeric(as.character(Climate_table_cleaned$Avg_Temp_October))
Climate_table_cleaned$Precipitation_inches = as.numeric(as.character(Climate_table_cleaned$Precipitation_inches))
Climate_table_cleaned$Precipitation_days = as.numeric(as.character(Climate_table_cleaned$Precipitation_days)) # Here's our first table!
Climate_table_cleaned
```

    ## # A tibble: 100 x 9
    ##    City     Avg_Temp_January Avg_Temp_April Avg_Temp_July Avg_Temp_October
    ##    <chr>               <dbl>          <dbl> <chr>                    <dbl>
    ##  1 Albany,…             22.2           46.6 71.1                      49.3
    ##  2 Albuque…             35.7           55.6 78.5                      57.3
    ##  3 Anchora…             15.8           36.3 58.4                      34.1
    ##  4 Ashevil…             35.8           54.1 73.0                      55.2
    ##  5 Atlanta…             42.7           61.6 80.0                      62.8
    ##  6 Atlanti…             32.1           50.6 75.3                      55.1
    ##  7 Austin,…             50.2           68.3 84.2                      70.6
    ##  8 Baltimo…             32.3           53.2 76.5                      55.4
    ##  9 Baton R…             50.1           66.6 81.7                      68.1
    ## 10 Billing…             24.0           46.1 72.0                      48.1
    ## # ... with 90 more rows, and 4 more variables: Precipitation_inches <dbl>,
    ## #   Precipitation_days <dbl>, Snowfall <chr>, Observed_Days <chr>

Success! Now, let's get data on Mood across Cities
--------------------------------------------------

Excellent, now that we collected data on the climate we will also need to also gather data on mood across different cities in the United States to unite these data sets for analysis. I found an interesting article on WalletHub that collected cumulative metrics and calculated overall happiness in different cities. They rate happiness off from the combination of emotional, financial, and communal well-being to calculate an overall score as well as rank cities individually from 1-180 in each category.

Here's the link: <https://wallethub.com/edu/happiest-places-to-live/32619/>

#### Scraping our Second HTML site

Now, that we are clear on our data, let's scrape WalletHub for their happiness metrics table. Once again the table we want to use is the first table on the site, so it is easy to locate using developer tools on Chrome. Also, in this case the table is parsed so that the ranks and score of well-being are already numeric, so we don't have to do any conversions which is nice.

``` r
library(tidyr)
library(rvest)
library(magrittr)
library(dplyr)
library(readr)
library(tidyverse)

# Store the URL into a variable we can use later
Mood_URL <- "https://wallethub.com/edu/happiest-places-to-live/32619/"


# Here is our second major pipeline for scraping data off websites
Mood_table_cleaned <- Mood_URL %>%
  read_html() %>%
  html_node("table") %>% #This line acts as a css selector to locate the table in the web page
  html_table(fill = TRUE) %>%
  set_colnames(c("OverallRank","City","Total_Score","Emotional_WB_Rank","Income_WB_Rank", "Communal_WB_Rank")) %>%
  as_data_frame ## Converting to easy to use R data frame

Mood_table_cleaned
```

    ## # A tibble: 182 x 6
    ##    OverallRank City            Total_Score Emotional_WB_Ra… Income_WB_Rank
    ##          <int> <chr>                 <dbl>            <int>          <int>
    ##  1           1 Fremont, CA            79.9                3              6
    ##  2           2 Bismarck, ND           78.4                4              1
    ##  3           3 San Jose, CA           76.4                1              8
    ##  4           4 Pearl City, HI         75.2                2             57
    ##  5           5 Plano, TX              73.6                8             17
    ##  6           6 Fargo, ND              73.5               12              2
    ##  7           7 Sioux Falls, SD        72.0                7             48
    ##  8           8 Irvine, CA             71.2               13             33
    ##  9           9 Huntington Bea…        70.7               16             25
    ## 10          10 Grand Prairie,…        70.5               29             16
    ## # ... with 172 more rows, and 1 more variable: Communal_WB_Rank <int>

#### Now we have our Two Data Sets we'll use for the rest of this tutorial.

First, we must parse the data, so that we can easily work with both of these two data sets to explore the relationship between Mood and Climate.

But, right now we have two different data sets from completely different sources. So, there will likely be some incongruency in the way they categorized and display data.

We'll need to be able to unite these tables, so we'll have to do further Parsing and Data Management to make this work.

In our case, we have data in the form of U.S cities, which is great. But cities climate within the same state don't usually differ too durastically, so they don't tell as much about climate generally. Secondly both studies used different sample sizes as well as different cities, therefore we can't directly compare the data.

Data Tidying and Management
---------------------------

First, we'll split up the (City,State) column into two columns that are CITY and STATE, so that we can unite these two data sets by respective states. This way there will be no missing data or complex joins.

``` r
library(tidyr)
library(rvest)
library(magrittr)
library(dplyr)
library(readr)
library(tidyverse)

## Let's seperate the City columns of both tables into their respective cities and states.
Regional_Mood_table = separate(data = Mood_table_cleaned, col = City, sep = ",", into = c("City", "State"))
Regional_Mood_table
```

    ## # A tibble: 182 x 7
    ##    OverallRank City      State Total_Score Emotional_WB_Ra… Income_WB_Rank
    ##          <int> <chr>     <chr>       <dbl>            <int>          <int>
    ##  1           1 Fremont   " CA"        79.9                3              6
    ##  2           2 Bismarck  " ND"        78.4                4              1
    ##  3           3 San Jose  " CA"        76.4                1              8
    ##  4           4 Pearl Ci… " HI"        75.2                2             57
    ##  5           5 Plano     " TX"        73.6                8             17
    ##  6           6 Fargo     " ND"        73.5               12              2
    ##  7           7 Sioux Fa… " SD"        72.0                7             48
    ##  8           8 Irvine    " CA"        71.2               13             33
    ##  9           9 Huntingt… " CA"        70.7               16             25
    ## 10          10 Grand Pr… " TX"        70.5               29             16
    ## # ... with 172 more rows, and 1 more variable: Communal_WB_Rank <int>

``` r
Regional_Climate_table = separate(data = Climate_table_cleaned, col = City, sep = ",", into = c("City", "State"))
Regional_Climate_table 
```

    ## # A tibble: 100 x 10
    ##    City          State     Avg_Temp_January Avg_Temp_April Avg_Temp_July
    ##    <chr>         <chr>                <dbl>          <dbl> <chr>        
    ##  1 Albany        " N.Y."               22.2           46.6 71.1         
    ##  2 Albuquerque   " N.M."               35.7           55.6 78.5         
    ##  3 Anchorage     " Alaska"             15.8           36.3 58.4         
    ##  4 Asheville     " N.C."               35.8           54.1 73.0         
    ##  5 Atlanta       " Ga."                42.7           61.6 80.0         
    ##  6 Atlantic City " N.J."               32.1           50.6 75.3         
    ##  7 Austin        " Texas"              50.2           68.3 84.2         
    ##  8 Baltimore     " Md."                32.3           53.2 76.5         
    ##  9 Baton Rouge   " La."                50.1           66.6 81.7         
    ## 10 Billings      " Mont."              24.0           46.1 72.0         
    ## # ... with 90 more rows, and 5 more variables: Avg_Temp_October <dbl>,
    ## #   Precipitation_inches <dbl>, Precipitation_days <dbl>, Snowfall <chr>,
    ## #   Observed_Days <chr>

#### Grouping by State

Since, I pointed out that we will be doing analysis on a state-wide basis rather than city-wide, we group the two tables by their states. Then, summarize the data to take averages across every city to convert into an average single state metric.

``` r
# Group Climate table by state 
State_Climate_table <- Regional_Climate_table %>% group_by(State) %>%
  summarize(Avg_Temp_April = mean(Avg_Temp_April), Avg_Temp_January = mean(Avg_Temp_January), Avg_Precipitation_inches = mean(Precipitation_inches), Avg_Precipitation_days = mean(Precipitation_days))
State_Climate_table
```

    ## # A tibble: 51 x 5
    ##    State Avg_Temp_April Avg_Temp_January Avg_Precipitati… Avg_Precipitati…
    ##    <chr>          <dbl>            <dbl>            <dbl>            <dbl>
    ##  1 " Al…           63.9             46.4             58.4            115  
    ##  2 " Al…           36.3             10.6             28.2            148  
    ##  3 " Ar…           68.1             53.0             10.2             44.5
    ##  4 " Ar…           61.4             40.1             50.9            104  
    ##  5 " Ca…           60.3             53.5             15.0             45.6
    ##  6 " Co…           49.2             27.6             12.4             80.5
    ##  7 " Co…           48.9             27.8             45.2            124  
    ##  8 " D.…           56.1             34.9             39.4            113  
    ##  9 " De…           52.4             31.5             42.8            117  
    ## 10 " Fl…           71.3             61.4             51.9            120  
    ## # ... with 41 more rows

``` r
# Group Mood table by state
State_Mood_table <- Regional_Mood_table %>% group_by(State) %>%
  summarize(Emotional_WB_Rank = mean(Emotional_WB_Rank),Communal_WB_Rank = mean(Communal_WB_Rank), Income_WB_Rank = mean(Income_WB_Rank), 
            Overall_WB_Rank = (Emotional_WB_Rank+Communal_WB_Rank+Income_WB_Rank)/3)
State_Mood_table
```

    ## # A tibble: 51 x 5
    ##    State Emotional_WB_Rank Communal_WB_Rank Income_WB_Rank Overall_WB_Rank
    ##    <chr>             <dbl>            <dbl>          <dbl>           <dbl>
    ##  1 " AK"              72.5            112             29.5            71.3
    ##  2 " AL"             167               88.0          152             136  
    ##  3 " AR"             178               91.5          132             134  
    ##  4 " AZ"              75.6             80.1           73.7            76.4
    ##  5 " CA"              53.6             73.3           76.9            67.9
    ##  6 " CO"              56.0             83.3           69.3            69.6
    ##  7 " CT"             129              148            156             145  
    ##  8 " DC"              15.0            171             31.0            72.3
    ##  9 " DE"             129              179            142             150  
    ## 10 " FL"              84.5            119             94.0            99.2
    ## # ... with 41 more rows

#### Correcting data to Unite

I notice that even though we have state columns for both data sets, one of them uses the full states name while the other uses abbreviations. So, I manually create a column to standardize both sets to have the State\_std abbreviation. Then we unite the data.

``` r
#Next we need to standardize the state column abbreviations, so that we can join the tables for data analysis.

State_Mood_table$State_std = c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL",
                               "GA","HI","IA","IL","ID","IN","KS","KY","LA","MA",
                              "MD","ME","MI","MN","MO","MS","MT","NC","ND","NE",
                              "NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI",
                              "SC","SD","TN", "TX","UT","VA","VT","WA","WI","WV",
                              "WY")
State_Mood_table
```

    ## # A tibble: 51 x 6
    ##    State Emotional_WB_Rank Communal_WB_Rank Income_WB_Rank Overall_WB_Rank
    ##    <chr>             <dbl>            <dbl>          <dbl>           <dbl>
    ##  1 " AK"              72.5            112             29.5            71.3
    ##  2 " AL"             167               88.0          152             136  
    ##  3 " AR"             178               91.5          132             134  
    ##  4 " AZ"              75.6             80.1           73.7            76.4
    ##  5 " CA"              53.6             73.3           76.9            67.9
    ##  6 " CO"              56.0             83.3           69.3            69.6
    ##  7 " CT"             129              148            156             145  
    ##  8 " DC"              15.0            171             31.0            72.3
    ##  9 " DE"             129              179            142             150  
    ## 10 " FL"              84.5            119             94.0            99.2
    ## # ... with 41 more rows, and 1 more variable: State_std <chr>

``` r
State_Climate_table$State_std = c("AL","AK","AZ","AR","CA","CO","CT","DC","DE","FL",
                               "GA","HI","IA","ID","IL","IN","KS","KY","LA","ME","MA",
                              "MD","MI","MN","MS","MO","MT","NC","ND","NH",
                              "NJ","NM", "NY", "NE","NV","OH","OK","OR","PA","RI",
                              "SC","SD","TN", "TX","UT","VA","VT","WV","WA","WI",
                              "WY")
State_Climate_table
```

    ## # A tibble: 51 x 6
    ##    State Avg_Temp_April Avg_Temp_January Avg_Precipitati… Avg_Precipitati…
    ##    <chr>          <dbl>            <dbl>            <dbl>            <dbl>
    ##  1 " Al…           63.9             46.4             58.4            115  
    ##  2 " Al…           36.3             10.6             28.2            148  
    ##  3 " Ar…           68.1             53.0             10.2             44.5
    ##  4 " Ar…           61.4             40.1             50.9            104  
    ##  5 " Ca…           60.3             53.5             15.0             45.6
    ##  6 " Co…           49.2             27.6             12.4             80.5
    ##  7 " Co…           48.9             27.8             45.2            124  
    ##  8 " D.…           56.1             34.9             39.4            113  
    ##  9 " De…           52.4             31.5             42.8            117  
    ## 10 " Fl…           71.3             61.4             51.9            120  
    ## # ... with 41 more rows, and 1 more variable: State_std <chr>

``` r
United_Table <- merge(State_Mood_table,State_Climate_table, by = "State_std")

# Now we finally have data that we can run EDA and look for trends in the data
United_Table %>% slice(1:10)
```

    ## # A tibble: 10 x 11
    ##    State_std State.x Emotional_WB_Rank Communal_WB_Rank Income_WB_Rank
    ##    <chr>     <chr>               <dbl>            <dbl>          <dbl>
    ##  1 AK        " AK"                72.5            112             29.5
    ##  2 AL        " AL"               167               88.0          152  
    ##  3 AR        " AR"               178               91.5          132  
    ##  4 AZ        " AZ"                75.6             80.1           73.7
    ##  5 CA        " CA"                53.6             73.3           76.9
    ##  6 CO        " CO"                56.0             83.3           69.3
    ##  7 CT        " CT"               129              148            156  
    ##  8 DC        " DC"                15.0            171             31.0
    ##  9 DE        " DE"               129              179            142  
    ## 10 FL        " FL"                84.5            119             94.0
    ## # ... with 6 more variables: Overall_WB_Rank <dbl>, State.y <chr>,
    ## #   Avg_Temp_April <dbl>, Avg_Temp_January <dbl>,
    ## #   Avg_Precipitation_inches <dbl>, Avg_Precipitation_days <dbl>

Exploratory Data Analysis and Machine Learning.
-----------------------------------------------

Now, that we have successfully tidied and standardized to form one united data table, we can finally start doing EDA and looking for trends in the data.

We'll start by making basic plots of a few key variables of interest. We set out to explore whether or not mood correlates to climate, but first we need to understand how well being is related to weather on a basic level. So, we're going to make some basic plots to investigate. Remember, in these plots that the WalletHub article ranked cities by their happiness. Therefore, the lower the Overall Well-Being Rank the higher ranked the state is in happiness.

#### Overall Well-Being Rank Vs. Average Temperature in January by State (Lower Rank is Higher Well Being)

Plotting and hypothesis
-----------------------

My hypothesis is that there will be a linear correlation between average winter temperature and overall well being in a state in the US. I would assume that if we live in a warmer climate year round our mood would also be improved. The null hypothesis is that there is no correlation between these two variables. So, lets test this assumption with a plot. We use the ggplot library to plot these two variables with each state labeled as the data point.

``` r
library(ggplot2)


United_Table %>%
  ggplot(aes(x=Avg_Temp_January, y=Overall_WB_Rank, label = State_std)) + geom_text() 
```

![](On_Mood_and_Climate2_files/figure-markdown_github/plot_overall-1.png)

#### Post plot analysis

After looking at our plotted data, it seems that any kind of linear relationship between Average Winter Temperature and Overall Well-Being is unlikely. Our data in this plot appears to take on more of a bell-shaped curve that would be best fit by a higher degree function rather than linear (possibly a normal density function). But let's do some statistical analyses to see if this prediction is correct.

We'll use the lm funtion (linear modeling) to run a quick and easy statistical analysis and test our hypothesis that these two variables have a linear correlation.

#### Linear model summaries and hypothesis testing

``` r
library(ggplot2)
library(tidyverse)
library(broom)

# These next to steps are calculating a linear model for average january temperature vs. Overall Well Being.
regr_model <-  lm(United_Table$Avg_Temp_January ~ United_Table$Overall_WB_Rank)
regr_model
```

    ## 
    ## Call:
    ## lm(formula = United_Table$Avg_Temp_January ~ United_Table$Overall_WB_Rank)
    ## 
    ## Coefficients:
    ##                  (Intercept)  United_Table$Overall_WB_Rank  
    ##                     25.51920                       0.07178

``` r
summary(regr_model)
```

    ## 
    ## Call:
    ## lm(formula = United_Table$Avg_Temp_January ~ United_Table$Overall_WB_Rank)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.039  -8.144  -2.862   5.861  45.507 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  25.51920    5.87097   4.347 6.96e-05 ***
    ## United_Table$Overall_WB_Rank  0.07178    0.05849   1.227    0.226    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.19 on 49 degrees of freedom
    ## Multiple R-squared:  0.02982,    Adjusted R-squared:  0.01002 
    ## F-statistic: 1.506 on 1 and 49 DF,  p-value: 0.2256

#### Poor Linear Fit

So, we can see through our fit by the adjusted R-Squared value, a linear model only explains about 1% of the variance in the regression fit. Also, our p-value is relatively high and would not match a 5% standard of significance for a linear fit between these two variables. So, we cannot reject the null hypothesis that there is no linear correlation between these two variables.

Let's try to fit a curve
------------------------

The stat\_smooth function in the ggplot methods is used to predict a best fit model for data that is more complex to analyze.

``` r
library(ggplot2)


United_Table %>%
  ggplot(aes(x=Avg_Temp_January, y=Overall_WB_Rank, label = State_std)) + geom_text() + geom_smooth()
```

![](On_Mood_and_Climate2_files/figure-markdown_github/plot_stat-1.png)

Much better fit.
----------------

As we can see with this updated plot it appears that the best fit for Winter weather vs Overall Well-Being would be a type of polynomial function along the lines of f(x) =-(x^2 - 80 x + 1480). The geom\_smooth function automatically defaults to a prediction within a 95% confidence interval for best fit, but as we can see it is a bell-shaped curve somewhat similar to a normal distribution.

#### Some Findings

After finding the curve of best fit for our plot and ruling out a direct linear correlation, we can breakdown some interesting concepts in relation to the topic at hand. Counter-intuitive to my initial thoughts warmer year round weather (as predicted by warmer winter temperatures) do not predict overall happiness and well-being in the United States. According to our best fit curve, it appears that living in weather with more extreme (higher and lower) temperatures actually predict higher rates of well-being across state populations. This means that those who live in the coldest states and well as the warmest winter states may experience higher levels of well being.

Getting back to our initial exploration about climate and mood there are many more factors to be considered. With climate we also need to include rainfall, temperatures throughout other times of year, and proximity to the coast (large bodies of water). Because this is a Intro Data Science tutorial, we will explore just one more factor and then see if we can make some generalizations about our topic at hand.

#### Overall Well-Being Rank Vs. Precipitation by State (Lower Rank is Higher Well Being)

Null Hypothesis is once again that there is no linear correlation between average precipitation and overall well being. My prediction is that the more precipitation a state gets per year the lower the populations well being (higher rank). I predict this outcome because more precipitation means more cloudy skies and one major factor for human health and well being is the vitamin D that we get from the sun.

``` r
library(ggplot2)


United_Table %>%
  ggplot(aes(x=Avg_Precipitation_inches, y=Overall_WB_Rank, label = State_std)) + geom_text() + geom_smooth(method = lm)
```

![](On_Mood_and_Climate2_files/figure-markdown_github/plot_rain-1.png) \#\#Better linear fit It appears that a linear fit for yearly rainfall and overall well being could be a better fit then for temperature in the winter time. As I predicted, this trendline appears to show that the more precipatation a state gets per year, the less overall well being. But, let's run a quick statistical analysis as well as a regression plot to see.

#### Statistical analysis and regression model for Precipitation vs. Well Being

We're going to run another summary statistics on these two variables linear fit.

``` r
library(ggplot2)
library(tidyverse)
library(broom)

# These next to steps are calculating a linear model for average precipitation vs. Overall Well Being.
regr_model <-  lm(United_Table$Avg_Precipitation_inches ~ United_Table$Overall_WB_Rank)
regr_model
```

    ## 
    ## Call:
    ## lm(formula = United_Table$Avg_Precipitation_inches ~ United_Table$Overall_WB_Rank)
    ## 
    ## Coefficients:
    ##                  (Intercept)  United_Table$Overall_WB_Rank  
    ##                      12.7420                        0.2424

``` r
summary(regr_model)
```

    ## 
    ## Call:
    ## lm(formula = United_Table$Avg_Precipitation_inches ~ United_Table$Overall_WB_Rank)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -33.890  -6.620   1.671   6.744  34.305 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  12.74202    5.61466   2.269   0.0277 *  
    ## United_Table$Overall_WB_Rank  0.24244    0.05594   4.334 7.25e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.61 on 49 degrees of freedom
    ## Multiple R-squared:  0.2771, Adjusted R-squared:  0.2624 
    ## F-statistic: 18.78 on 1 and 49 DF,  p-value: 7.252e-05

#### Reject Null-Hypothesis, but Questionable Linear Fit.

So, after running our statistical summary on overall well being vs. precipitation our p-value is much less than the 5% upper bound confidence for a linear fit. So, just based off of this value we can reject the null hypothesis that there is no linear relationship between rainfall and mood. But, as we can this model only accounts for about 26% of the variation of our fit.

So, lets create a residual plot to test whether a linear model is truly the best fit for this data. \#\#Residual Plot

``` r
# Create augmented regression model for plotting
augment_regr <- augment(regr_model)

augment_regr %>%
  ggplot(aes(x=factor(United_Table$Avg_Precipitation_inches), y=.resid)) +
    geom_point() +
    labs(title="Residual Plot",
         x = "Rainfall",
         y = "residuals")
```

![](On_Mood_and_Climate2_files/figure-markdown_github/regr_rain-1.png)

Also, after plotting the residuals, it appears that the data is not independent randomly distributed about the horizontal axis. It appears that there is a positive linear trend within the residual plot, which may mean that a linear model is not the best fit for our data.

Conclusions and Further Study
-----------------------------

After running statistical analyses on two different kinds of trends within our data we can see that basic weather patterns do not necessarily have a quickly interpretable correlation with overall mood and well-being. This is because climate is made up of many variables and so to truly do an in depth breakdown we would most likely need to create a variable that is inclusive of many different weather and location factors that effect climate.
\#\#\#\#Climate is complex After breaking down our average weather plot earlier in this tutorial it appears that to truly understand the effect of climate on our collective mood we need to take into consideration many factors at once rather than a single variable at a time. I was interested in studying this topic for a few main reasons. I wanted to know if objectively a given climate would create more overall well being (since I'm planning on moving out of Maryland after University). Secondly, I was interested to learn more about how climate change would affect our overall disposition as a culture in the United States for better or worse. But, it looks like I have more research to do. Definitively, we can say that climate and weather trends of all kinds do have a correlation with our overall mood. Here is an interesting study on psychology to learn more:

<https://psychcentral.com/blog/can-weather-affect-your-mood/>

I will continue this research, so come back to this tutorial and learn more soon!
