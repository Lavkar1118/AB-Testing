AB_testing_Part1
================
Lavanya Muthukumar
5/19/2022

``` r
library(tidyverse)
library(ggplot2)
library(data.table)
```

### **E-Learning Company Webpage A/B Testing - Part 1**

#### **Key Project Steps**

–

1.  Problem statement
2.  Dataset description
3.  Data cleaning and wrangling
4.  A/B testing
5.  Regression Approach
6.  Consideration of confounders
7.  Recommendation and conclusion

–

1.  **Problem Statement**

An e-learning company develops a new webpage aimed to increase the
number of users who enroll in their data analyst course.They employed a
metric called converting to track how many users visiting this new page
enrolled in the course compared to the users who were subjected to the
old webpage using a randomized experiment. We will now run an A/B test
to see if the new page is efficient over the old page and if it should
be implemented. Further, the company also wanted to analyze the success
of the experiment based on a user’s country of residence across three
different countries - US, UK and Canada.Our goal here is to analyze
results across both groups and help the company decide if the new page
is worth implementing.

2.  **Dataset description**

``` r
ab_data <- read.csv("Data/part1_results.csv")

head(ab_data, 10)
```

    ##    user_id                  timestamp     group landing_page converted
    ## 1   851104 2017-01-21 22:11:48.556739   control     old_page         0
    ## 2   804228 2017-01-12 08:01:45.159739   control     old_page         0
    ## 3   661590 2017-01-11 16:55:06.154213 treatment     new_page         0
    ## 4   853541 2017-01-08 18:28:03.143765 treatment     new_page         0
    ## 5   864975 2017-01-21 01:52:26.210827   control     old_page         1
    ## 6   936923 2017-01-10 15:20:49.083499   control     old_page         0
    ## 7   679687 2017-01-19 03:26:46.940749 treatment     new_page         1
    ## 8   719014 2017-01-17 01:48:29.539573   control     old_page         0
    ## 9   817355 2017-01-04 17:58:08.979471 treatment     new_page         1
    ## 10  839785 2017-01-15 18:11:06.610965 treatment     new_page         1

``` r
str(ab_data)
```

    ## 'data.frame':    294478 obs. of  5 variables:
    ##  $ user_id     : int  851104 804228 661590 853541 864975 936923 679687 719014 817355 839785 ...
    ##  $ timestamp   : chr  "2017-01-21 22:11:48.556739" "2017-01-12 08:01:45.159739" "2017-01-11 16:55:06.154213" "2017-01-08 18:28:03.143765" ...
    ##  $ group       : chr  "control" "control" "treatment" "treatment" ...
    ##  $ landing_page: chr  "old_page" "old_page" "new_page" "new_page" ...
    ##  $ converted   : int  0 0 0 0 1 0 1 0 1 1 ...

2.1 **Preliminary data exploration**

``` r
total_rows <- nrow(ab_data)

unique_user <- as.integer(length(unique(ab_data$user_id)))

missing_rows <- nrow(is.null(ab_data))


#checking conversion rates

#for unique users
conv_rate_uni <- round( length(unique(ab_data$user_id[ab_data$converted == 1]))/length(unique(ab_data$user_id)), 4)


#for all users

conv_rate_all <- round(length(ab_data$user_id[ab_data$converted == 1])/length(ab_data$user_id),4 )

conv_rate_all
```

    ## [1] 0.1197

``` r
#check for misalignment between the variables group vs landing 

mismatch_original_data <- ab_data %>% filter(landing_page == "new_page" & group != "treatment" | landing_page == "old_page" & group == "treatment") %>% summarise(n =n() )
```

    ## No.of rows = 294478

    ## No.of unique users = 290584

    ## No.of missing rows = 0

    ## Conversion rates based on unique users = 0.121

    ## Conversion rates based on all users = 0.1197

    ## No. of rows where group and landing page have mismatch = 3893

3.  **Data Cleaning and Wrangling**

``` r
#create new dataframe by removing rows with misaligned data

ab_data_2 <- ab_data %>% filter(landing_page == "new_page" & group == "treatment" | landing_page == "old_page" & group  == "control") 

#recheck for row count, unique rows, missing values, misaligned rows

total_rows_2 <- nrow(ab_data_2)

missing_rows_2 <- nrow(is.null(ab_data_2))

unique_user2 <- length(unique(ab_data_2$user_id))

mismatch_original_data_2 <- ab_data_2 %>% filter(landing_page == "new_page" & group != "treatment" | landing_page == "old_page" & group == "treatment") %>% summarise(n =n() )

#identify duplicate entry by user id and the corresponding row information and drop them

dup_entry <- ab_data_2$user_id[duplicated(ab_data_2$user_id)]

dup_entry_count <- ab_data_2 %>% filter(user_id == 773192) %>% group_by(user_id) %>% summarise(n= n())

no_of_dup_entry <- nrow(dup_entry_count)

dup_entry_row_info <- ab_data_2 %>% filter(user_id == 773192)


ab_data_2 <- distinct(ab_data_2, user_id, .keep_all = T)

#recheck row number after dropping duplicates

total_rows_2.1 <- nrow(ab_data_2)
```

    ## No.of rows = 290585

    ## No.of unique users = 290584

    ## No.of missing rows = 0

    ## No.of rows where group and landing page have mismatch = 0

    ## No.of duplicate users = 1

    ## User id of duplicate entry = 773192entered2times

    ## No.of rows after removing duplicates = 290584

4.  **AB Testing**

4.1 **Probabilities and Conversion Rates**

``` r
#conversion rate for overall sample irrespective of the group

overall_conv <- round( length(ab_data_2$user_id[ab_data_2$converted ==1])/ unique_user2,4)

#conversion rates by groups

#control group

unique_cnt <- length(ab_data_2$user_id[ab_data_2$group =="control"])

cnt_conv <- round(length(ab_data_2$user_id[ab_data_2$converted ==1 & ab_data_2$group =="control"])/ unique_cnt, 4 )

#treatment group

unique_trt <- length(ab_data_2$user_id[ab_data_2$group =="treatment"])

trt_conv <- round(length(ab_data_2$user_id[ab_data_2$converted ==1 & ab_data_2$group =="treatment" ])/ unique_trt, 4)


#new page receiving probability

new_page_prob <- length(ab_data_2$user_id[ab_data_2$landing_page =="new_page" ])/ length(ab_data_2$user_id)

#difference in conversion rates

diff_conversion <- round(trt_conv - cnt_conv, 4)
```

    ## Overall conversion rates irrespective of page type = 0.1196

    ## Conversion rates among control group = 0.1204

    ## Conversion rates among treatment group = 0.1188

    ## Differnce in conversion rates between treatment and control group = -0.0016

    ## Probability that the indvidual received the new page = 0.500061944222669

4.2 **Hypothesis Formulation**

**Null:** The conversion rates between the new page (p_new) and old page
(p_old) are the same and equals the overall conversion rate in the
sample (0.1196).

**Alternate:** The conversion rates between new page and old page are
different

``` r
#conversion rates p_new under the null

p_new <- overall_conv

#conversion rates p_old under the null
p_old <- overall_conv

#number of individuals in treatment group
n_new <- unique_trt

#number of individuals in control group
n_old <- unique_cnt
```

    ## conversion rates p_new under the null = 0.1196

    ## conversion rates p_old under the null = 0.1196

    ## number of individuals in treatment group = 145310

    ## number of individuals in control group = 145274

4.3 **Sampling Distribution**

-   We will now perform the sampling distribution for the difference in
    converted between the two pages for over 10,000 iterations based on
    the conversion estimate from the null.

-   The sample sizes drawn will correspond to the original sample size
    of the individual groups( n_old = 145274 ; n_new =145310)

``` r
#Simulating n_new  

# new_page_converted <- sample(0:1,145274 ,T, prob= c(0.8804,0.1196))

new_page_converted <- sample(0:1,145274 ,T, prob= c(1-overall_conv ,overall_conv))

sim_new_page_converted <- length(new_page_converted[new_page_converted==1])/length(new_page_converted)

#Simulating n_old 

old_page_converted <- sample(0:1,145310 ,T, prob= c(1-overall_conv ,overall_conv))

sim_old_page_converted <- length(old_page_converted[new_page_converted==1])/length(old_page_converted)

#calculating the difference in conversion rates between both groups based on simulated values:
simulated_diff <- round(mean(new_page_converted) - mean(old_page_converted),4)
```

    ## Simulated conversion rate for the new page = 0.118672301994851

    ## Simulated conversion rate for the old page = 0.118670428738559

    ## Difference in conversion rates between new and old page based on simulated data = -0.0015

``` r
#Simulating 10,000 p_new - p_old  values

#first create function perm_fun

perm_fun <- function(x,nA, nB)
{
  n <- nA + nB
  idx_b <- sample(1:n, nB)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

obs_pct_dff <- 100 * (trt_conv - cnt_conv )

overall_converted_count <- length(ab_data_2$user_id[ab_data_2$converted ==1])
  
overall_non_converted_count <- length(ab_data_2$user_id[ab_data_2$converted ==0])
  
conversion <- c(rep(0,overall_non_converted_count ), rep(1,overall_converted_count))
 
p_diffs <- rep(0,10000)
 
 for (i in 1:10000) {
   p_diffs[i] = 100* perm_fun(conversion, unique_trt,unique_cnt)
 }
```

``` r
hist (p_diffs, col = "skyblue", xlab= "Simultaed differnce in rates(percent)", main = "Distribution of conversion rate differences from \n 10,000 simulated iterations", cex.main = 1, cex.axis = 1, cex.lab = 1, font.lab = 2) 

abline(v= abs(obs_pct_dff), col = "red", lwd = 2.5, lty  = "dashed")

abline(v= -abs(obs_pct_dff), col = "red", lwd = 2.5, lty  = "dashed")
```

![](AB_testing_Part1_files/figure-gfm/step_3%20-%20plotting%20the%20difference%20in%20conversion%20rates%20across%20each%20iteration%20using%20a%20histogram-1.png)<!-- -->

    ## Observed difference in conversion rates  = -0.159999999999999

    ## Proportion of samples where the absolute difference between rates was greater than the absolute observed difference = 0.185

``` r
convert_old <- 17489

convert_new <- 17263

prop.results <- prop.test(x = c (convert_new, convert_old), n = c(n_new, n_old), alternative = "two.sided")

prop.results
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  c(convert_new, convert_old) out of c(n_new, n_old)
    ## X-squared = 1.7186, df = 1, p-value = 0.1899
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  -0.0039515954  0.0007813537
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.1188012 0.1203863

4.4 **Inference:**

We tried to see if the observed difference in conversion rates is due to
chance (i.e. what is the probability of getting a differnce as extereme
as the one observed in the data) by computing a sampling distribution of
difference across 10,000 samples and computed the probablity to validate
our hypothesis. Based on the obtained value there is a 19% probability
we can observe a difference as extreme as \|-0.0016\|.

5.  **Regression approach**

Logistic regression is performed to confirm the results obtained
previously

5.1 **Logistic Regression**

``` r
#Adding an intercept column, as well as an ab_page column, which is 1 when an individual receives the treatment and 0 if control.

ab_data_2 <- ab_data_2 %>% mutate(ab_page = ifelse(group == "control", 0, 1), intercept = 1)


head(ab_data_2) 
```

    ##   user_id                  timestamp     group landing_page converted ab_page
    ## 1  851104 2017-01-21 22:11:48.556739   control     old_page         0       0
    ## 2  804228 2017-01-12 08:01:45.159739   control     old_page         0       0
    ## 3  661590 2017-01-11 16:55:06.154213 treatment     new_page         0       1
    ## 4  853541 2017-01-08 18:28:03.143765 treatment     new_page         0       1
    ## 5  864975 2017-01-21 01:52:26.210827   control     old_page         1       0
    ## 6  936923 2017-01-10 15:20:49.083499   control     old_page         0       0
    ##   intercept
    ## 1         1
    ## 2         1
    ## 3         1
    ## 4         1
    ## 5         1
    ## 6         1

``` r
#the model

reg_res <- glm(converted ~ ab_page, data = ab_data_2, family = "binomial")

logistic_results <- summary(reg_res)

Odds_ratio<- exp(summary(reg_res)$coefficients[2,1])
```

    ## 
    ## Call:
    ## glm(formula = converted ~ ab_page, family = "binomial", data = ab_data_2)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5065  -0.5065  -0.5030  -0.5030   2.0641  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept) -1.988777   0.008062 -246.671   <2e-16 ***
    ## ab_page     -0.014989   0.011434   -1.311     0.19    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 212778  on 290583  degrees of freedom
    ## Residual deviance: 212776  on 290582  degrees of freedom
    ## AIC: 212780
    ## 
    ## Number of Fisher Scoring iterations: 4

    ## Odds_ratio from the logistic model = 0.98512266411068

The odds ratio indicates that users subjected to new_page are 2% less
likely to be enroll in the data analytics course compared to users
visiting the old page.

5.2 **Inference:**

The logistic model provides evidence that there is no statistically
significant difference in conversion rates between pages (p-value:0.19).

Thus we fail to reject our null and can conclude that the new page does
not increase the enrollment rates.

6.  **Consideration of confounders**

Here, we test the effect that the country that a user lives in, has on
the conversion rate

6.1 **Logistic regression with multiple variables**

``` r
#loading additional data
countries <- read.csv("Data/part1_countries.csv")

ab_data_2 <- merge(ab_data_2,countries, "user_id")

#creating dummy variables

ab_data_2 <- ab_data_2 %>% mutate(US = ifelse(country == "US", 1, 0), UK = ifelse(country == "UK", 1, 0), CA = ifelse(country == "CA", 1, 0))

#conversion rates by country

ab_data_2 %>% select(converted,country) %>% group_by(country, converted) %>% summarise(n = n())  %>%
  mutate(freq = n / sum(n))
```

    ## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.

    ## # A tibble: 6 x 4
    ## # Groups:   country [3]
    ##   country converted      n  freq
    ##   <chr>       <int>  <int> <dbl>
    ## 1 CA              0  12827 0.885
    ## 2 CA              1   1672 0.115
    ## 3 UK              0  63727 0.879
    ## 4 UK              1   8739 0.121
    ## 5 US              0 179277 0.880
    ## 6 US              1  24342 0.120

``` r
#US- 0.120, UK = 0.121, CA = 0.115

#adding columns to look at an interaction between page and country to see if there significant effects on conversion

ab_data_2 <- ab_data_2 %>% mutate(US_con = ab_page * US, UK_con = ab_page * UK, CA_con = ab_page * CA)
```

``` r
#new logistic model

reg_2 <- glm(converted ~  US_con + UK_con + CA_con, data = ab_data_2, family = "binomial")

logistic_results_country <- summary (reg_2)

odds_ratio_country <- exp(reg_2$coefficients)
```

    ## 
    ## Call:
    ## glm(formula = converted ~ US_con + UK_con + CA_con, family = "binomial", 
    ##     data = ab_data_2)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5083  -0.5065  -0.5065  -0.5022   2.0929  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept) -1.988777   0.008062 -246.671   <2e-16 ***
    ## US_con      -0.018264   0.012608   -1.449   0.1475    
    ## UK_con       0.007389   0.018030    0.410   0.6819    
    ## CA_con      -0.082677   0.037989   -2.176   0.0295 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 212778  on 290583  degrees of freedom
    ## Residual deviance: 212771  on 290580  degrees of freedom
    ## AIC: 212779
    ## 
    ## Number of Fisher Scoring iterations: 4

    ## (Intercept)      US_con      UK_con      CA_con 
    ##   0.1368627   0.9819014   1.0074168   0.9206487

6.2 **Inference**

When taking country of residence into consideration, based on the
obtained p-values, except for Canada the country of residence did not
have a significant effect in the conversion rates between pages. And
even in Canada the direction of association is opposite to what might be
observed when the new page is successful.

7.  **Recommendation and conclusion**

-   Based on the analysis of the AB test results using different
    techniques like z-test and regression, it can be concluded that the
    difference in percentage of users enrolling in the free trial of the
    data science program between the new and old web page is not
    statistically significant.

-   When the influence of the countries that the users resided in was
    taken into account, the difference was not statistically different
    for UK and US. For Canada however, it was observed that the new web
    page is about 8% less likely to convert users compared to the old
    page.

-   Based on these results, the recommendation to the e-commerce company
    is to not launch the new web page
