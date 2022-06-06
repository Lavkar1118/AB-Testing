AB_testing_Part2
================
Lavanya Muthukumar
5/24/2022

``` r
library(tidyverse)
library(ggplot2)
library(data.table)
```

### **E-Learning Company Webpage A/B Testing - Part 2**

#### **Key Project Steps**

–

1.  Project statement and goals
2.  Baseline data
3.  Standard error of metrics
4.  Experiment Sizing, Duration and Exposure
5.  Experimental Analysis
6.  Recommendation and conclusion

–

1.  **Problem statement:**

The company then pivoted its goal from increasing free enrollments to
maximizing its user retention rate which would also result in a decrease
in overall free enrollments. This was being implemented to streamline
the allocation of most study resources and instructor support towards
students who were more likely to complete the course. The expreiment
involved a prompt message clearly stating the time commitment
expectations of the course to deter users from enrolling and then
dropping out before the end of the trial. So, the goal here was test if
the message prompt resulted in fewer gross course enrollments along with
an increased retention in course past the 14 day trial period (making at
least one payment).

2.  **Baseline Data**

``` r
options(scipen = 999) # for some reason the raw_value col was formatted in scientific notation. code used  to obtain values in regular integer format.

base_line <- read.csv("Data/part2_baseline_values.csv")

head(base_line)
```

    ##                                              Variable raw_values
    ## 1 Unique cookies to view course overview page per day   40000.00
    ## 2  Unique cookies to click "Start free trial" per day    3200.00
    ## 3                                 Enrollments per day     660.00
    ## 4     Click-through-probability on "Start free trial"       0.08
    ## 5               Probability of enrolling, given click       0.21
    ## 6                Probability of payment, given enroll       0.53

3.  **Standard Error of Metrics**

**The metrics chosen were**:

-   Gross conversion
-   Net conversion
-   Retention

``` r
#metrics


no_of_unique_cookies <- 40000

cookies_click_free_trail <- 3200

enrollments <- 660

no_of_payments <- round(0.53 * 660, 0) 
#(note: probability of payment, given enroll * enrollment)


gross_conversion = round(enrollments/cookies_click_free_trail, 4 )

net_conversion = round(no_of_payments/cookies_click_free_trail,4)

retention = round(no_of_payments/enrollments,4)
```

    ## Baseline gross conversion = 0.2062

    ## Baseline net conversion = 0.1094

    ## Baseline retention = 0.1094

``` r
#standard error = sqrt(p(1-p)/n)

#standard error based on a sample size of 50000

gross_conversion_se_ana <- round(sqrt(0.21*(1-0.21)/(400)),4)

net_conversion_se_ana <- round(sqrt(0.11*(1-0.11)/ (400)),4)

#enrollment rate = 660/3200 = 0.206; for the given clicks of 400, the enrollment would be 400*0.206 = 82.5

retention_se_ana <- round(sqrt(0.53*(1-0.53)/ (82.5)),4)
```

    ## Std.error for gross conversion = 0.0204

    ## Std.error for net conversion = 0.0156

    ## Std.error for retention = 0.0549

4.  **Experiment Sizing, Duration and Exposure**

4.1 **Sizing**

Calculating number of Page views across both groups to adequately power
the experiment.

Method: Without using the Bonferroni correction; calculated using a
sample size calculator from
[here](https://www.evanmiller.org/ab-testing/sample-size.html) for
alpha= 0.05 and beta= 0.2

**Gross Conversion**

-   Baseline Conversion: 20.625%
-   Minimum Detectable Effect: 1%
-   alpha: 5%
-   beta: 20%
-   1 - beta: 80%
-   sample size = 25,835 enrollments/group
-   Number of groups = 2 (experiment and control)
-   total sample size = 51,670 enrollments
-   clicks/pageview: 3200/40000 = .08 clicks/pageview
-   pageviews = 645,875

**Retention**

-   Baseline Conversion: 53%
-   Minimum Detectable Effect: 1%
-   alpha: 5%
-   beta: 20%
-   1 - beta: 80%
-   sample size = 39,155 enrollments/group
-   Number of groups = 2 (experiment and control)
-   total sample size = 78,230 enrollments
-   enrollments/pageview: 660/40000 = .0165
-   pageviews = 78,230/.0165 = 4,741,212

**Net Conversion**

-   Baseline Conversion: 10.9313%
-   Minimum Detectable Effect: .75%
-   alpha: 5%
-   beta: 20%
-   1 - beta: 80%
-   sample size = 27,413 enrollments/group
-   Number of groups = 2 (experiment and control)
-   total sample size = 54,826
-   clicks/pageview: 3200/40000 = .08 clicks/pageview
-   pageviews = 685,325

**Based on the above results, the maximum pageviews required to complete
the new experiment is 4,741,212**

4.2 **Duration and Exposure**

If we use the largest sample size from the above calculations,then based
on the baseline value of 40000 page views per day we will need,
4741212/40000 = 119 days to complete the experiment

If we drop retention, then we can complete the experiment in
685325/40000 = 18 days.

**Given that there might be other experiments going on simultaneously,
we are likely to use a 50% diversion rat and run the experiment for 36
days.**

5.**Experiment Analysis**

``` r
control <- read.csv("Data/part2_control_results.csv")
treat <- read.csv("Data/part2_exp_results.csv")
```

``` r
control_val <- control %>%  summarize(cookies = sum(Pageviews, na.rm = T), clicks = sum(Clicks, na.rm = T), enrolls = sum(Enrollments, na.rm = T), payments = sum(Payments, na.rm = T))


treat_val <- treat %>%  summarize(cookies = sum(Pageviews, na.rm = T), clicks = sum(Clicks, na.rm = T), enrolls = sum(Enrollments, na.rm = T), payments = sum(Payments, na.rm = T))

groups <- as.data.frame(c('control', 'treatment'))

colnames(groups) <- c("Group")

overall_metrics <- rbind(control_val, treat_val)

overall_metrics <- cbind(groups, overall_metrics)

overall_metrics_original <- overall_metrics

overall_metrics<-  overall_metrics %>% pivot_longer(!Group, names_to = "Parameter", values_to = "metrics")

overall_metrics <-overall_metrics  %>% pivot_wider(names_from = "Group", values_from = "metrics")

overall_metrics <- as.data.frame(overall_metrics)

overall_metrics 
```

    ##   Parameter control treatment
    ## 1   cookies  345543    344660
    ## 2    clicks   28378     28325
    ## 3   enrolls    3785      3423
    ## 4  payments    2033      1945

5.1 **Sanity checks**

Comparing if the invariant metrics are equally distributed within both
treatment groups

Metrics tested here are: cookies, clicks\[count metrics\]; click through
probability\[other metrics\]

5.1.1 **Count Metrics**

``` r
#checking if count metrics are distributed equally among both groups

#cookies

cookie_test <- prop.test(x = c (345543, 344660), n = c(690203, 690203), alternative = "two.sided")


# clicks

click_test <- prop.test(x = c(28378, 28325), n = c(56703, 56703), alternative = "two.sided" )


#CI(-0.004903033,  0.006772423)

#enrollments

enrollments_test <- prop.test(x = c(3785, 3423), n = c(7208, 7208), alternative = "two.sided")

#payments

payments_test <- prop.test(x = c(2033, 1945), n = c(3978, 3978), alternative = "two.sided")


#adding 95% CI, observed difference and p-values to the table

#p_values

p_values <- c(cookie_test$p.value ,click_test$p.value, enrollments_test$p.value, payments_test$p.value)

#conf_intervals

#lower
cookie_lower <- as.list(cookie_test$conf.int)[1][1]
click_lower <- as.list(click_test$conf.int)[1][1]
enroll_lower <- as.list(enrollments_test$conf.int)[1][1]
payment_lower <- as.list(payments_test$conf.int)[1][1]

conf_int_lower <- c (cookie_lower, click_lower, enroll_lower, payment_lower)

#upper
cookie_upper <- as.list(cookie_test$conf.int)[2][1]
click_upper <- as.list(click_test$conf.int)[2][1]
enroll_upper <- as.list(enrollments_test$conf.int)[2][1]
payment_upper <- as.list(payments_test$conf.int)[2][1]

conf_int_upper <- c (cookie_upper, click_upper, enroll_upper, payment_upper)


#difference between proportions

cookie_diff <- cookie_test$estimate["prop 1"] - cookie_test$estimate["prop 2"]

click_diff <- click_test$estimate["prop 1"] - click_test$estimate["prop 2"]

enroll_diff <- enrollments_test$estimate["prop 1"] - enrollments_test$estimate["prop 2"]

payments_diff <- payments_test$estimate["prop 1"] - payments_test$estimate["prop 2"]


prop_diff <- c(cookie_diff, click_diff, enroll_diff, payments_diff)

#adding it to the overall_metrics table 

overall_metrics <- overall_metrics %>% mutate(obs_diff= round(prop_diff,6), CI_lower = as.numeric(conf_int_lower), CI_upper = as.numeric(conf_int_upper), p_value = round( p_values, 6), pass_sanity = ifelse(obs_diff >= CI_lower & obs_diff <= CI_upper, "True", "False"))

overall_metrics 
```

    ##   Parameter control treatment obs_diff       CI_lower    CI_upper  p_value
    ## 1   cookies  345543    344660 0.001279 -0.00039030049 0.002948968 0.133253
    ## 2    clicks   28378     28325 0.000935 -0.00490303341 0.006772423 0.757453
    ## 3   enrolls    3785      3423 0.050222  0.03377987353 0.066664078 0.000000
    ## 4  payments    2033      1945 0.022122 -0.00009791034 0.044341249 0.051087
    ##   pass_sanity
    ## 1        True
    ## 2        True
    ## 3        True
    ## 4        True

5.1.2 **Other metrics**

``` r
#click through probability

#control

control_clicks <-  overall_metrics_original$clicks[overall_metrics_original$Group == "control"]  
control_cookies <-  overall_metrics_original$cookies[overall_metrics_original$Group == "control"]


click_thr_cnt <- control_clicks / control_cookies

#treatment 

treatment_clicks <-  overall_metrics_original$clicks[overall_metrics_original$Group == "treatment"]  
treatment_cookies <-  overall_metrics_original$cookies[overall_metrics_original$Group == "treatment"]

click_thr_trt <- treatment_clicks / treatment_cookies

#two sample proportion test
click_thr_test <- prop.test(x= c(control_clicks,treatment_clicks),
          n = c(345543,344660), alternative = "two.sided")

#or use the proportion in experiment group and see if its CI includes the control proportion

# prop.test(x= treatment_clicks, n = 344660, alternative = "two.sided")$conf.int

click_thr_tab <- data.frame("click_through_probability",as.numeric(click_thr_cnt), as.numeric(click_thr_trt), as.numeric(click_thr_trt -click_thr_cnt ), as.list(click_thr_test$conf.int)[1][1], as.list(click_thr_test$conf.int)[2][1],click_thr_test$p.value)

names(click_thr_tab) <- c("Parameter", "control", "treatment", "obs_diff", "CI_lower", "CI_upper", "p_value")


click_thr_tab <- click_thr_tab %>% mutate( pass_sanity = ifelse(obs_diff >= CI_lower & obs_diff <= CI_upper, "True", "False"))

click_thr_tab
```

    ##                   Parameter    control  treatment      obs_diff     CI_lower
    ## 1 click_through_probability 0.08212581 0.08218244 0.00005662709 -0.001355181
    ##      CI_upper   p_value pass_sanity
    ## 1 0.001241927 0.9352212        True

5.2 **AB Testing** (Testing for Evaluation Metrics across both groups)

**Calculating necessary metrics**

``` r
#dropping rows with NA

control_2 <- drop_na(control)

treat_2 <- drop_na(treat)

#recreating the table with count metrics

control_val_2 <- control_2 %>%  summarize(cookies = sum(Pageviews, na.rm = T), clicks = sum(Clicks, na.rm = T), enrolls = sum(Enrollments, na.rm = T), payments = sum(Payments, na.rm = T))


treat_val_2 <- treat_2 %>%  summarize(cookies = sum(Pageviews, na.rm = T), clicks = sum(Clicks, na.rm = T), enrolls = sum(Enrollments, na.rm = T), payments = sum(Payments, na.rm = T))

groups_2 <- as.data.frame(c('control', 'treatment'))

colnames(groups_2) <- c("Group")


overall_metrics_2 <- rbind(control_val_2, treat_val_2)

overall_metrics_2 <- cbind(groups_2, overall_metrics_2)


#assigning variable names

#treatment values

clicks_trt <- overall_metrics_2$clicks[overall_metrics_2$Group == "treatment"]

enrollment_trt <- overall_metrics_2$enrolls[overall_metrics_2$Group == "treatment"]

payments_trt <- overall_metrics_2$payments[overall_metrics_2$Group == "treatment"]

# control values

clicks_cnt <- overall_metrics_2$clicks[overall_metrics_2$Group == "control"]

enrollment_cnt <- overall_metrics_2$enrolls[overall_metrics_2$Group == "control"]

payments_cnt <- overall_metrics_2$payments[overall_metrics_2$Group == "control"]


#calculating the metrics

#treatment
gross_conversion_trt <- enrollment_trt / clicks_trt

net_conversion_trt <- payments_trt / clicks_trt

#control
gross_conversion_cnt <- enrollment_cnt / clicks_cnt

net_conversion_cnt <- payments_cnt / clicks_cnt


#overall sample metrics

gross_conversion_overall <- (enrollment_cnt + enrollment_trt) / (clicks_cnt + clicks_trt)

net_conversion_overall <- (payments_cnt + payments_trt) /(clicks_cnt + clicks_trt)
```

    ##       Group cookies clicks enrolls payments
    ## 1   control  212163  17293    3785     2033
    ## 2 treatment  211362  17260    3423     1945

    ## Gross conversion for control = 0.218874689180593

    ## Net conversion for control = 0.117562019314173

    ## Gross conversion for control = 0.198319814600232

    ## Net conversion for control = 0.112688296639629

**Testing for differnece between two groups using a two sided two
proportion test**

``` r
#gross conversion rates 

#note(direction: treatment - control)

gross_conv_test <- prop.test(x = c(enrollment_trt, enrollment_cnt), n = c(clicks_trt, clicks_cnt), alternative = "two.sided")



gross_conv_tab <- data.frame("gross conversion rates", 0.01, as.numeric(gross_conversion_cnt), as.numeric(gross_conversion_trt), as.numeric( gross_conversion_trt - gross_conversion_cnt), as.list(gross_conv_test$conf.int)[1][1], as.list(gross_conv_test$conf.int)[2][1],gross_conv_test$p.value)

names(gross_conv_tab) <- c("Parameter","D_min", "control", "treatment", "obs_diff", "CI_lower", "CI_upper", "p_value")

gross_conv_tab
```

    ##                Parameter D_min   control treatment    obs_diff    CI_lower
    ## 1 gross conversion rates  0.01 0.2188747 0.1983198 -0.02055487 -0.02917804
    ##     CI_upper        p_value
    ## 1 -0.0119317 0.000002750943

**Inference**

-   Since the confidence interval does not contain zero, the difference
    is statistically significant. Further, since the difference is
    negative, it can be inferred that the experiment has reduced the
    gross conversion rate.

-   Also, since the difference is greater than the minimum detectable
    effect of 0.01, it is considered to be practically significant as
    well.

``` r
#net conversion rates

#note(direction: treatment - control)
net_conv_test <- prop.test(x = c(payments_trt, payments_cnt), n = c(clicks_trt, clicks_cnt), alternative = "two.sided")

net_conv_tab <- data.frame("net conversion rates",0.0075,as.numeric(net_conversion_cnt), as.numeric(net_conversion_trt), as.numeric( net_conversion_trt - net_conversion_cnt ), as.list(net_conv_test$conf.int)[1][1], as.list(net_conv_test$conf.int)[2][1],net_conv_test$p.value)

names(net_conv_tab) <- c("Parameter","D_min", "control", "treatment", "obs_diff", "CI_lower", "CI_upper", "p_value")

net_conv_tab
```

    ##              Parameter  D_min  control treatment     obs_diff    CI_lower
    ## 1 net conversion rates 0.0075 0.117562 0.1126883 -0.004873723 -0.01166207
    ##      CI_upper   p_value
    ## 1 0.001914623 0.1608122

**Inference**

-   Since the confidence interval contains zero, the difference is not
    statistically significant.

-   Also, since the difference is lesser than the minimum detectable
    effect of 0.0075, it is not considered to be practically significant
    as well.

6.  **Recommendation and Conclusion**

-   Based on the analysis of the AB test, it can be concluded that the
    experiment resulted in a decrease in the gross conversion rate of
    users. In other words, the ratio of the number of users enrolling in
    the free trial to the number of users clicking on the free trial has
    reduced.

-   However, the experiment did not increase net enrollment in a
    statistically significant manner.

-   Therefore, it can be considered partly successful. The
    recommendation is to launch the experiment, while continuing to
    design additional experiments to achieve the goal of improved net
    enrollment.
