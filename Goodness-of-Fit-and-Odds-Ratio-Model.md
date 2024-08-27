Goodness of Fit and Odds Ratio Model
================
Ethan Bensadoun
2024-08-15

``` r
#load the data into R
survey_data = read.csv("/Users/ethanbensadoun/Desktop/GSS1991.csv", header=T)
```

## (a)

We can assume that the model we are fitting here is a logistic
regression model since the outcomes are binary: yes, no, and don’t know.
We can also say that since the model (BP,DP) relies on the the
association between B and D conditional on P, given that the answer to
the question about an African-American president (P) is Yes.

``` r
model0 <- glm(Count ~ Busing*President + Home*President, 
              data = survey_data, family = "poisson")
summary(model0)
```

    ## 
    ## Call:
    ## glm(formula = Count ~ Busing * President + Home * President, 
    ##     family = "poisson", data = survey_data)
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)              -2.0149     1.1972  -1.683  0.09238 . 
    ## BusingNo                  2.3026     1.0488   2.195  0.02813 * 
    ## BusingYes                 1.3863     1.1180   1.240  0.21500   
    ## PresidentNo             -19.7080  4263.2096  -0.005  0.99631   
    ## PresidentYes             -0.9612     1.5767  -0.610  0.54212   
    ## HomeNo                    1.8718     0.7596   2.464  0.01373 * 
    ## HomeYes                 -18.6816  4886.4023  -0.004  0.99695   
    ## BusingNo:PresidentNo      1.5476     1.4565   1.063  0.28799   
    ## BusingYes:PresidentNo     0.5596     1.5469   0.362  0.71752   
    ## BusingNo:PresidentYes     0.2408     1.0770   0.224  0.82311   
    ## BusingYes:PresidentYes    0.3868     1.1467   0.337  0.73590   
    ## PresidentNo:HomeNo       19.7356  4263.2093   0.005  0.99631   
    ## PresidentYes:HomeNo       3.6047     1.2574   2.867  0.00415 **
    ## PresidentNo:HomeYes      38.1889  6484.7421   0.006  0.99530   
    ## PresidentYes:HomeYes     23.4090  4886.4024   0.005  0.99618   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 1219.786  on 26  degrees of freedom
    ## Residual deviance:   25.214  on 12  degrees of freedom
    ## AIC: 116.32
    ## 
    ## Number of Fisher Scoring iterations: 17

``` r
# Next we need to create a flat contigency table
ftable(xtabs(Count~., data = survey_data))
```

    ##                          Home Don't know  No Yes
    ## X  President  Busing                            
    ## 1  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0  41
    ## 2  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0  65   0
    ## 3  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 4  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0  71
    ##               Yes                      0   0   0
    ## 5  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0 157   0
    ##               Yes                      0   0   0
    ## 6  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       1   0   0
    ##               Yes                      0   0   0
    ## 7  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   1
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 8  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0  17   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 9  Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 10 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   2
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 11 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   5   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 12 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 13 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   3
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 14 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0  44   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 15 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 16 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   1
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 17 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 18 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 19 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 20 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   3   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 21 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      1   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 22 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 23 Don't know Don't know               0   0   0
    ##               No                       0  10   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 24 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 25 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 26 Don't know Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ## 27 Don't know Don't know               1   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    No         Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0
    ##    Yes        Don't know               0   0   0
    ##               No                       0   0   0
    ##               Yes                      0   0   0

## (b)

We can use the Likelihood Ratio Test to measure goodness of fit.

``` r
# Here we anova as a likelihood ratio test statistic for goodness of fit.
LRT_test <- anova(model0, test = "LRT")
LRT_test
```

    ## Analysis of Deviance Table
    ## 
    ## Model: poisson, link: log
    ## 
    ## Response: Count
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                  Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                                26    1219.79              
    ## Busing            2   282.75        24     937.03 < 2.2e-16 ***
    ## President         2   477.12        22     459.91 < 2.2e-16 ***
    ## Home              2   393.05        20      66.86 < 2.2e-16 ***
    ## Busing:President  4    10.54        16      56.31   0.03221 *  
    ## President:Home    4    31.10        12      25.21 2.923e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The model demonstrates we fit in part (a) demonstrates a satisfactory
level of fit when examining the associations between attitudes toward
busing and presidential preferences, as well as presidential preferences
and home invitations.

While the p-value for the b:p relationship suggests a less robust level
of significance, both relationships are characterized by low deviances
and residual deviances, indicating that the model’s predictions are
relatively close to the observed data.

## (c)

``` r
confint.data <- survey_data
# Here are the confidence intervals
confint.data$President <- c(1,1,1,1,1,1,1,1,1, 
                            0,0,0,0,0,0,0,0,0,
                            .5,.5,.5,.5,.5,.5,.5,.5,.5)
confint.data$Busing <- c(1,1,1, 0,0,0, .5,.5,.5,
                    1,1,1, 0,0,0, .5,.5,.5,
                    1,1,1, 0,0,0, .5,.5,.5)
confint.data$Home <- c(rep(c(1,0,.5),9))
# Now we can form the new model based on the confidence intervals above:
confint.model <-glm(Count ~ Busing*President + Home*President, 
                    data = confint.data, family = "poisson")
confint(confint.model)
```

    ## Waiting for profiling to be done...

    ##                      2.5 %    97.5 %
    ## (Intercept)       2.558536  3.274059
    ## Busing           -4.561685 -2.366147
    ## President         1.082454  1.925167
    ## Home             -6.099353 -3.383338
    ## Busing:President  1.292430  3.628838
    ## President:Home    2.292064  5.129119

``` r
summary(confint.model)
```

    ## 
    ## Call:
    ## glm(formula = Count ~ Busing * President + Home * President, 
    ##     family = "poisson", data = confint.data)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        2.9322     0.1823  16.087  < 2e-16 ***
    ## Busing            -3.3990     0.5575  -6.097 1.08e-09 ***
    ## President          1.4918     0.2147   6.949 3.67e-12 ***
    ## Home              -4.6386     0.6886  -6.736 1.63e-11 ***
    ## Busing:President   2.4032     0.5935   4.049 5.15e-05 ***
    ## President:Home     3.6165     0.7194   5.027 4.99e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 1219.79  on 26  degrees of freedom
    ## Residual deviance:  584.44  on 21  degrees of freedom
    ## AIC: 657.55
    ## 
    ## Number of Fisher Scoring iterations: 7

Above we have the different confidence intervals for the counts,
including the 95% confidence intervals for the interaction terms. We can
see that all the estimates in the summary table fall in the confidence
intervals, we are 95% confident the parameters are true.

``` r
#If we want to compute the likelihood ratio test we also need a saturated version of the model fit
sat_model <- glm(Count ~ Busing + President, data = survey_data, family = "poisson")
LRT <- logLik(model0, sat_model)
```

    ## Warning in logLik.glm(model0, sat_model): extra arguments discarded

We want to include the maximum number of parameters, hence, we need a
saturated model. We can see that model0 is better at predicting the
counts than the saturated model based on the lRT ratio.

## (d)

We can evaluate various models and review their summary tables to
determine if they are suitable.

``` r
# Here we don't have interaction terms between the variables
model1 <- glm(Count ~ Busing + Home + President, 
              data = survey_data, family = "poisson")
# Here we have a interaction between B and P, H and B
model2 <- glm(Count ~ Busing*President + Home*Busing, 
              data = survey_data, family = "poisson")
# Here we have a interaction between B and H, H and P
model3 <- glm(Count ~ Busing*Home + Home*President, 
              data = survey_data, family = "poisson")
# Here we have a interaction between B and P, H and P
model4 <- glm(Count ~ Busing*President + Home*President, 
              data = survey_data, family = "poisson")
```
