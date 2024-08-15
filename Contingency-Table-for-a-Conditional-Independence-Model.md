Contingency Table for a Conditional Independence Model
================
Ethan Bensadoun
2024-08-15

## Assignment 4 - Question 1

``` r
# Here we can manually input the data from the contigency table in question 1
HighwaySafety_data <- tibble(
  Seatbelt = c("Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No"), 
  Ejected = c("Yes", "Yes","No", "No", "Yes", "Yes", "No", "No"),
  Injury = c("Nonfatal", "Fatal","Nonfatal","Fatal","Nonfatal", "Fatal","Nonfatal", "Fatal"),
  Count = c(1105, 14, 411111, 483, 4624, 497, 157342, 1008))
```

## a.

We can explore a conditional independence model figure out the
relationship between safety equipment use, ejection and injury:

``` r
model1 <- glm(Count ~ Seatbelt*Ejected + Ejected*Injury, data = HighwaySafety_data, family = "poisson")
summary(model1)
```

    ## 
    ## Call:
    ## glm(formula = Count ~ Seatbelt * Ejected + Ejected * Injury, 
    ##     family = "poisson", data = HighwaySafety_data)
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                6.026472   0.025986 231.916   <2e-16 ***
    ## SeatbeltYes                0.955230   0.002957 323.024   <2e-16 ***
    ## EjectedYes                 0.012267   0.051645   0.238    0.812    
    ## InjuryNonfatal             5.943472   0.025932 229.198   <2e-16 ***
    ## SeatbeltYes:EjectedYes    -2.476144   0.033131 -74.738   <2e-16 ***
    ## EjectedYes:InjuryNonfatal -3.526545   0.052952 -66.599   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 1624865.3  on 7  degrees of freedom
    ## Residual deviance:    1144.6  on 2  degrees of freedom
    ## AIC: 1233.6
    ## 
    ## Number of Fisher Scoring iterations: 5

The model 1 data describes that the iteraction between the
seatbelt:ejected estimate is -2.476144. Thus, this interaction tests
whether the effect of using a seat belt on the count of the response
variable is different for those who were ejected versus those who were
not. The log count of the outcome is expected to decrease by 2.476144.
e^-2.476144 = 0.084.describes the incident rate ratio , which means that
it is expected that there is an 8.4% probability of a person being
ejected given they wear a seatbelt during a crash.

Similary we can analyse the interaction term for ejected:injury, which
has an estimate of -3.526545. For e^-3.526545 = 0.029, thus, we can say
that there is a 2.9% probability that ejection occurs with a certain
severity of injury. Since this interaction term tests whether the effect
of being ejected on the count of the response variable is different for
different injury levels.
