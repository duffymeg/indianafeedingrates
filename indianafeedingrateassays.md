Indiana Feeding Rate Assays
================
Meghan Duffy
October 27, 2016

Does feeding rate differ before and after epidemics?
----------------------------------------------------

Data collection for this project led by Katie Hunsberger (technician in Duffy Lab), using clones sent from Spencer Hall's lab in Indiana. Data analysis led by Meghan Duffy (<duffymeg@umich.edu>)

Right now, data sheet just has Island data in it. Will need to update this with rest of data, but starting with this as a trial.

Note about data: The fluorometer drifts over time. We quantify this by running controls periodically. We then need to factor out this drift to determine how the baseline RFUs for an ungrazed tube shifts over time.

Jenny Bryan's (elegant!) proposal for dealing with fluorometer drift issue
==========================================================================

Read data in.

``` r
df <- read.csv("indianafeedingrateassays_withcontrols.csv", na.strings = ".")
df$Block <- factor(df$Block)
head(df)
#>     Lake Block CloneSubline Rep     RFU length_um length_mm order
#> 1 Island     1      Control   1 7554.90        NA        NA     1
#> 2 Island     1           3B   1 5954.23   1290.44   1.29044     2
#> 3 Island     1           3B   2 6368.27   1258.19   1.25819     3
#> 4 Island     1           3B   3 6458.64   1266.39   1.26639     4
#> 5 Island     1           3B   4 6561.49   1253.18   1.25318     5
#> 6 Island     1           3B   5 7305.20   1234.70   1.23470     6
str(df)
#> 'data.frame':    265 obs. of  8 variables:
#>  $ Lake        : Factor w/ 1 level "Island": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Block       : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ CloneSubline: Factor w/ 55 levels "1-9A","10-37A",..: 53 22 22 22 22 22 1 1 1 1 ...
#>  $ Rep         : int  1 1 2 3 4 5 1 2 3 4 ...
#>  $ RFU         : num  7555 5954 6368 6459 6561 ...
#>  $ length_um   : num  NA 1290 1258 1266 1253 ...
#>  $ length_mm   : num  NA 1.29 1.26 1.27 1.25 ...
#>  $ order       : int  1 2 3 4 5 6 7 8 9 10 ...
```

For the controls only, plot RFU against order and color by Block. Add a fitted line for each Block.

``` r
df %>%
  filter(CloneSubline == "Control") %>% 
  ggplot(aes(x = order, y = RFU, color = Block)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 
```

![](indianafeedingrateassays_files/figure-markdown_github/plot%20controls-1.png)

Regress RFU on order within Block, for the controls. Eyeball-o-metrically check the intercept and slope against the plot.

``` r
control_fit <- lm(RFU ~ Block/order - 1, data = df,
                  subset = CloneSubline == "Control")
summary(control_fit)
#> 
#> Call:
#> lm(formula = RFU ~ Block/order - 1, data = df, subset = CloneSubline == 
#>     "Control")
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -259.97  -83.89   11.24   81.23  338.41 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> Block1       7532.003     98.293  76.628  < 2e-16 ***
#> Block2       7329.393    103.085  71.100  < 2e-16 ***
#> Block1:order   -9.239      1.226  -7.538 1.78e-06 ***
#> Block2:order  -10.721      1.400  -7.658 1.47e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 165.4 on 15 degrees of freedom
#> Multiple R-squared:  0.9995, Adjusted R-squared:  0.9994 
#> F-statistic:  8038 on 4 and 15 DF,  p-value: < 2.2e-16
```

Add a variable into `df` for the predicted RFU of a control tube; this tells us what RFUs we'd expect if there was no Daphnia in a tube (or if the Daphnia didn't eat anything)

``` r
df$predictedControlRFU <-
  predict(control_fit, newdata = df[c("order", "Block")])
head(df)
#>     Lake Block CloneSubline Rep     RFU length_um length_mm order
#> 1 Island     1      Control   1 7554.90        NA        NA     1
#> 2 Island     1           3B   1 5954.23   1290.44   1.29044     2
#> 3 Island     1           3B   2 6368.27   1258.19   1.25819     3
#> 4 Island     1           3B   3 6458.64   1266.39   1.26639     4
#> 5 Island     1           3B   4 6561.49   1253.18   1.25318     5
#> 6 Island     1           3B   5 7305.20   1234.70   1.23470     6
#>   predictedControlRFU
#> 1            7522.764
#> 2            7513.525
#> 3            7504.285
#> 4            7495.046
#> 5            7485.807
#> 6            7476.567
```

Now use that to calculate the clearance rate (which we also call feeding rate, because "clearance" has a different meaning to disease ecologists, and so can be a confusing term). Also need to know that the assays were run for 3 hours (0.125 days) and in 15 mL (0.015 L) water.

``` r
df <-
  mutate(df, CR = log(predictedControlRFU/RFU)*0.015*0.125)
head(df)
#>     Lake Block CloneSubline Rep     RFU length_um length_mm order
#> 1 Island     1      Control   1 7554.90        NA        NA     1
#> 2 Island     1           3B   1 5954.23   1290.44   1.29044     2
#> 3 Island     1           3B   2 6368.27   1258.19   1.25819     3
#> 4 Island     1           3B   3 6458.64   1266.39   1.26639     4
#> 5 Island     1           3B   4 6561.49   1253.18   1.25318     5
#> 6 Island     1           3B   5 7305.20   1234.70   1.23470     6
#>   predictedControlRFU            CR
#> 1            7522.764 -7.992626e-06
#> 2            7513.525  4.361302e-04
#> 3            7504.285  3.077745e-04
#> 4            7495.046  2.790440e-04
#> 5            7485.807  2.471082e-04
#> 6            7476.567  4.347626e-05
```

Now calculate the size-specific clearance rate

``` r
df <-
  mutate(df, SSCR = CR/(length_mm)^2)
head(df)
#>     Lake Block CloneSubline Rep     RFU length_um length_mm order
#> 1 Island     1      Control   1 7554.90        NA        NA     1
#> 2 Island     1           3B   1 5954.23   1290.44   1.29044     2
#> 3 Island     1           3B   2 6368.27   1258.19   1.25819     3
#> 4 Island     1           3B   3 6458.64   1266.39   1.26639     4
#> 5 Island     1           3B   4 6561.49   1253.18   1.25318     5
#> 6 Island     1           3B   5 7305.20   1234.70   1.23470     6
#>   predictedControlRFU            CR         SSCR
#> 1            7522.764 -7.992626e-06           NA
#> 2            7513.525  4.361302e-04 2.619031e-04
#> 3            7504.285  3.077745e-04 1.944196e-04
#> 4            7495.046  2.790440e-04 1.739954e-04
#> 5            7485.807  2.471082e-04 1.573477e-04
#> 6            7476.567  4.347626e-05 2.851867e-05
```

Check to make sure there don't seem to be major trends in the clearance rates over the course of the assay

``` r
df %>%
  ggplot(aes(x = order, y = CR, color = Block)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 
```

![](indianafeedingrateassays_files/figure-markdown_github/plot%20clearance%20rate-1.png)

Hrm, something looks not right with block 1. Will need to look into that more. First check: is something going wrong with the predictions? Testing this by subsetting the data so there's just one block.

``` r
testdf <- read.csv("indianafeedingrateassays_withcontrols.csv", na.strings = ".")
testdf$Block <- factor(testdf$Block)
testdf <- subset(testdf, Block == 1)
tail(testdf)
#>       Lake Block CloneSubline Rep     RFU length_um length_mm order
#> 136 Island     1          17A   5 5712.41   1082.95   1.08295   136
#> 137 Island     1       10-37B   1 6015.49   1152.83   1.15283   137
#> 138 Island     1       10-37B   2 6082.15   1146.98   1.14698   138
#> 139 Island     1       10-37B   3 6232.23   1198.24   1.19824   139
#> 140 Island     1       10-37B   4 6416.42   1232.65   1.23265   140
#> 141 Island     1       10-37B   5 5807.59   1156.63   1.15663   141
```

``` r
control_fit2 <- lm(RFU ~ order, data = testdf,
                  subset = CloneSubline == "Control")
summary(control_fit2)
#> 
#> Call:
#> lm(formula = RFU ~ order, data = testdf, subset = CloneSubline == 
#>     "Control")
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -150.02  -78.40   13.43   67.58  180.45 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 7532.0034    64.3879  116.98 3.19e-14 ***
#> order         -9.2394     0.8029  -11.51 2.95e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 108.3 on 8 degrees of freedom
#> Multiple R-squared:  0.943,  Adjusted R-squared:  0.9359 
#> F-statistic: 132.4 on 1 and 8 DF,  p-value: 2.949e-06
```

``` r
testdf$predictedControlRFU <-
  predict(control_fit2, newdata = testdf[c("order", "Block")])
head(testdf)
#>     Lake Block CloneSubline Rep     RFU length_um length_mm order
#> 1 Island     1      Control   1 7554.90        NA        NA     1
#> 2 Island     1           3B   1 5954.23   1290.44   1.29044     2
#> 3 Island     1           3B   2 6368.27   1258.19   1.25819     3
#> 4 Island     1           3B   3 6458.64   1266.39   1.26639     4
#> 5 Island     1           3B   4 6561.49   1253.18   1.25318     5
#> 6 Island     1           3B   5 7305.20   1234.70   1.23470     6
#>   predictedControlRFU
#> 1            7522.764
#> 2            7513.525
#> 3            7504.285
#> 4            7495.046
#> 5            7485.807
#> 6            7476.567
```

``` r
testdf <-
  mutate(testdf, CR = log(predictedControlRFU/RFU)*0.015*0.125)
head(testdf)
#>     Lake Block CloneSubline Rep     RFU length_um length_mm order
#> 1 Island     1      Control   1 7554.90        NA        NA     1
#> 2 Island     1           3B   1 5954.23   1290.44   1.29044     2
#> 3 Island     1           3B   2 6368.27   1258.19   1.25819     3
#> 4 Island     1           3B   3 6458.64   1266.39   1.26639     4
#> 5 Island     1           3B   4 6561.49   1253.18   1.25318     5
#> 6 Island     1           3B   5 7305.20   1234.70   1.23470     6
#>   predictedControlRFU            CR
#> 1            7522.764 -7.992626e-06
#> 2            7513.525  4.361302e-04
#> 3            7504.285  3.077745e-04
#> 4            7495.046  2.790440e-04
#> 5            7485.807  2.471082e-04
#> 6            7476.567  4.347626e-05
```

``` r
testdf %>%
  ggplot(aes(x = order, y = CR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 
```

![](indianafeedingrateassays_files/figure-markdown_github/plot%20clearance%20rate%20for%20just%20block%201-1.png)

Okay. It's not just a problem with calculating predicted RFUs. Will need to look into this more later.
