# Chapter 3
Jeff Fossett  
September 4, 2015  

## Notes

* Running a model with full interactions is equivalent to running the individual model separately within each subset of the data. 
* When to look for interactions? When inputs have large main effects, it's a good idea to look for interactions. 

Definitions: 

* Units: Answer to "What is the unit of analysis?" e.g. "persons" or "schools"
* Predictors: the $X$ variables in the regression
* Outcome: The outcome or $Y$ variable. 
* Inputs: Information on the units that goes into the $X$-variables. Inputs are not the same as predictors. 

Not so familiar with the notion of multivariate distribution. If $x$ is a $k$-dimensional random vector $x = [x_1, x_2, ..., x_k]$, then we can write that it has a multivariate normal distribution like: 

$$  x \sim \mathcal{N}(\mu, \Sigma) $$

Where $u$ is the vector of means $[u_1, u_2, ..., u_k]$ of the R.Vs and $\Sigma$ is the $k \times k$ covariance matrix: 

$$ \Sigma = [\mathrm{Cov}[X_i, X_j]], i= 1,2,...k; j = 1,2,..., k$$

So if we say that we can write the linear regression model in notation like so: 

$$ y \sim \mathcal{N}(X\beta, \sigma^2I) $$ 

That means $X\beta$ gives the vector of deterministic expected values. And then deviations from each of those follow a normal distribution with s.d. $\sigma^2$ (and they are indepenent since cov matrix has only diagonal). 

Least squares: the estimate $\hat{\beta}$ that minimizes sum of squared errors, $\sum_{i=1}^n(y_i - X_i\hat{\beta})^2$ for the data $X, y$.

We are trying to minimize the error in our prediction of $y$. 

Note that the OLS estimate is also the MLE estimate if the errors $\epsilon_i$ are independent with equal variance and normally distributed. 

Can express OLS estimator in matrix form like: $$\hat{\beta} = (X^tX)^{-1}X^ty$$

What matters: $\hat{\beta}$ is a linear function of $y$. 

# Exercises 

1. Read in the `pyth` data & fit the model: 


```r
library(tidyr)
```

```
## Warning: package 'tidyr' was built under R version 3.2.3
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

# read data 
p <- read.csv('pyth.tsv', sep=' ')
p %>% head(40) -> p_head

# What does it look like? 

# Hist
p_head %>% 
  select(x1, x2) %>% 
  gather(var, val) %>% 
  ggplot(aes(x=val)) + geom_histogram() + facet_grid(.~var)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](Ch3_files/figure-html/unnamed-chunk-1-1.png) 


```r
p_head %>% ggplot(aes(x=x1, y =y)) + geom_point() + ggtitle("Y vs. X1")
```

![](Ch3_files/figure-html/unnamed-chunk-2-1.png) 

X1 doesn't seem to have a clear linear association with y. 


```r
p_head %>% ggplot(aes(x=x2, y =y)) + geom_point() + ggtitle("Y vs. X2")
```

![](Ch3_files/figure-html/unnamed-chunk-3-1.png) 

There is a strong linear relationship between X2 and y. 

Here is X1 vs X2: 


```r
p_head %>% ggplot(aes(x=x1, y =x2)) + geom_point() + ggtitle("X1 vs. X2")
```

![](Ch3_files/figure-html/unnamed-chunk-4-1.png) 

Fit the model: 


```r
l <- lm(y ~ x1+x2, data=p_head)

s <- summary(l)

s
```

```
## 
## Call:
## lm(formula = y ~ x1 + x2, data = p_head)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.9585 -0.5865 -0.3356  0.3973  2.8548 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.31513    0.38769   3.392  0.00166 ** 
## x1           0.51481    0.04590  11.216 1.84e-13 ***
## x2           0.80692    0.02434  33.148  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9 on 37 degrees of freedom
## Multiple R-squared:  0.9724,	Adjusted R-squared:  0.9709 
## F-statistic: 652.4 on 2 and 37 DF,  p-value: < 2.2e-16
```

We see that both coefficients are significant and positive. For a unit increase in x1 we expect an increase of 0.5148104; for a unit increase in x2, we expect and increas of 0.8069195. The r-squared for the model is high: 0.9724241.

Here is a plot of the distribution of residuals: 


```r
p_head$resids <- l$residuals

p_head %>% ggplot(aes(x=resids, bindwidth = 0.5)) + geom_histogram() + ggtitle("Distribution of residuals")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](Ch3_files/figure-html/unnamed-chunk-6-1.png) 

Here is the residuals plotted against each of X1 and X2: 


```r
p_head %>% ggplot(aes(y=resids, x=x1)) + geom_point() + ggtitle("X1 vs. Residuals")
```

![](Ch3_files/figure-html/unnamed-chunk-7-1.png) 


```r
p_head %>% ggplot(aes(y=resids, x=x2)) + geom_point() + ggtitle("X2 vs. Residuals")
```

![](Ch3_files/figure-html/unnamed-chunk-8-1.png) 

Let's make predictions: 


```r
library(knitr)

predict(l, p %>% tail(20), interval='prediction') %>% kable()
```

            fit         lwr         upr
---  ----------  ----------  ----------
41    14.812484   12.916966   16.708002
42    19.142865   17.241520   21.044211
43     5.916816    3.958626    7.875006
44    10.530475    8.636141   12.424810
45    19.012485   17.118597   20.906373
46    13.398863   11.551815   15.245911
47     4.829144    2.918323    6.739965
48     9.145767    7.228364   11.063170
49     5.892489    3.979060    7.805918
50    12.338639   10.426349   14.250929
51    18.908561   17.021818   20.795303
52    16.064649   14.212209   17.917088
53     8.963122    7.084081   10.842163
54    14.972786   13.094194   16.851379
55     5.859744    3.959679    7.759808
56     7.374900    5.480922    9.268879
57     4.535267    2.616996    6.453539
58    15.133280   13.282467   16.984094
59     9.100899    7.223395   10.978403
60    16.084900   14.196990   17.972811
