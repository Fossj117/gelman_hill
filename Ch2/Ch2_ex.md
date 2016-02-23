# Chapter 2 Exercises
Jeff Fossett  
February 10, 2016  




### Q1

(1a) If $X$ is the original test scores, then let the rescaled version $X' = 1.5X + 47.5$. This will have mean $\mu' = 1.5\mu + 47.5 = 100$ and $\sigma' = 1.5\sigma = 10 \times 1.5 = 15$ as desired. 

(1b) I don't think that this is possible. 

### Q2

Read in the data: 


```r
setwd('/Users/jeff_fossett/repos/gelman_hill/Ch2')
girls <- read.csv('girls.csv')

# values 
p <- mean(girls$prop_girls)
n <- 3900
df <- 23

# observed sd 
obs <- sd(girls$prop_girls)

# expected sd 
exp <- sqrt(p*(1-p)/n)
```

The standard deviation observed here is 0.0064097. If the sexes of babies were independently decided with a constant probability over the 24-month period, we would expect the standard deviation to be given by: $$\sigma = \sqrt{\frac{p \dot (1-p)}{n}}$$ In this case, p is 0.485675 and so the expected standard deviation is 0.0080031. The difference between observed and expected is -0.0015934.

Is the difference significant? Actual variance should have a distribution with expected value equal to theoretical variance and proportional to a $\chi^2$ with 23 d.f. 

More specifically, we [know](http://www.milefoot.com/math/stat/ci-variances.htm) that if the original population of data is normally distributed, then the expression $$\frac{(n-1)s^2}{\sigma^2}$$ has a chi-square distribution with $n-1$ degrees of freedom. Here $n$ is number of samples and $s$ is the sample standard deviation. Flipping this around, it means we can get a confidence interval for the population variance like so: $$\frac{(n-1)s^2}{\chi^2_{\alpha/2}} \leq \sigma^2 \leq \frac{(n-1)s^2}{\chi^2_{1-\alpha/2}}$$. 


```r
# do some math 
df <- 23 
upper <- qchisq(0.025, 23)
lower <- qchisq(0.975, 23)

var_upper <- df*obs^2/upper
var_lower <- df*obs^2/lower

sd_upper <- sqrt(var_upper)
sd_lower <- sqrt(var_lower)
```

In this case, n = 24 and so $df = n -1 = 23$. Using R we can compute the critical values $\alpha = 0.05$ for a $\chi^2$ with $df=23$ as being 38.08 and 11.69. 


Evaluating $\frac{(n-1)s^2}{\chi^2_{\alpha/2}}$ and $\frac{(n-1)s^2}{\chi^2_{1-\alpha/2}}$ then gives 0.0000248 and 0.0000808 respectively. Consequently, our 95% confidence interval for the population variance is [0.005 , 0.009]. Since the observed standard deviation of 0.0064 is in this range, we can conclude that the observed difference is not signficant at $\alpha = 0.05$. 

### Q3

Let's demonstrate the central limit theorem. A Uniform(0,1) variable has expected value 0.5 and standard deviation 0.2886751


```r
# simulate some data! 
n_runs <- 1000
vals <- data.frame(out = replicate(n_runs, sum(runif(20,0,1))))

var_one <- (0-1)^2/12
var_sum <- 20*var_one

sd_sum <- sqrt(var_sum)

# plot it up
vals %>% ggplot(aes(x=out), family = "Courier") + 
  geom_density() + 
  stat_function( fun=dnorm, 
                args=list(mean=10, sd=sd_sum), 
                lwd = 2, 
                col = 'blue', 
                alpha = 0.2) + xlab("Value") + ylab("Density") + ggtitle("Demonstrating the CLT")
```

![](Ch2_ex_files/figure-html/unnamed-chunk-3-1.png) 

Pretty close! Probably even closer if we up the number of samples: 


```r
# simulate some data! 
n_runs <- 10000
vals <- data.frame(out = replicate(n_runs, sum(runif(20,0,1))))

var_one <- (0-1)^2/12
var_sum <- 20*var_one

sd_sum <- sqrt(var_sum)

# plot it up
vals %>% ggplot(aes(x=out), family = "Courier") + 
  geom_density() + 
  stat_function( fun=dnorm, 
                args=list(mean=10, sd=sd_sum), 
                lwd = 2, 
                col = 'blue', 
                alpha = 0.2) + xlab("Value") + ylab("Density") + ggtitle("Demonstrating the CLT")
```

![](Ch2_ex_files/figure-html/unnamed-chunk-4-1.png) 

