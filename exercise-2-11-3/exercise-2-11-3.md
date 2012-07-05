

> Predictive distributions: let $y$ be the number of 6's in 1000 rolls of a fair die.
>
> 1. Sketch the approximate distriution of $y$, based on the normal approximation.
> 2. Using the normal distribution table, give approximate 5%, 25%, 50%, 75%, and 95% points for the distribution of $y$.


# Calculate Mean and standard deviation


```r
n <- 1000
p <- 1/6

E_y <- n * p
var_y <- n * p * (1 - p)
sd_y <- sqrt(var_y)
```




# Sketch the normal approximation


```r
curve(dnorm(x, E_y, sd_y), E_y - 4 * sd_y, E_y + 4 * sd_y)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


# Give approximate percentiles based on normal approximation


```r
percentiles <- c(0.05, 0.25, 0.5, 0.75, 0.95)
curve(dnorm(x, E_y, sd_y), E_y - 4 * sd_y, E_y + 4 * sd_y)
quantiles <- qnorm(percentiles, E_y, sd_y)
abline(v = quantiles, lty = 2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


