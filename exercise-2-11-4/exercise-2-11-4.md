



```r
key_values <- function(x, digits = 2) {
    round(c(mean = mean(x), var = var(x), sd = sd(x)), digits)
}
```





> Predictive distributions: let $y$ be the number of 6's in 1000 independent rolls of a particular real die, which may be unfair. Let $\theta$ be the probability that the die lands on '6.' Suppose your prior distribution for $\theta$ is as follows:
> $$
\begin{array}{lll}
Pr(\theta = 1/12) & = & 0.25,\\
Pr(\theta = 1/6) & = & 0.5,\\
Pr(\theta = 1/4) & = & 0.25.
\end{array}
$$
> 
> 1. Using the normal approximation for the conditional distributions, $p(y|\theta)$, sketch your approximate prior predictive distribution for $y$.
> 2. Give approximate 5%, 25%, 50%, 75%, 95% points for the distribution of $y$. 

# Set up prior
Let $p(\theta_i)$ be the prior probability of the $i$ th posited value of $\theta_i$, where $i = 1, ..., k$, and $k = 3$.



```r
priors <- data.frame(
    i = 1:3,
    p_theta_i = c(.25, .5, .25),
    theta_i = c(1/12, 1/6, 1/4),
    n = rep(1000, 3))

priors$mean <- priors$n * priors$theta_i
priors$var <- with(priors, n * theta_i * (1 - theta_i))
```




The distribution $p(y|\theta)$ is a mixture distribution. 
The mean and variance for weighted sums of normal distributions are [set out on Wikipedia](http://en.wikipedia.org/wiki/Mixture_distribution#Moments).

Specifically,

$$
\begin{array}{lll}
E(X) & = & \sum_{i=1}^k w_i \mu_i \\
\text{var}(X) & = & \sum_{i=1}^k w_i((\mu_i - \mu)^2 + \sigma^2_i)
\end{array}
$$



```r
combined <- c(mean = sum(priors$mean * priors$p_theta_i))
combined["var"] <- sum(priors$p_theta_i * ((priors$mean - combined["mean"])^2 + 
    priors$var))
combined["sd"] <- sqrt(combined["var"])
```





# Sketch the graph with percentiles


```r
percentiles <- c(0.05, 0.25, 0.5, 0.75, 0.95)
curve(dnorm(x, combined["mean"], combined["sd"]), combined["mean"] - 
    4 * combined["sd"], combined["mean"] + 4 * combined["sd"])
(quantiles <- qnorm(percentiles, combined["mean"], combined["sd"]))
```



```
## [1]  67.87 126.15 166.67 207.18 265.46
```



```r
abline(v = quantiles, lty = 2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


# Compare with simulation


```r
k <- 1e+05
i <- sample(1:3, k, replace = TRUE, prob = priors$p_theta_i)
y <- rbinom(k, priors$n[i], priors$theta_i[i])

c(simulated_mean = mean(y), exact_mean = combined["mean"], simulated_sd = sd(y), 
    exact_sd = combined["sd"])
```



```
##  simulated_mean exact_mean.mean    simulated_sd     exact_sd.sd 
##          166.57          166.67           60.10           60.06 
```



```r

hist(sample(y), 100)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 




# Examining arithmetic on random variables
## Sums of random variables

The $E(X + Y) = E(X) + E(Y)$



```r
k <- 1e+06
e_x <- 1
e_y <- 2
x <- rnorm(k, e_x, 3)
y <- rnorm(k, e_y, 1)
z <- x + y

round(c(e_x = mean(x), e_y = mean(y), e_x_plus_y = mean(z)), 2)
```



```
##        e_x        e_y e_x_plus_y 
##       1.00       2.00       3.01 
```




## Variance of a sum of two independent random variables
If $X$ and $Y$ are indendent then 

$$
\begin{array}{ll}
\text{var}(X + Y)      & = \text{var}(X) + \text{var}(Y) \\
\text{sd}(X + Y) & = \sqrt{\text{var}(X) + \text{var}(Y)}
\end{array}
$$



```r
k <- 1e+06
Table <- expand.grid(e_x = c(0, 1, 2), e_y = c(0, 1, 2), var_x = c(1, 
    2), var_y = c(1, 2))

results <- t(apply(Table, 1, function(X) key_values(rnorm(k, X["e_x"], 
    sqrt(X["var_x"])) + rnorm(k, X["e_y"], sqrt(X["var_y"])))))
Table <- cbind(Table, results)
Table$exact_var <- Table$var_x + Table$var_y

Table$diff <- Table$var - Table$exact_var
Table
```



```
##    e_x e_y var_x var_y mean  var   sd exact_var  diff
## 1    0   0     1     1    0 2.00 1.41         2  0.00
## 2    1   0     1     1    1 2.00 1.41         2  0.00
## 3    2   0     1     1    2 2.00 1.41         2  0.00
## 4    0   1     1     1    1 1.99 1.41         2 -0.01
## 5    1   1     1     1    2 2.00 1.41         2  0.00
## 6    2   1     1     1    3 2.00 1.41         2  0.00
## 7    0   2     1     1    2 2.01 1.42         2  0.01
## 8    1   2     1     1    3 2.00 1.41         2  0.00
## 9    2   2     1     1    4 2.00 1.41         2  0.00
## 10   0   0     2     1    0 3.00 1.73         3  0.00
## 11   1   0     2     1    1 3.00 1.73         3  0.00
## 12   2   0     2     1    2 3.00 1.73         3  0.00
## 13   0   1     2     1    1 3.00 1.73         3  0.00
## 14   1   1     2     1    2 3.00 1.73         3  0.00
## 15   2   1     2     1    3 3.01 1.73         3  0.01
## 16   0   2     2     1    2 3.00 1.73         3  0.00
## 17   1   2     2     1    3 3.00 1.73         3  0.00
## 18   2   2     2     1    4 3.00 1.73         3  0.00
## 19   0   0     1     2    0 3.01 1.73         3  0.01
## 20   1   0     1     2    1 3.01 1.73         3  0.01
## 21   2   0     1     2    2 3.00 1.73         3  0.00
## 22   0   1     1     2    1 3.00 1.73         3  0.00
## 23   1   1     1     2    2 2.99 1.73         3 -0.01
## 24   2   1     1     2    3 3.00 1.73         3  0.00
## 25   0   2     1     2    2 3.00 1.73         3  0.00
## 26   1   2     1     2    3 3.00 1.73         3  0.00
## 27   2   2     1     2    4 3.00 1.73         3  0.00
## 28   0   0     2     2    0 4.01 2.00         4  0.01
## 29   1   0     2     2    1 4.00 2.00         4  0.00
## 30   2   0     2     2    2 3.99 2.00         4 -0.01
## 31   0   1     2     2    1 4.01 2.00         4  0.01
## 32   1   1     2     2    2 4.01 2.00         4  0.01
## 33   2   1     2     2    3 4.00 2.00         4  0.00
## 34   0   2     2     2    2 3.99 2.00         4 -0.01
## 35   1   2     2     2    3 4.00 2.00         4  0.00
## 36   2   2     2     2    4 4.00 2.00         4  0.00
```





## Variance of a multiple of a variable

$$
\begin{array}{lll}
\text{sd}(bX) & = & \beta ~ \text{sd}(X) \\
\text{var}(bX) & = & (\beta ~ \text{sd}(X))^2 = \beta ^2 ~ \text{sd}(X)^2 \\
\end{array}
$$



```r
k <- 1e+05
Table <- expand.grid(beta = c(1, 2, 0.5), x_sd = c(1, 3), x_mean = c(0, 
    1))

results <- t(apply(Table, 1, function(X) key_values(X["beta"] * rnorm(k, 
    X["x_mean"], X["x_sd"]))))
Table <- cbind(Table, results)
Table$exact_var <- Table$beta^2 * Table$x_sd^2

Table$diff <- Table$var - Table$exact_var
Table
```



```
##    beta x_sd x_mean  mean   var   sd exact_var  diff
## 1   1.0    1      0  0.00  1.00 1.00      1.00  0.00
## 2   2.0    1      0  0.01  4.02 2.01      4.00  0.02
## 3   0.5    1      0  0.00  0.25 0.50      0.25  0.00
## 4   1.0    3      0  0.00  8.98 3.00      9.00 -0.02
## 5   2.0    3      0 -0.03 36.26 6.02     36.00  0.26
## 6   0.5    3      0  0.00  2.26 1.50      2.25  0.01
## 7   1.0    1      1  1.00  1.00 1.00      1.00  0.00
## 8   2.0    1      1  2.00  4.04 2.01      4.00  0.04
## 9   0.5    1      1  0.50  0.25 0.50      0.25  0.00
## 10  1.0    3      1  1.00  9.04 3.01      9.00  0.04
## 11  2.0    3      1  2.05 36.10 6.01     36.00  0.10
## 12  0.5    3      1  0.49  2.27 1.51      2.25  0.02
```




## Mean and variance of mixture distributions


```r
k <- 10000
Table <- expand.grid(w_x = c(0.1, 0.25, 0.5), mean_x = c(0, 1), mean_y = 0, 
    var_x = c(1, 4), var_y = c(1, 4))
Table$w_y <- 1 - Table$w_x
Table$sd_x <- sqrt(Table$var_x)
Table$sd_y <- sqrt(Table$var_y)


sample_mixture <- function(w_x, w_y, mean_x, mean_y, sd_x, sd_y, 
    k = 10000) {
    i <- sample(1:2, k, replace = TRUE, prob = c(w_x, w_y))
    y <- ifelse(i == 1, rnorm(k, mean_x, sd_x), rnorm(k, mean_y, sd_y))
    key_values(y)
}

results <- t(apply(Table, 1, function(X) sample_mixture(X["w_x"], 
    X["w_y"], X["mean_x"], X["mean_y"], X["sd_x"], X["sd_y"])))

Table <- cbind(Table, results)
Table$exact_mean <- Table$w_x * Table$mean_x + Table$w_y * Table$mean_y
Table$exact_var <- Table$w_x * ((Table$mean_x - Table$exact_mean)^2 + 
    Table$var_x) + Table$w_y * ((Table$mean_y - Table$exact_mean)^2 + Table$var_y)

Table$diff <- Table$var - Table$exact_var
Table
```



```
##     w_x mean_x mean_y var_x var_y  w_y sd_x sd_y  mean  var   sd
## 1  0.10      0      0     1     1 0.90    1    1  0.01 1.01 1.01
## 2  0.25      0      0     1     1 0.75    1    1  0.01 0.99 0.99
## 3  0.50      0      0     1     1 0.50    1    1 -0.01 0.99 0.99
## 4  0.10      1      0     1     1 0.90    1    1  0.11 1.08 1.04
## 5  0.25      1      0     1     1 0.75    1    1  0.26 1.21 1.10
## 6  0.50      1      0     1     1 0.50    1    1  0.51 1.24 1.11
## 7  0.10      0      0     4     1 0.90    2    1  0.00 1.36 1.16
## 8  0.25      0      0     4     1 0.75    2    1 -0.01 1.77 1.33
## 9  0.50      0      0     4     1 0.50    2    1  0.00 2.52 1.59
## 10 0.10      1      0     4     1 0.90    2    1  0.12 1.41 1.19
## 11 0.25      1      0     4     1 0.75    2    1  0.27 1.95 1.40
## 12 0.50      1      0     4     1 0.50    2    1  0.48 2.67 1.63
## 13 0.10      0      0     1     4 0.90    1    2 -0.02 3.75 1.94
## 14 0.25      0      0     1     4 0.75    1    2 -0.01 3.20 1.79
## 15 0.50      0      0     1     4 0.50    1    2  0.01 2.50 1.58
## 16 0.10      1      0     1     4 0.90    1    2  0.11 3.81 1.95
## 17 0.25      1      0     1     4 0.75    1    2  0.23 3.39 1.84
## 18 0.50      1      0     1     4 0.50    1    2  0.51 2.73 1.65
## 19 0.10      0      0     4     4 0.90    2    2 -0.01 4.07 2.02
## 20 0.25      0      0     4     4 0.75    2    2 -0.04 3.93 1.98
## 21 0.50      0      0     4     4 0.50    2    2 -0.01 4.09 2.02
## 22 0.10      1      0     4     4 0.90    2    2  0.09 4.03 2.01
## 23 0.25      1      0     4     4 0.75    2    2  0.24 4.14 2.04
## 24 0.50      1      0     4     4 0.50    2    2  0.52 4.24 2.06
##    exact_mean exact_var    diff
## 1        0.00     1.000  0.0100
## 2        0.00     1.000 -0.0100
## 3        0.00     1.000 -0.0100
## 4        0.10     1.090 -0.0100
## 5        0.25     1.188  0.0225
## 6        0.50     1.250 -0.0100
## 7        0.00     1.300  0.0600
## 8        0.00     1.750  0.0200
## 9        0.00     2.500  0.0200
## 10       0.10     1.390  0.0200
## 11       0.25     1.938  0.0125
## 12       0.50     2.750 -0.0800
## 13       0.00     3.700  0.0500
## 14       0.00     3.250 -0.0500
## 15       0.00     2.500  0.0000
## 16       0.10     3.790  0.0200
## 17       0.25     3.438 -0.0475
## 18       0.50     2.750 -0.0200
## 19       0.00     4.000  0.0700
## 20       0.00     4.000 -0.0700
## 21       0.00     4.000  0.0900
## 22       0.10     4.090 -0.0600
## 23       0.25     4.188 -0.0475
## 24       0.50     4.250 -0.0100
```




