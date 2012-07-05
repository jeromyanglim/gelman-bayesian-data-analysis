
# Chapter 2 - Single-parameter models
These notes explore various ideas discussed in Chapter 2 of Gelman et al *Bayesian Data Analysis*.

# Examining Binomial distribution
The following is just a refresher on the [binomial coefficient](http://en.wikipedia.org/wiki/Binomial_coefficient)

The following plots show the value for different values of $n$ and $k$.



```r
par(mfrow = c(3, 3))
plot_choose <- function(n, ...) {
    plot(0:n, choose(n, 0:n), xlab = "k", ylab = "n choose k", ...)
}

for (X in c(2, 5, 8, 10, 20, 30, 50, 100, 200)) {
    plot_choose(X, main = paste("n =", X))
}
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


# Placenta privia example.



```r
german_data <- c(rep("female", 437), rep("male", 980 - 437))
table(german_data)
```



```
## german_data
## female   male 
##    437    543 
```



```r
round(prop.table(table(german_data)), 3)
```



```
## german_data
## female   male 
##  0.446  0.554 
```





Examine beta distribution

The following plots $\text{Beta}(438, 544)$.



```r
Alpha <- 438
Beta <- 544
curve(dbeta(x, Alpha, Beta), xlab = expression(theta), ylab = expression(paste("p(", 
    theta, "|y)")))
expected_beta <- Alpha/(Alpha + Beta)
var_beta <- (Alpha * Beta)/((Alpha + Beta)^2 * (Alpha + Beta + 1))
sd_beta <- sqrt(var_beta)
abline(v = expected_beta)

low_2.5_beta <- qbeta(0.025, Alpha, Beta)
hi_2.5_beta <- qbeta(0.975, Alpha, Beta)
abline(v = c(low_2.5_beta, hi_2.5_beta), lty = 2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Now let's compare beta to normal approximation



```r
low_2.5_normal <- qnorm(0.025, expected_beta, sd_beta)
hi_2.5_normal <- qnorm(0.975, expected_beta, sd_beta)

normal_beta <- cbind(`2.5` = c(low_2.5_normal, low_2.5_beta), `97.5` = c(hi_2.5_normal, 
    hi_2.5_beta))
normal_beta <- rbind(normal_beta, apply(normal_beta, 2, diff))
row.names(normal_beta) <- c("normal", "beta", "diff")
round(normal_beta, 6)
```



```
##             2.5     97.5
## normal 0.414955 0.477102
## beta   0.415065 0.477200
## diff   0.000111 0.000097
```




* The normal approximation uses the mean and standard deviation of the Beta distribution in combination with the normal quantile function.
* This confirms what the authors state about the similarity between the exact betaq and the normal approximation of the 95% posterior interval.
* The values

Now let's simulate 1000 draws from the Beta distribution and calculate the 95% interval.



```r
set.seed(1234)
simulated_theta <- rbeta(1000, Alpha, Beta)
low_2.5_sim_beta <- sort(simulated_theta)[25]
hi_97.5_sim_beta <- sort(simulated_theta)[976]

rbind(normal_beta, sim_beta = c(low_2.5_sim_beta, hi_97.5_sim_beta))
```



```
##                2.5      97.5
## normal   0.4149546 4.771e-01
## beta     0.4150655 4.772e-01
## diff     0.0001109 9.732e-05
## sim_beta 0.4163722 4.785e-01
```



```r

median(simulated_theta)
```



```
## [1] 0.4453
```



```r

c(simulated = mean(simulated_theta), expected = expected_beta)
```



```
## simulated  expected 
##    0.4456    0.4460 
```



```r

c(simulated = sd(simulated_theta), sd = sd_beta)
```



```
## simulated        sd 
##   0.01558   0.01585 
```




* It might be interesting to compare results for smaller samples or for values of $\theta$ closer to zero or one.

# Exploring table 2.1


```r
what_alpha_beta <- function(a_over_a_plus_b, a_plus_b) {
    # determines alpha and beta for a beta function where alpha / (alpha +
    # beta) and alpha + beta are known
    
    Alpha <- a_over_a_plus_b/a_plus_b
    Beta <- a_plus_b - Alpha
    c(Alpha = Alpha, Beta = Beta)
}

table_2.1 <- cbind(one = c(0.5, rep(0.485, 6)), two = c(2, 2, 5, 
    10, 20, 100, 200))

table_2.1 <- cbind(table_2.1, t(apply(table_2.1, 1, function(X) what_alpha_beta(X[1], 
    X[2]))))

table_2.1
```



```
##        one two Alpha.one Beta.two
## [1,] 0.500   2  0.250000    1.750
## [2,] 0.485   2  0.242500    1.758
## [3,] 0.485   5  0.097000    4.903
## [4,] 0.485  10  0.048500    9.951
## [5,] 0.485  20  0.024250   19.976
## [6,] 0.485 100  0.004850   99.995
## [7,] 0.485 200  0.002425  199.998
```



```r
# I haven't completed this analysis
```




# Piecewise linear density
What are some examples of piecewise linear densities?

Here's one strategy for constructing a piecewise density using increments and decrements and a cumulative total. However, it's normalised for discrete data, which is a limitation. 



```r
Data <- data.frame(theta = seq(0, 1, 0.01), raw_p = cumsum(c(1, rep(0, 
    40), rep(1, 10), rep(-1, 10), rep(0, 40))))

Data$normalised_p <- Data$raw_p/sum(Data$raw_p)
plot(Data$theta, Data$normalised_p, type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


# Asthma death example


```r
Alpha <- 3
Beta <- 5

curve(dgamma(x, Alpha, Beta), from = 0, to = 4)
abline(v = Alpha/Beta)
abline(v = (Alpha - 1)/Beta)
text(2, 1, paste("E(X)=", Alpha/Beta))
text(2, 0.9, paste("var(X)=", Alpha/Beta^2))
text(2, 0.8, paste("mode(X)=", (Alpha - 1)/Beta))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-81.png) 

```r

par(mfrow = c(3, 3))
curve(gamma(x), from = 0.5, to = 4)
curve(gamma(x) * Beta^x, from = 0.5, to = 4)
curve(1/(gamma(x) * Beta^x), from = 0.5, to = 4)

curve(Beta^x, from = 0.5, 4)
curve(exp(-Beta/x), from = 0.5, to = 4)
curve(exp(-Beta/x), from = 0.5, to = 400)

# Form used in Gelman
curve(exp(-Beta * x), from = 0.5, to = 4)
curve(exp(-Beta * x), from = 0.5, to = 400)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-82.png) 


Note that there are different parameterisations of the Gamma distribution.

* Some people use $e^{-\theta/\beta}$ (i.e., a scale parameter) whereas Gelman et al uses $e^{-\beta\theta}$ (i.e., a rate parameter).
* This changes $E(X) = \alpha\beta$ to $E(X) = \frac{\alpha}{\beta}$.
* The `dgamma` function defaults to a rate parameter, but also has an optional `scale` parameter that can be used.




```r
# sample from posterior
x <- rgamma(1000, 6, 7)
hist(x, 20, freq = FALSE)
abline(v = mean(x))
lines(density(x))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


# Exponential distribution



```r
par(mfrow = c(2, 2))
Betas <- 1:4
for (Beta in Betas) {
    curve(dexp(x, Beta), 0, 10)
    text(4, 1, paste("E(X)=", Beta))
    text(4, 0.6, paste("var(X)=", Beta^2))
    text(4, 0.4, paste("mode(X)=", 0))
    abline(v = 1)
}
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


# Example 2.8


```r
curve(dgamma(x, 20, 430000), from = 0, to = 1e-04)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
set.seed(1234)
thetas <- rgamma(500, 20, 430000)
ys <- rpois(thetas, thetas * 10000)
mean(ys/10000)
```



```
## [1] 4.04e-05
```



```r
table(ys)
```



```
## ys
##   0   1   2   3 
## 332 140  22   6 
```



```r
prop.table(table(ys))
```



```
## ys
##     0     1     2     3 
## 0.664 0.280 0.044 0.012 
```



```r

thetas <- rgamma(500, 20, 430000)
ys <- rpois(thetas, thetas * 1e+06)
mean(ys/1e+06)
```



```
## [1] 4.741e-05
```



```r
table(ys)
```



```
## ys
## 17 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 
##  1  1  2  3  1  1  1  3  5  6  2  3 10  9 12 15 17  9  9 14 16 15 19 17 19 
## 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 
## 18 12 15  7 32 14  9 15 19  8 10  8  7  8 10  9  6 10  9  7  8  4  8  8  2 
## 69 70 71 72 73 74 76 78 79 80 85 90 91 94 
##  6  1  1  2  1  4  3  2  1  1  2  1  1  1 
```



```r
prop.table(table(ys))
```



```
## ys
##    17    20    21    22    23    24    25    26    27    28    29    30 
## 0.002 0.002 0.004 0.006 0.002 0.002 0.002 0.006 0.010 0.012 0.004 0.006 
##    31    32    33    34    35    36    37    38    39    40    41    42 
## 0.020 0.018 0.024 0.030 0.034 0.018 0.018 0.028 0.032 0.030 0.038 0.034 
##    43    44    45    46    47    48    49    50    51    52    53    54 
## 0.038 0.036 0.024 0.030 0.014 0.064 0.028 0.018 0.030 0.038 0.016 0.020 
##    55    56    57    58    59    60    61    62    63    64    65    66 
## 0.016 0.014 0.016 0.020 0.018 0.012 0.020 0.018 0.014 0.016 0.008 0.016 
##    67    68    69    70    71    72    73    74    76    78    79    80 
## 0.016 0.004 0.012 0.002 0.002 0.004 0.002 0.008 0.006 0.004 0.002 0.002 
##    85    90    91    94 
## 0.004 0.002 0.002 0.002 
```




# Inverse chi-square distribution


```r
par(mfrow = c(3, 3))
curve(exp(x), 0, 10)
curve(exp(-x), 0, 10)
curve(exp(1/x), 0, 0.01)
curve(exp(-1/x), 0, 10)
curve(-1/x, 0, 10)
curve(-1/(2 * x), 0, 10)
curve(exp(-1/(2 * x)), 0, 10)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


* $\exp(0)=1$
* $\lim_{x \to -\infty} \exp(x) = 0$
* $\lim_{x \to \infty} -1/x = 0$
* $\lim_{x \to \infty} \exp(-1/x) = 1$




```r
par(mfrow = c(3, 3))
curve(x^(-(1/2 + 1)), 0, 10)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 




```r
par(mfrow = c(3, 3))
plot_inv_chisquare_kernal <- function(Nu) {
    curve(x^(-Nu/2 - 1) * exp(-1/(2 * x)), 0, 5)
}

plot_inv_chisquare_kernal(3)
plot_inv_chisquare_kernal(2)
plot_inv_chisquare_kernal(1)
plot_inv_chisquare_kernal(1)

curve(exp(x^(10/2 - 1) * (-1/(10 * x))), 0, 10)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 




```r
integrate(function(x) 1/x^2, 1, Inf)
```



```
## 1 with absolute error < 1.1e-14
```



```r
integrate(function(x) 1/x^2, 1e-04, Inf)
```



```
## 10000 with absolute error < 0.024
```



```r
integrate(function(x) 1/x^2, 0, Inf)
```



```
## Error: the integral is probably divergent
```





