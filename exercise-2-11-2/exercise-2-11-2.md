# Exercise 2-11-2
To quote Problem 2.11.2 from Gelman et al (2004)
> Predictive distributions: consider two coins, $C_1$ and $C_2$, with the following characteristics: Pr(heads|$C_1$)= 0.6 and Pr(heads|$C_2$) = 0.4. Choose one of the coins at random and imagine spinning it repeatedly. Given that the first two spins from the chosen coin are tails, what is the expectation of the number of additional spins until a head shows up?




```r
library(xtable)
```





## Examine through simulation



```r
set.seed(1234)
k <- 10000
# Pick a random coin
coin <- sample(c(1,2), size=k, replace=TRUE)

# Generate a sequence of coins of length about 100 (enough for)
LENGTH <- 100

flip_sequence <- function(coin) {
    if (coin == 1) {
        y <- sample(c('T', 'H'), size=LENGTH, prob=c(.4, .6), replace=TRUE)
    }
    if (coin == 2) {
        y <- sample(c('T', 'H'), size=LENGTH, prob=c(.6, .4), replace=TRUE)
    }
    y
}

flips <- lapply(coin, function(X) flip_sequence(X))

## Extract subset with two tails
first_two_tails <- sapply(flips, function(X) X[1] == 'T' & X[2] == 'T')
flips_first_two_tails <- flips[first_two_tails]

# Determine number of flips before first head
first_head <- function(x, ignore_first_flips=2) {
    flips <-  x[-seq(ignore_first_flips)]
    min(which(flips=='H'))
}

trials_to_head <- sapply(flips_first_two_tails, function(X) first_head(X))

plot(prop.table(table(trials_to_head)))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

## take the subset with two heads
```







```r

print(xtable(prop.table(table(trials_to_head))), "html")
```

<!-- html table generated in R 2.15.1 by xtable 1.7-0 package -->
<!-- Tue Jul  3 20:53:27 2012 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> trials_to_head </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD align="right"> 0.46 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD align="right"> 0.24 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD align="right"> 0.08 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD align="right"> 0.00 </TD> </TR>
   </TABLE>



*Complete at a later point*.
