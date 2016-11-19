# emh v0.1.0 (beta)

emh is an R package designed to make testing the efficient market hypothesis as easy as possible. 

## Description

emh provides a standardized interface to a number of randomness tests which have historically been used to measure 
the efficiency of markets. As of version 0.1.0 the randomness tests included in emh are,

* [The Runs Test (a.k.a The Wald–Wolfowitz Test)](https://en.wikipedia.org/wiki/Wald%E2%80%93Wolfowitz_runs_test),
* [The Durbin-Watson Test for Serial Correlation](https://en.wikipedia.org/wiki/Durbin%E2%80%93Watson_statistic),
* [The Ljung-Box Test for Serial Correlation](https://en.wikipedia.org/wiki/Ljung%E2%80%93Box_test),
* [The Breusch-Godfrey Test for Serial Correlation](https://en.wikipedia.org/wiki/Breusch%E2%80%93Godfrey_test),
* [The Bartell Rank-based Variance Ratio Test](https://www.rdocumentation.org/packages/randtests/versions/1.0/topics/bartels.rank.test), and
* [The Lo-MacKinlay Heteroscedastic Variance Ratio Test](http://www.turingfinance.com/stock-market-prices-do-not-follow-random-walks/)

Given a univariate zoo time series and a confidence interval, emh will execute each of these tests on the data at 
multiple frequencies / lags. The result is a data.frame containing sample sizes, computed test statistics, p values, 
Z scores, and a boolean flag indicating whether or not the test indicates the data was non random.

All of this is done with one simple function call,

```R
emh::is_random(zoo.object)
```

## Installation

To install emh please use the devtools package,

```R
library(devtools)
devtools::install_github(repo="stuartgordonreid/emh")
```

## Examples

For examples of how to use the package once it is installed checkout the /examples directory. In there you will find a 
simple Jupyter notebook demonstrating how to use the emh package. For more information you can also checkout my website 
where I will be writing quite a bit about this package in the future: [Turing Finance](www.turingfinance.com).
