# emh v0.1.0 (beta)

## Description

EMH is an R package designed to make testing the efficient market hypothesis (EMH) as easy as possible. 

EMH implements or interfaces to a number of randomness tests which have historically been used to measure the efficiency
of markets. As of version 0.1.0 the tests included in EMH include the independent runs tests, the Durbin-Watson test, 
the Breusch-Godfrey test, the Ljung-Box test, the Bartell Variance Ratio Rank test, and the Lo-MacKinlay Heteroscedastic
Consistent Variance Ratio Test. EMH runs each one of the tests on your data, a univariate zoo time series, at multiple 
frequencies in order to a produce a data.frame of results containing the test name, the frequency, the sample size at
that frequency, the computed test statistic, the two-sided p value, the Z-score, and a boolean value indicating whether
or not the test indicated that the return series was non random. All of this is done by one function, is_random.

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
