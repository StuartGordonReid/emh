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
multiple frequencies / lags. The result is a data.frame containing frequencies, sample sizes, computed test statistics, 
p values, Z scores, and a boolean flag indicating whether or not the test indicates the data was non random.

All of this is done with one simple function call,

```R
emh::is_random(zoo.object)
```

## Installation

### Linux and Mac

You can install emh using the devtools package,

```R
library(devtools)
devtools::install_github(repo="stuartgordonreid/emh")
```

### Windows

As of v0.1.0 some Windows users are having trouble installing the package. This should be resolved by v0.2.0. In the meantim, 
if method one did not work for you then try cloning the respository to your local machine,

```
git clone https://github.com/StuartGordonReid/emh.git
```

And then build and installing the package either from [RStudio](https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages) or from the [R command line tool](http://kbroman.org/pkg_primer/pages/build.html). If you still run into issues please let me know.

## Examples

For examples of how to use the package once it is installed checkout the /examples directory. In there you will find a 
simple Jupyter notebook demonstrating how to use the emh package. For more information you can also checkout my website 
where I will be writing quite a bit about this package in the future: [Turing Finance](http://www.turingfinance.com/).

## Future

These are the plans for the future releases (contributions are welcome),

* More univariate randomness tests,
  * Runs Tests:
    * The Monobit Test (on residuals)
    * The Longest Runs Test (on residuals)
    * The Random Excursions Test (on residuals)
    * The Random Excursions Variant Test (on residuals)
  * Unit Root Tests:
    * The Zivot-Andrews test for Unit Roots
    * The Phillips-Perron test for Unit Roots
    * The Augmented Dickey Fuller Test for Unit Roots (ADF)
    * The Extended ADF Test for Unit Roots (GLS-ADF)
    * The Kwiatkowski–Phillips–Schmidt–Shin Test for Unit Roots(KPSS)
  * Serial Correlation Tests:
    * The Overlapping Patterns Test (on residuals)
    * The Non-Overlapping Patterns Test (on residuals)
  * Variance Ratio Tests:
    * The Multiple Variance Ratio Test
    * Rank and Sign based Multiple Variance Ratio Test
  * Complexity Tests:
    * The Matrix Rank Test (on residuals)
    * The Linear Complexity Test (on residuals)
    * The Approximate Entropy Test (on residuals)
    * Lossless Compression Based Tests
    * Lossy Compression Based Tests
* More stochastic processes (benchmarks),
  * The Noisy Sine Wave
  * The Heston Stochastic Volatility Model
  * The Ornstein-Uhlenbech Mean Reverting Model
  * The Cox-Ingersoll-Ross Mean Reverting Model
* Some multivariate randomness tests
* An ensemble of univariate tests
