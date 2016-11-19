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

## Example 

Let's first download some data,

```R
library(emh)
stock_indices <- data_quandl_downloader(data_quandl_indices())
```

Now let's get the stock market index of our choosing, the SATRIX Financials Index,

```R
return_series <- indices$`GOOG/JSE_STXFIN`
```

Now let's run the suite of randomness tests on the returns of the SATRIX Financials Index computed at different lags (1 to 5) and
frequencies (Monday to Monday returns, ..., Month to Month returns),

```R
randomness_results <- is_random(return_series, a = 0.99, 
				freqs1 = c(1, 2, 3, 4, 5), 
				freqs2 = c("Mon", "Tue", "Wed", "Thu", "Fri", "Week", "Month"))
```

You can now view the results by typing,

```R
View(randomness_results)
```

And you can also plot the results and see what percentage of tests indicate that the SATRIX Financials Index is non random by the test
name and the frequency of the test,

```R
plot_results(randomness_results)
```

![alt text](http://www.turingfinance.com/wp-content/uploads/2016/11/non_random_frequency.png "Non random results by frequency")

![alt text](http://www.turingfinance.com/wp-content/uploads/2016/11/non_random_tests-1.png "Non random results by test")

It's as easy as that. 

## More Information

For more information see my website, [Turing Finance](www.turingfinance.com).
