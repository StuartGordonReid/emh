#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector cAsLevels(NumericVector logrets) {
  NumericVector levels = logrets * 0;
  levels[0] = 1.0;
  for (int i = 1; i < levels.size(); i++)
    levels[i] = levels[i - 1] * exp(logrets[i]);
  return(levels);
}


// [[Rcpp::export]]
NumericVector cAsLogReturns(NumericVector levels, int order) {
  NumericVector logrets = levels * 0;
  for (int i = order; i < levels.size(); i++)
    logrets[i] = log(levels[i] / levels[i - order]);
  return(logrets);
}


// [[Rcpp::export]]
NumericVector cAsDifferences(NumericVector x, int order) {
  NumericVector diffs = x * 0;
  for (int i = order; i < x.size(); i++)
    diffs[i] = x[i] - x[i - order];
  return(diffs);
}


// [[Rcpp::export]]
NumericMatrix cAsLogReturnsMatrix(NumericMatrix levels, int order) {
  NumericMatrix logrets = levels * 0;
  for (int i = order; i < levels.rows(); i++)
    logrets(i, _) = log(levels(i, _) / levels(i - order, _));
  return(logrets);
}


// [[Rcpp::export]]
NumericVector cAsBinary(NumericVector rets) {
  NumericVector binrets(rets.size());
  for (int i = 0; i < rets.size(); i++) {
    if (rets[i] > 0) {
      binrets[i] = 1;
    } else {
      binrets[i] = 0;
    }
  }
  return(binrets);
}


// [[Rcpp::export]]
NumericVector cAsRollingResiduals(NumericVector rets, int w) {
  NumericVector residuals(rets.size() - w);
  double rollingSum = 0;
  for (int i = 0; i < rets.size(); i++) {
    if (i < w) {
      rollingSum += log(rets[i] + 1);
    } else {
      rollingSum += log(rets[i] + 1);
      rollingSum -= log(rets[i - w] + 1);
      residuals[i - w] = log(rets[i] + 1) - (rollingSum / w);
    }
  }
  return(exp(residuals) - 1);
}


// [[Rcpp::export]]
NumericVector cAsRollingTrend(NumericVector rets, int w) {
  NumericVector trend(rets.size() - w);
  double rollingSum = 0;
  for (int i = 0; i < rets.size(); i++) {
    if (i < w) {
      rollingSum += log(rets[i] + 1);
    } else {
      rollingSum += log(rets[i] + 1);
      rollingSum -= log(rets[i - w] + 1);
      trend[i - w] = rollingSum / w;
    }
  }
  return(exp(trend) - 1);
}


// [[Rcpp::export]]
std::list<NumericMatrix> cWindow(NumericMatrix data, int window) {
  std::list<NumericMatrix> windows;
  for (int i = (window - 1); i < data.rows(); i++)
    windows.push_back(data(Range(i - (window - 1), i), _));
  return(windows);
}


// [[Rcpp::export]]
NumericMatrix cMatrixSubRows(NumericMatrix matrix, int ix, int size) {
  return(matrix(Range((ix - size), (ix - 1)), _));
}


// [[Rcpp::export]]
NumericMatrix cMatrixSubCols(NumericMatrix matrix, int start, int end) {
  return(matrix(_, Range(start - 1, end - 1)));
}


// [[Rcpp::export]]
NumericVector cMomentum(NumericMatrix matrix) {
  return(matrix(matrix.rows() - 1, _) / matrix(0, _));
}


// [[Rcpp::export]]
std::vector<double> cSort(std::vector<double> data) {
  std::sort(data.begin(), data.end());
  return(data);
}


// [[Rcpp::export]]
double cLarge(std::vector<double> data, int k) {
  std::sort(data.begin(), data.end());
  return(data[data.size() - k]);
}


// [[Rcpp::export]]
double cSmall(std::vector<double> data, int k) {
  std::sort(data.begin(), data.end());
  return(data[k - 1]);
}

