#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int cBitsOne(NumericVector x) {
  int N = 0;
  for (int i = 0; i < x.size(); i++) {
    if (x[i] == 1) {
      N += 1;
    }
  }
  return(N);
}


// [[Rcpp::export]]
int cBitsZero(NumericVector x) {
  int N = 0;
  for (int i = 0; i < x.size(); i++) {
    if (x[i] == 0) {
      N += 1;
    }
  }
  return(N);
}


// [[Rcpp::export]]
int cRunsNumber(NumericVector x) {
  int N = 1;
  int carry = x[0];
  for (int i = 1; i < x.size(); i++) {
    if (x[i] != carry) {
      carry = x[i];
      N += 1;
    }
  }
  return(N);
}


// [[Rcpp::export]]
int cRunsLongest(NumericVector x) {
  int n = 0; int N = 0;
  int carry = x[0];
  for (int i = 1; i < x.size(); i++) {
    if (x[i] == carry) {
      n += 1;
      if (n > N) {
        N = n;
      }
    } else {
      n = 0;
      carry = x[i];
    }
  }
  return(N);
}

