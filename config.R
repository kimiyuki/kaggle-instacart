library(tidyverse)
library(feather)
library(multidplyr)
library(data.table)
TODAY = Sys.Date() %>% gsub("-","",.)
DATAPATH = "input/"
LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

#https://stackoverflow.com/questions/34771088/why-is-standard-r-median-function-so-much-slower-than-a-simple-c-alternative
#http://adv-r.had.co.nz/Rcpp.html
library(Rcpp)
cppFunction('double cpp_med2(Rcpp::NumericVector xx) {
    Rcpp::NumericVector x = Rcpp::clone(xx);
    std::size_t n = x.size() / 2;
    std::nth_element(x.begin(), x.begin() + n, x.end());
    
    if (x.size() % 2) return x[n]; 
    return (x[n] + *std::max_element(x.begin(), x.begin() + n)) / 2.;
}')
cppFunction('double cpp_med(Rcpp::NumericVector x){
  std::size_t size = x.size();
  std::sort(x.begin(), x.end());
  if (size  % 2 == 0) return (x[size / 2 - 1] + x[size / 2]) / 2.0;
  return x[size / 2];
}')
#https://www.kaggle.com/lukeeee/fast-r-port-of-faron-s-f1-maximization
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
library(inline)
cppFunction(
  'NumericMatrix get_expectations(NumericVector p, double p_none) {
    // Assuming p is sorted, p_none == 0 if not specified
    int n = p.size();
    NumericMatrix expectations = NumericMatrix(2, n + 1);
    double DP_C[n + 2][n + 1];
    std::fill(DP_C[0], DP_C[0] + (n + 2) * (n + 1), 0);
    if (p_none == 0.0) {
        p_none = std::accumulate(p.begin(), p.end(), 1.0, [](double &a, double &b) {return a * (1.0 - b);});
    }
    DP_C[0][0] = 1.0;
    for (int j = 1; j < n; ++j)
        DP_C[0][j] = (1.0 - p[j - 1]) * DP_C[0][j - 1];
    for (int i = 1; i < n + 1; ++i) {
        DP_C[i][i] = DP_C[i - 1][i - 1] * p[i - 1];
        for (int j = i + 1; j < n + 1; ++j)
            DP_C[i][j] = p[j - 1] * DP_C[i - 1][j - 1] + (1.0 - p[j - 1]) * DP_C[i][j - 1];
    }
    double DP_S[2 * n + 1];
    double DP_SNone[2 * n + 1];
    for (int i = 1; i < (2 * n + 1); ++i) {
        DP_S[i] = 1.0 / (1.0 * i);
        DP_SNone[i] = 1.0 / (1.0 * i + 1);
    }
    for (int k = n; k >= 0; --k) {
        double f1 = 0.0;
        double f1None = 0.0;
        for (int k1 = 0; k1 < (n + 1); ++k1) {
            f1 += 2 * k1 * DP_C[k1][k] * DP_S[k + k1];
            f1None += 2 * k1 * DP_C[k1][k] * DP_SNone[k + k1];
        }
        for (int i = 1; i < (2 * k - 1); ++i) {
            DP_S[i] = (1 - p[k - 1]) * DP_S[i] + p[k - 1] * DP_S[i + 1];
            DP_SNone[i] = (1 - p[k - 1]) * DP_SNone[i] + p[k - 1] * DP_SNone[i + 1];
        }
        expectations(0, k) = f1None + 2 * p_none / (2.0 + k);
        expectations(1, k) = f1;
    }
    return expectations;
}'
)

x = rnorm(100)
library(microbenchmark)
microbenchmark(
  median(x),
  cpp_med(x),
  mean(x),
  cpp_med2(x),
  times = 20000
)
  
