library(tidyverse)
library(feather)
library(multidplyr)
TODAY = Sys.Date() %>% gsub("-","",.)
DATAPATH = "data/"

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
x = rnorm(100)
library(microbenchmark)
microbenchmark(
  median(x),
  cpp_med(x),
  mean(x),
  cpp_med2(x),
  times = 20000
)
