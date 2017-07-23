library(tidyverse)
library(feather)
TODAY = Sys.Date() %>% gsub("-","",.)
DATAPATH = "data/"

LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}