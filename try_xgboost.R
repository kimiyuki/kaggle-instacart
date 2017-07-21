source("config.R")
data = read_feather("feather/data.feather")
# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"         = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

subtrain <- train %>% sample_frac(0.1)
require(Matrix)
#X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
X <-  sparse.model.matrix(~., data = subtrain %>% select(-reordered))
model <- xgboost(data = X, label= subtrain$reordered, params = params, nrounds = 60)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)
ggsave(paste0('importances/plot', TODAY, ".png"))

#TODO to get score for train data.
rm(X, importance, subtrain)
gc()


# Apply model -------------------------------------------------------------
#X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
X1 <-  sparse.model.matrix(~., data = test %>% select(-order_id, -product_id))
test$reordered <- predict(model, X1)
write_csv(test, paste0("test-results/", TODAY, ".csv"))

test$reordered <- (test$reordered > 0.21) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write_csv(submission, path = paste0("submissions/", TODAY, ".csv"))

