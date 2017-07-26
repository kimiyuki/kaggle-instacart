source("config.R")
data = read_feather("feather/data.feather")
# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
test <- as.data.frame(data[data$eval_set == "test",])
train$reordered[is.na(train$reordered)] <- 0
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
## 131,209 users. take 30,000, 30,000
users_ids = train$user_id %>% unique() %>% sample(60000) 
subtrain1 <- train %>% filter( user_id %in%  users_ids[1:30000])
subtrain2 <- train %>% filter( user_id %in%  users_ids[30001:60000])
require(Matrix)

## make model
sp_mdl_mtrx = function (df){
  sparse.model.matrix(
    ~.,  
    data = df %>% select(-reordered, -eval_set, -user_id, -order_id, -product_id) # %>% select_if(...) 
  )}
X <- sp_mdl_mtrx(subtrain1) 
model <- xgboost(data = X, label= subtrain1$reordered, params = params, nrounds = 50)

## evaluation... need to cross validation more properly?
subtrain1$pred_reordered <- predict(model, sp_mdl_mtrx(subtrain1)) 
print("subtrain1 for cv")
print(LogLossBinary(subtrain1$reordered, subtrain1$pred_reordered))
write_csv(subtrain1, paste0("test-results/subtrain1", TODAY, ".csv"))

subtrain2$pred_reordered <- predict(model, sp_mdl_mtrx(subtrain2)) 
print("subtrain2 for cv")
print(LogLossBinary(subtrain2$reordered, subtrain2$pred_reordered))
write_csv(subtrain2, paste0("test-results/subtrain2", TODAY, ".csv"))

## need record in every? time

## evaluation
importance <- xgb.importance(colnames(X), model = model)
write_csv(importance, paste0("importances/importance-", TODAY, ".csv"))
#xgb.ggplot.importance(importance)
#ggsave(paste0('importances/plot', TODAY, ".png"))

#TODO to get score for train data.
rm(X, importance, ls(pattern="subtrain"))
xgb.save(model, paste0("xgbmodels/", TODAY, ".model"))
gc()

# tmp-------------
top15.imp <- importance[1:15,"Feature"] %>% pull()
X.top15 <- sparse.model.matrix(
   ~., data =  subtrain %>% select(top15.imp))
model.top15 <- xgboost(data = X.top15, label = subtrain$reordered,
                       params = params, nrounds = 50)

# Apply model -------------------------------------------------------------
#X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
X <-  sparse.model.matrix(~., data = test %>% select(-order_id, -product_id))
test$reordered <- predict(model, X)
write_csv(test, paste0("test-results/test", TODAY, ".csv"))

## I may need to top number N, whose N is sum(reordered), then apply it to theshold 0.21
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

