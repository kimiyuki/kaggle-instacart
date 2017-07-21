## forked from below
###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################
library(tidyverse)
library(data.table)
library(feather)
TODAY = Sys.Date() %>% gsub("-","",.)

# Load Data ---------------------------------------------------------------
path <- "data/"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
#orderp <- fred(file.path(path, "order_products__prior.csv"))
sales_prior <- read_feather('feather/orderp.feather')
sales_train <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))

# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
orders$order_dow = as.factor(orders$order_dow)
orders = as_data_frame(orders)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)
#write_feather(products, "feather/products.feather")
#sales_train$user_id <- orders$user_id[match(sales_train$order_id, orders$order_id)]
#sales_prior$user_id <- orders$user_id[match(sales_prior$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(sales_prior, by = "order_id")

rm(sales_prior,orderp)
gc()


# Products ----------------------------------------------------------------
prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>% 
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2)
  )

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>%
  select(-prod_reorders, -prod_first_orders, -prod_second_orders)
             

rm(products)
gc()

# Users -------------------------------------------------------------------
users_order <- orders %>% as_data_frame() %>% 
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

users_dow <- orders %>% group_by(user_id,order_dow) %>% count() %>%
  group_by(user_id) %>% arrange(user_id, desc(n)) %>% filter(row_number() == 1) %>% 
  select(-n)

users_hod <- orders %>% mutate(order_hod = as.factor(order_hour_of_day %/% 8)) %>% 
  group_by(user_id, order_hod) %>% count() %>%
  group_by(user_id) %>% arrange(user_id, desc(n)) %>% filter(row_number() == 1) %>% 
  select(-n)

users_product <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

users <- users_order %>% 
  left_join(users_dow) %>% 
  left_join(users_hod) %>% 
  left_join(users_product)

users$user_average_basket <- users$user_total_products / users$user_orders

users_train_test <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(users_train_test)

rm(users_order, users_dow, users_hod, users_users_product, users_train_test)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

#rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(sales_train %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))
rm(ordert, prd, users)
gc()
write_feather(data, "feather/data.feather")

# Train / Test datasets ---------------------------------------------------
#data < read_feather("feather/data.feather")
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
model <- xgboost(data = X, label= subtrain$reordered, params = params, nrounds = 80)

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

