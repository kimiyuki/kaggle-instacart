source("config.R")
aisles <- read_csv(file.path(DATAPATH, "aisles.csv"))
departments <- read_csv(file.path(DATAPATH, "departments.csv"))
sales_prior <- read_csv(file.path(DATAPATH, "order_products__prior.csv"))
sales_train <- read_csv(file.path(DATAPATH, "order_products__train.csv"))
orders <- read_csv(file.path(DATAPATH, "orders.csv"))
products <- read_csv(file.path(DATAPATH, "products.csv"))

aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
orders$order_dow = as.factor(orders$order_dow)

# train data ----------------
sales_train %>% 
  left_join(orders %>% select(order_id, user_id)) %>% 
  write_feather("feather/sales_train.feather")

# Products -----------------------------------------------------------
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id, -product_name)
rm(aisles, departments)
gc()
write_feather(products, "feather/products.feather")

## Sales with order info
sales_orders <- orders %>% inner_join(sales_prior, by = "order_id")
rm(sales_prior,orderp)
gc()
write_feather(sales_orders, "feather/sales_orders.feather")

# Products ----------------------------------------------------------------
prd <- sales_order %>%
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

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)
products <- prd %>% left_join(products)
rm(products)
gc()
write_feather(products, "feather/products.feather")

# Users -------------------------------------------------------------------
users_order <- orders %>% 
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

users_dow <- orders %>% group_by(user_id, order_dow) %>% count() %>%
  group_by(user_id) %>% arrange(user_id, desc(n)) %>% filter(row_number() == 1) %>% 
  select(-n) %>% rename(freq_order_dow = order_dow)

users_hod <- orders %>% 
  mutate(freq_order_hod = as.factor(as.numeric(order_hour_of_day) %/% 8)) %>% 
  group_by(user_id, freq_order_hod) %>% count() %>%
  group_by(user_id) %>% arrange(user_id, desc(n)) %>% filter(row_number() == 1) %>% 
  select(-n)

users_product <- sales_order %>%
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
  mutate(
         last_order_dow = order_dow,
         last_order_hod = as.factor(as.numeric(order_hour_of_day) %% 8),
         is_last_order_7ago = as.numeric(days_since_prior_order ==7),
         is_last_order_Today = as.numeric(days_since_prior_order ==0))
  
users <- users %>% inner_join(users_train_test)

rm(users_order, users_dow, users_hod, users_users_product, users_train_test)
gc()
write_feather(users, "feather/users.feather")
