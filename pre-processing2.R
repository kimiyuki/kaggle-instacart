source("config.R")
sales_orders = read_feather("feather/sales_orders.feather")
products = read_feather("feather/products.feather")
users = read_feather('feather/users.feather')
sales_train <- read_feather("feather/sales_train.feather")

# Database ----------------------------------------------------------------
data <- sales_orders %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

data <- data %>% 
  inner_join(products, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- 
  data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(sales_train %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))
rm(sales_order, products, users, sales_train)
gc()
write_feather(data, "feather/data.feather")