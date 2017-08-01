source("config.R")
sales_orders = read_feather("feather/sales_orders.feather")
products = read_feather("feather/products.feather")
users = read_feather('feather/users.feather')
sales_train <- read_feather("feather/sales_train.feather")

sales_orders <- left_join(sales_orders, select(users, user_id, user_lastorder))
# https://github.com/hadley/multidplyr/issues/49
arrange_.party_df <- function (.data, ..., .dots = list()) 
{
  multidplyr:::shard_call(.data, quote(dplyr::arrange), ..., .dots = .dots, 
                          groups = .data$groups[-length(.data$groups)])
}
# Database ----------------------------------------------------------------
data0 <- sales_orders %>% partition(user_id, product_id)
cluster_library(data0, "dplyr")
data <- data0 %>% arrange(user_id, product_id, desc(order_number)) %>% 
  summarise(
    up_orders = n()
    ,up_first_order = min(order_number)
    ,up_last_order = max(order_number) %>% .[1]
    ,up_gap_last_buyorder = user_lastorder[1] - up_last_order
    ,up_streak_order = (order_number - lag(order_number)) %>% rle(.) %>% .$length %>% .[2]
    ,up_average_cart_position = mean(add_to_cart_order)
    #,up_median = median(add_to_cart_order) too slow....
    #,up_median_cart_position = cpp_med2(add_to_cart_order) #multidply + Rccpfunc
  )
data <- data %>% collect() %>%  
  inner_join(products, by = "product_id") %>%
  inner_join(users %>% select(-user_lastorder), by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- 
  data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(sales_train %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))
rm(sales_orders, products, users, sales_train)
write_feather(data, "feather/data.feather")
rm(data, data0)
