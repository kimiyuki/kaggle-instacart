library(tidyverse)
library(data.table)
orders <- read_csv("input/orders.csv.zip") %>% select(order_id, user_id, order_number,eval_set)
orderp <- read_csv("input/order_products__prior.csv.zip") %>% select(-add_to_cart_order)
orders <- as.data.table(orders)
orderp <- as.data.table(orderp)
product_list <- orderp[, .(current_order = list(product_id)), by=order_id]
order_list <- merge(orders, product_list, by="order_id")

setorderv(order_list, c("user_id", "order_number"))
order_list[, previous_order := shift(list(current_order)), by=user_id]
order_list[1:5]

intersect <- function(x, y){ y[match(x, y, 0L)]}
setdiff <- function(x, y){ x[match(x, y, 0L) == 0L]}

order_list[order_number>1, T11 := mapply(intersect, previous_order, current_order)]
order_list[order_number>1, T01 := mapply(setdiff, current_order, previous_order)]
order_list[order_number>1, T10 := mapply(setdiff, previous_order, current_order)]
library(feather)
write_feather(order_list, "feather/order_list.feather")
