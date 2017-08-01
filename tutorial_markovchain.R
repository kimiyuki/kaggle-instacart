
order_list[2]

N_PRODUCTS <- 49688L
transitionCount <- function(L)tabulate(unlist(L), nbins=N_PRODUCTS)

order_list[, n_orders := max(order_number), user_id]
N <- order_list[, sum(n_orders-1L)]
N <- nrow(order_list) - length(unique(order_list$user_id))

N1 <- order_list[order_number>1, transitionCount(previous_order)]
N11 <- order_list[order_number>1, transitionCount(T11)]
N10 <- order_list[order_number>1, transitionCount(T10)]

N0 <- N - N1 
N01 <- order_list[order_number>1, transitionCount(T01)]
N00 <- N0 - N01

p <- data.table(
  product_id = 1:N_PRODUCTS,
  P0 = (N0+1)/(N+2),
  P00 = (N00+1)/(N0+2),
  P01 = (N01+1)/(N0+2)
 ,P1 = (N1+1)/(N+2) 
 ,P10 = (N10+1)/(N1+2)
 ,P11 = (N11+1)/(N1+2)
)
library(feather)
write_feather(p, "feather/product_transition.feather")
