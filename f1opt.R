# Input: ps - item reorder probabilities, prods - item ids
# Output: reordered items string (as required in submission)
exact_F1_max_none <- function(ps, prods) {
  prods <- as.character(prods)
  perm <- order(ps, decreasing = T)
  ps <- ps[perm]
  prods <- prods[perm]
  expectations <-  get_expectations(ps, 0.0)
  max_idx <-  which.max(expectations)
  add_none <- max_idx %% 2 == 1
  size <- as.integer(max(0, max_idx - 1) / 2)
  if (size == 0) {
    return("None")
  }
  else {
    if (add_none)
      return(paste(c(prods[1:size], "None"), collapse = " "))
    else 
      return(paste(prods[1:size], collapse = " "))
  }
}

# How to use it with dplyr:
#
# submission <- data %>%
#        group_by(order_id) %>%
#        summarise(products = exact_F1_max_none(reordered_prob, product_id))

# Quick example
exact_F1_max_none(c(0.5, 0.9, 0.8, 0.1, 0.2, 0.3), c(129832, 1024, 32, 432, 1421, 1032))
