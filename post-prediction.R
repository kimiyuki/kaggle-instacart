source("config.R")
library(magrittr)
gc()
ret = read_csv(paste0('test-results/subtrain1', TODAY, ".csv"))


###get recall
ret0 = ret %>% select(order_id,user_id, product_id, reordered, pred_reordered, order_number) 

##F1 score
  
ret1 <- ret %>% group_by(order_id) %>% nest()

  do(
    items = paste(product_id, collapse=","),
    actual = product_id[reordered == 1],
    re.items = paste(product_id[reordered == 1], collapse=","),
    pd.items = paste(product_id[pred_reordered > 0.21], collapse=",")
    )

write_csv(a, paste0("test-results/f1", TODAY, ".csv"))


f1 <- function(pred, actual){
  precision = sum( !is.na(match(pred, actual)), na.rm=T)/length(pred)
  recall = sum( !is.na(match(actual,  pred)), na.rm=T)/length(actual)
  2 * (precision*recall)/ (precision+recall)
}

ret0 %>% mutate(
  f1score = f1(str_split(pd.items, ",")[[1]],
               str_split(re.items, ",")[[1]]
               )) %>% select(f1score)

# nest? 
ret1 <- ret %>% partition(order_id)
ret2 <- ret1 %>% do(rd = as.vector(.$product_id[.$reordered==1]))
ret3 <- ret2 %>% collect(.)
ret4 <- ret %>% nest(-order_id) %>% 
  mutate(
    recall = map(~ setdiff(product_id[pred_reordered>0.21], product_id[reordered==1])
  )


ret2 <- ret %>% group_by(order_id) %>% do(rd = as.vector(.$product_id[.$reordered==1]))



## tmp ------------------------------------------------
## how pred_ordered predict reordered?
ret %>% select(reordered, pred_reordered) %>%
  mutate(pred_reordered = round(pred_reordered, 2)) %>% 
  group_by(pred_reordered) %>% summarise(u = mean(reordered), n = n()) %>% 
  ggplot(aes(pred_reordered, u)) + geom_bar(stat="identity")
ggsave("images/subtrain-actual_predtion-mean_value.png") 

ret %>% select(reordered, pred_reordered) %>%
  mutate(pred_reordered = round(pred_reordered, 2)) %>% 
  group_by(pred_reordered) %>% summarise(u = mean(reordered), n = n()) %>% 
  ggplot(aes(pred_reordered, n)) + geom_bar(stat="identity") + scale_y_log10()
