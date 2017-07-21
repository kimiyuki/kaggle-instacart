source("config.R")
gc()
ret = read_csv(paste0('test-results/subtrain', TODAY, ".csv"))
ret %>% select(reordered, pred_reordered) %>%
  mutate(pred_reordered = round(pred_reordered, 2)) %>% 
  group_by(pred_reordered) %>% summarise(u = mean(reordered), n = n()) %>% 
  ggplot(aes(pred_reordered, u)) + geom_bar(stat="identity")
  
ret %>% select(reordered, pred_reordered) %>%
  mutate(pred_reordered = round(pred_reordered, 2)) %>% 
  group_by(pred_reordered) %>% summarise(u = mean(reordered), n = n()) %>% 
  ggplot(aes(pred_reordered, n)) + geom_bar(stat="identity") + scale_y_log10()
