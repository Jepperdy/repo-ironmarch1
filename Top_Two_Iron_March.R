library(tidyverse)
library(ironmarch)
forum_threads <- im_forums_dfs$forums_topics
forum_posts <- im_forums_dfs$forums_posts

filtered_posts <- forum_posts %>%  
  filter(topic_id %in% c(3751, 34, 743, 416, 6287, 49, 310, 781, 7637, 52)) %>%  
  left_join(forum_threads %>% select(tid, title), by = c("topic_id" = "tid"))

filtered_posts <- filtered_posts %>%  
  mutate(post = str_replace_all(post, "<.*?>", "")) %>%  
  mutate(post = str_replace_all(post, "\\n", "")) 

top_threads <- filtered_posts %>%  
  select(title, post)

library(tidytext)
thread_words <- top_threads %>%  
  unnest_tokens(word, post) %>%  count(word, title, sort = TRUE)

total_words_z <- thread_words %>%
  group_by(title) %>%
  summarize(total = sum(n))

dataframez_total <- left_join(thread_words,total_words_z)

freq_by_rank_z <- dataframez_total %>% 
  group_by(title) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

post_tf_idf_z <- freq_by_rank_z %>%
  bind_tf_idf(word,title,n)

post_tf_idf_z %>%
  arrange(desc(tf_idf))

data("stop_words")
post_tf_idf_z <- post_tf_idf_z %>%
  anti_join(stop_words)


library(forcats)
post_tf_idf_z %>%  
  group_by(title) %>%  
  slice_max(tf_idf, n = 15) %>%  
  ungroup() %>%  
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +  
  geom_col(show.legend = FALSE) +  
  facet_wrap(~title, ncol = 2, scales = "free") +  
  labs(x = "tf-idf", y = NULL)