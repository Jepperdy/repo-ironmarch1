library(tidyverse)
library(tidytext)
library(ironmarch)
library(dplyr)
library(ggplot2)
library(usethis)
forum_topics <- im_forums_dfs$forums_topics
forums <- im_forums_dfs$forums_forums
posts <- im_forums_dfs$forums_posts
forums %>% 
  select(last_title, id) %>% 
  left_join(
    forum_topics %>% select(title, forum_id), 
    by = c("id" = "forum_id")) %>% 
  View

counted_titles <- forum_topics %>% 
  count(forum_id) %>% 
  left_join(forums %>% select(id, last_title), by = c("forum_id" = "id"))

data(stop_words)
forum_text <- tibble(line = 1:126, text = counted_titles$last_title)
forum_words <- forum_text %>% unnest_tokens(word,text)
forum_word_count <- forum_words %>%
  count(word,sort = TRUE)
forum_word_count <- forum_word_count %>%
  anti_join(stop_words)

thread_text <- tibble(line = 1:7168, text = forum_topics$title)
thread_words <- thread_text %>% unnest_tokens(word,text)
thread_word_count <- thread_words %>%
  count(word, sort = TRUE)
thread_word_count <- thread_word_count %>%
  anti_join(stop_words)

message_text <- tibble(line = 1:22309, text = messages$msg_post)
message_words <- message_text %>% unnest_tokens(word,text)
message_word_count <- message_words %>%
  count(word, sort = TRUE)
message_word_count <- message_word_count %>%
  anti_join(stop_words)

forum_threads_arranged <- arrange(forum_threads, desc(posts))

thread1 <- filter(posts, topic_id == 3751)
thread1_arranged <- arrange(thread1, desc(post_date))
thread2 <- filter(posts, topic_id == 34)
thread2_arranged <- arrange(thread2, desc(post_date))
thread3 <- filter(posts, topic_id == 743)
thread3_arranged <- arrange(thread3, desc(post_date))
thread4 <- filter(posts, topic_id == 416)
thread4_arranged <- arrange(thread4, desc(post_date))
thread5 <- filter(posts, topic_id == 6287)
thread5_arranged <- arrange(thread5, desc(post_date))
thread6 <- filter(posts, topic_id == 49)
thread6_arranged <- arrange(thread6, desc(post_date))
thread7 <- filter(posts, topic_id == 310)
thread7_arranged <- arrange(thread7, desc(post_date))
thread8 <- filter(posts, topic_id == 781)
thread8_arranged <- arrange(thread8, desc(post_date))
thread9 <- filter(posts, topic_id == 7637)
thread9_arranged <- arrange(thread9, desc(post_date))
thread10 <- filter(posts, topic_id == 52)
thread10_arranged <- arrange(thread10, desc(post_date))

topthread1_post_text <- tibble(line = 1:10703, text = thread1_arranged$post)
topthread1_post_words <- topthread1_post_text %>% unnest_tokens(word,text)
topthread1_post_word_count <- topthread1_post_words %>%
  count(line,word, sort = TRUE)
clean_topthread1 <- c("p","ipsquote","data","div","blockquote","forums","class","http","src","3751","contentapp","contentclass","contentid","contenttype","forums_topic","username","contentcommentid","timestamp","img","https","iframe","rel","href","external","nofollow","span","www.youtube.com","width","jpg","feature","embed","allowfullscreen","frameborder","ipsembeddedvideo","oembed","style","font","i.imgur.com","amp","v","gt","ipsquote_citation","ipsquote_contents","userid,emoticons","filestore.core_emoticons","media.tumblr.com")
dataframe_clean_topthread1 <- data.frame(clean_topthread1)
topthread1_post_word_count <- topthread1_post_word_count %>%
  anti_join(dataframe_clean_topthread1, by = c('word' = 'clean_topthread1'))

topthread2_post_text <- tibble(line = 1:8756, text = thread2_arranged$post)
topthread2_post_words <- topthread2_post_text %>% unnest_tokens(word,text)
topthread2_post_word_count <- topthread2_post_words %>%
  count(line,word, sort = TRUE)
clean_topthread2 <- c("p","ipsquote","data","div","blockquote","forums","class","http","src","3751","contentapp","contentclass","contentid","contenttype","forums_topic","username","contentcommentid","timestamp","img","https","iframe","rel","href","external","nofollow","span","www.youtube.com","width","jpg","feature","embed","allowfullscreen","frameborder","ipsembeddedvideo","oembed","style","font","i.imgur.com","amp","v","gt","ipsquote_citation","ipsquote_contents","userid,emoticons","filestore.core_emoticons","media.tumblr.com")
dataframe_clean_topthread2 <- data.frame(clean_topthread2)
topthread2_post_word_count <- topthread2_post_word_count %>%
  anti_join(dataframe_clean_topthread2, by = c('word' = 'clean_topthread2'))

total_words1 <- topthread1_post_word_count %>%
  group_by(line) %>%
  summarize(total = sum(n))

topthread1_post_word_count <- left_join(topthread1_post_word_count,total_words1)

total_words2 <- topthread2_post_word_count %>%
  group_by(line) %>%
  summarize(total = sum(n))

topthread2_post_word_count <- left_join(topthread2_post_word_count,total_words2)
topthread1_post_word_count_reduced <- filter(topthread1_post_word_count, line<=3000)

ggplot(topthread1_post_word_count_reduced, aes(n/total, fill = line)) +
  geom_histogram(show.legend = FALSE) +
  xlim(0, 0.5) +
  facet_wrap(~line, ncol = 2, scales = "free_y")

topthread2_post_word_count_reduced <- filter(topthread2_post_word_count, line<=6)

ggplot(topthread2_post_word_count_reduced, aes(n/total, fill = line)) +
  geom_histogram(show.legend = FALSE) +
  xlim(0, 0.5) +
  facet_wrap(~line, ncol = 2, scales = "free_y")

freq_by_rank1 <- topthread1_post_word_count %>%
  group_by(line) %>%
  mutate(rank = row_number(),
         'term frequency' = n/total) %>%
  ungroup()

freq_by_rank1 %>%
  ggplot(aes(rank,'term frequency', color = line)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10(limits = c(0.0001,100000)) +
  scale_y_log10(-1,-4)

edit_git_config()

