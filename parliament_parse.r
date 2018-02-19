library(tidyverse)
library(tidytext)

df1 <- read_csv("pm_lee.csv")

colnames(df1) <- c("text","year")

year_words <- df1 %>%
  unnest_tokens(word, text) %>%
  count(year, word, sort = TRUE) %>%
  ungroup()

total_words <- year_words %>% 
  group_by(year) %>% 
  summarize(total = sum(n))

year_words <- left_join(year_words, total_words)

year_words <- year_words %>%
  bind_tf_idf(word, year, n)

year_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

year_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(year) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, ncol = 3, scales = "free") +
  coord_flip()

