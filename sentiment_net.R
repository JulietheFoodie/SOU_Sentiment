sentiment_net <- function(SOU) {
  library(tidyverse)
  library(tidytext)
  SOU_df <- data_frame(text = SOU)
  tokens <- unnest_tokens(SOU_df, word, text)
  SOU_df2 <- anti_join(tokens, stop_words)
  words_before <- NROW(SOU_df2)
  SOU_df3 <- inner_join(SOU_df2, get_sentiments("afinn"))
  words_after <- NROW(SOU_df3)
  final_positive_count <- SOU_df3 %>%
    filter(score > 0) %>%
    summarise(total = n())
  final_positive_score <- SOU_df3 %>%
    filter(score > 0) %>%
    summarise(total = sum(score))
  final_negative_count <- SOU_df3 %>%
    filter(score < 0) %>%
    summarise(total = n())
  final_negative_score <- SOU_df3 %>%
    filter(score < 0) %>%
    summarise(total = sum(score))
  final_net_score <- final_positive_score$total + final_negative_score$total
  output = list(words_before,
                words_after,
                final_positive_count,
                final_positive_score,
                final_negative_count,
                final_negative_score,
                final_net_score)
  return(output)
  }

