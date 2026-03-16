###SENTIMENT ANALYSIS
setwd("~/Desktop/NYTimes")

#Packages 
install.packages(c(
  "tidyverse",
  "tidytext",
  "textdata",
  "stopwords",
  "wordcloud",
  "igraph",
  "ggraph",
  "quanteda",
  "quanteda.textplots"))

library(tidyverse)
library(tidytext)
library(textdata)
library(stopwords)
library(wordcloud)
library(igraph)
library(ggraph)
library(quanteda)
library(quanteda.textplots)

#upload dataset with words
tidy_nyt<- read_csv("token_nyt.csv")

#################
###1.BING LEXICON
#################
bing_sentiments <- get_sentiments("bing")

nyt_sentiment_bing <- tidy_nyt %>%
  inner_join(bing_sentiments, by = "word")

abstracts_scores <- nyt_sentiment_bing %>%
  count(story_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    sentiment_score = positive - negative,
    sentiment_words = positive + negative,
    sentiment_norm = ifelse(sentiment_words > 0,
                            (positive - negative) / sentiment_words,
                            0)
  )

final_sentiment_BING <- tidy_nyt %>%
  distinct(story_id, .keep_all = TRUE) %>%
  left_join(abstracts_scores, by = "story_id") %>%
  mutate(
    sentiment_score = ifelse(is.na(sentiment_score), 0, sentiment_score),
    sentiment_norm  = ifelse(is.na(sentiment_norm),  0, sentiment_norm),
    sentiment_words = ifelse(is.na(sentiment_words), 0, sentiment_words)
  )

#write.csv(final_sentiment_BING, "Final_sentiment_BING.csv", row.names = FALSE)


#GRAPHS
#1.Top 10 most frequent positive/negative words
nyt_sentiment_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Positive and Negative Words (BING)", x = "Word", y = "Word Frequency")

#2.Frequencies of negative/positive/neutral
final_sentiment_BING <- final_sentiment_BING %>%
  mutate(sentiment_category = case_when(
    sentiment_norm > 0 ~ "Positive",
    sentiment_norm < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

ggplot(final_sentiment_BING, aes(x = sentiment_category, fill = sentiment_category)) +
  geom_bar() +
  scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red3", "Neutral" = "gray")) +
  theme_minimal() +
  labs(title = "Sentiment Category Distribution (BING)", x = "Sentiment Category", y = "Number of Abstracts")

#3.Distribution of sentiment
#SCORE
ggplot(final_sentiment_BING, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 1, fill = "darkred", color = "black") +
  coord_cartesian(xlim = c(-10, 10)) +
  theme_minimal() +
  labs(title = "Distribution of Abstracts Sentiment Scores (zoom)",
       x = "Sentiment Score (Positive - Negative)",
       y = "Number of Abstracts")
#NORM
ggplot(final_sentiment_BING, aes(x = sentiment_norm)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Normalized Sentiment (Bing)",
       x = "Normalized Sentiment ((pos - neg) / (pos + neg))",
       y = "Number of Abstracts")




###########
####2.AFINN
###########
library(dplyr)
library(tidytext)
library(ggplot2)

afinn <- get_sentiments("afinn")

nyt_afinn <- tidy_nyt %>%
  inner_join(afinn, by = "word") %>%
  mutate(sentiment_score_word = value)

# Aggregation for abstract
final_sentiment_AFINN <- nyt_afinn %>%
  group_by(story_id) %>%
  summarise(
    sentiment_score = sum(sentiment_score_word, na.rm = TRUE),
    sentiment_words = n(),
    sentiment_norm  = ifelse(sentiment_words > 0,
                             sentiment_score / sentiment_words,
                             0),
    .groups = "drop"
  ) %>%
  left_join(
    tidy_nyt %>%
      select(story_id, date) %>%
      distinct(),
    by = "story_id"
  )

write.csv(final_sentiment_AFINN, "Final_sentiment_AFINN.csv", row.names = FALSE)

# Sentiment Category Distribution (AFINN Scores)
final_sentiment_AFINN <- final_sentiment_AFINN %>%
  mutate(
    rounded_score = round(sentiment_score),
    binned_score = case_when(
      rounded_score <= -5 ~ -5,
      rounded_score >= 5  ~ 5,
      TRUE ~ rounded_score
    )
  )

# graph
ggplot(final_sentiment_AFINN, aes(x = factor(binned_score), fill = factor(binned_score))) +
  geom_bar() +
  scale_fill_manual(values = c(
    "-5" = "#67000d", "-4" = "#a50f15", "-3" = "#de2d26", "-2" = "#fb6a4a", "-1" = "#fcae91",
    "0"  = "#D3D3D3", 
    "1"  = "#c7e9c0", "2"  = "#74c476", "3"  = "#31a354", "4"  = "#006d2c", "5"  = "#00441b"
  )) +
  theme_minimal() +
  labs(
    title = "Sentiment Category Distribution (AFINN Scores)",
    x = "Sentiment Score Intensity",
    y = "Number of Abstracts"
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


# 2. Top positive and negative words, for score levels
top_words_level <- nyt_afinn %>%
  group_by(value, word) %>%
  summarise(weighted_count = sum(abs(value)), .groups = "drop") %>%
  group_by(value) %>%
  slice_max(weighted_count, n = 10) %>%
  ungroup()

ggplot(top_words_level, aes(x = reorder(word, weighted_count), y = weighted_count)) +
  geom_col(fill = "#008080") +
  coord_flip() +
  facet_wrap(~ value, scales = "free_y") +
  theme_minimal() +
  labs(title = "Top Positive and Negative Words (AFINN)", x = "Word", y = "Word frequency")

# 3. distribution of normalized sentiment
ggplot(final_sentiment_AFINN, aes(x = sentiment_norm)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Normalized sentiment distribution (AFINN)", x = "Sentiment normalizzato", y = "Numero di abstract")



###################################################
#### SENTIMENT TRENDS, BING VS AFINN
##################################################
library(dplyr)
library(lubridate)
library(tidyr)

trend_bing_data <- final_sentiment_BING %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(month) %>%
  summarise(mean_sent = mean(sentiment_norm, na.rm = TRUE), .groups = "drop") %>%
  mutate(lexicon = "Bing")

trend_afinn_data <- final_sentiment_AFINN %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(month) %>%
  summarise(mean_sent = mean(sentiment_norm, na.rm = TRUE), .groups = "drop") %>%
  mutate(lexicon = "AFINN")

trend_comparativo <- bind_rows(trend_bing_data, trend_afinn_data)



#graph

presidents <- data.frame(
  president = c("Bush (W.)", "Obama", "Trump", "Biden", "Trump"),
  start = as.Date(c("2005-01-20", "2009-01-20", "2017-01-20", "2021-01-20", "2025-01-20")),
  end   = as.Date(c("2009-01-19", "2017-01-19", "2021-01-19", "2025-01-19", Sys.Date()))
)

pres_colors <- c("Bush (W.)" = "#F0A500", "Obama" = "#5DADE2", 
                 "Trump" = "#E74C3C", "Biden" = "#58D68D")

lexicon_colors <- c("Bing" = "#1B4F72", "AFINN" = "#117A65")

ggplot(trend_comparativo, aes(x = month, y = mean_sent, color = lexicon)) +
  geom_rect(data = presidents, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = president), 
            alpha = 0.15, inherit.aes = FALSE) +
           geom_line(alpha = 0.4, size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, size = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.3) +
  scale_fill_manual(values = pres_colors, name = "President") +
  scale_color_manual(values = lexicon_colors, name = "Lexicon") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Sentiment trend over time: Bing vs AFINN (2005-2025)",
    x = "Year",
    y = "Sentiment mean"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  )

