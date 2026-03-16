setwd("~/Desktop/NYTimes")

##SENTIMENT PER TOPIC
library(dplyr)
library(tidyr)

tidy_nyt<- read_csv("token_nyt.csv")
data_topics<- read_csv("data_topics.csv")

##CALCOLO DEL SENTIMENT

# dominant topic
topic_names <- c(
  "Border_Crisis_Flows", 
  "Political_Legislative_Debate", 
  "Daily_Life_Integration", 
  "Law_Enforcement_Justice", 
  "Labor_Regularization")
data_topics$dominant_topic <- topic_names[apply(data_topics[, topic_names], 1, which.max)]


#setiment for topic
afinn_lex <- get_sentiments("afinn")
nyt_afinn <- tidy_nyt %>%
  inner_join(afinn_lex, by = "word") %>%
  mutate(sentiment_score_word = value)

##TOP 5 POSITIVE/NNEGATIVE WORDS FOR TOPIC
top_pos <- nyt_afinn %>%
  inner_join(data_topics %>% select(story_id, dominant_topic), by = "story_id") %>%
  group_by(dominant_topic, word) %>%
  summarise(total_score = sum(sentiment_score_word, na.rm = TRUE), .groups = 'drop') %>%
  group_by(dominant_topic) %>%
  slice_max(total_score, n = 5)

top_neg <- nyt_afinn %>%
  inner_join(data_topics %>% select(story_id, dominant_topic), by = "story_id") %>%
  group_by(dominant_topic, word) %>%
  summarise(total_score = sum(sentiment_score_word, na.rm = TRUE), .groups = 'drop') %>%
  group_by(dominant_topic) %>%
  slice_min(total_score, n = 5)

top_words_topic <- bind_rows(top_pos, top_neg)

#graph
ggplot(top_words_topic, aes(x = total_score, y = reorder_within(word, total_score, dominant_topic), fill = total_score > 0)) +
  geom_col() +
  facet_wrap(~dominant_topic, scales = "free") +
  scale_y_reordered() +
  scale_fill_manual(values = c("TRUE" = "#228B22", "FALSE" = "#CD1221"), labels = c("Negative", "Positive"), name = "Sentiment") +
  labs(title = "Top Sentiment Words per Topic (AFINN)", x = "Cumulative Sentiment Score") +
  theme_minimal()



##FREQUENCIES OF SENTIMENT NORM FOR TOPIC
data_topics$dominant_topic <- topic_names[apply(data_topics[, topic_names], 1, which.max)]

sentiment_abstract_topic <- nyt_afinn %>%
  inner_join(data_topics %>% select(story_id, dominant_topic), by = "story_id") %>%
  group_by(story_id, dominant_topic) %>%
  summarise(
    sentiment_score = sum(sentiment_score_word, na.rm = TRUE),
    sentiment_words = n(),
    sentiment_norm  = ifelse(sentiment_words > 0, sentiment_score / sentiment_words, 0),
    .groups = 'drop'
  ) %>%
  mutate(sentiment_cat = case_when(
    sentiment_norm > 0  ~ "Positive",
    sentiment_norm < 0  ~ "Negative",
    TRUE                ~ "Neutral"
  ))

#graph
ggplot(sentiment_abstract_topic, aes(x = dominant_topic, fill = sentiment_cat)) +
  geom_bar(position = "dodge") + # "dodge" mette le barre affiancate, "stack" le sovrappone
  scale_fill_manual(values = c("Positive" = "#228B22", "Negative" = "#CD1221", "Neutral" = "grey70")) +
  labs(
    title = "Sentiment distribution per Topic",
    x = "Topic",
    y = "Abstracts",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



##time serie
metadata_date <- tidy_nyt %>%
  distinct(story_id, date) %>%
  mutate(date = as.Date(date)) 
library(lubridate)

trend_topic_data <- sentiment_abstract_topic %>%
  inner_join(metadata_date, by = "story_id") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month, dominant_topic) %>%
  summarise(
    mean_sent = mean(sentiment_norm, na.rm = TRUE), 
    .groups = "drop"
  )

#graph
presidents <- data.frame(
  president = c("Bush (W.)", "Obama", "Trump", "Biden", "Trump"),
  start = as.Date(c("2005-01-20", "2009-01-20", "2017-01-20", "2021-01-20", "2025-01-20")),
  end   = as.Date(c("2009-01-19", "2017-01-19", "2021-01-19", "2025-01-19", Sys.Date()))
)

pres_colors <- c("Bush (W.)" = "#F0A500", "Obama" = "#5DADE2", 
                 "Trump" = "#E74C3C", "Biden" = "#58D68D")


ggplot() +
  geom_rect(data = presidents, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = president), 
            alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = trend_topic_data, 
            aes(x = month, y = mean_sent, color = dominant_topic), 
            alpha = 0.3, size = 0.5) +
  geom_smooth(data = trend_topic_data, 
              aes(x = month, y = mean_sent, color = dominant_topic), 
              method = "loess", se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.5) +
  scale_fill_manual(values = pres_colors, name = "President") +
  scale_color_brewer(palette = "Set1", name = "Topic") + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
  labs(
    title = "Sentiment Trend per Topic (2005-2025)",
    x = "Year",
    y = "Sentiment Mean (AFINN)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  )