setwd("~/Desktop/NYTimes")

#####  Regression models

##### 1.BASIC
library(tidyverse)
library(broom) 


df <- read.csv("Final_sentiment_AFINN.csv")
data_topics<- read_csv("data_topics.csv")

topic_names <- c(
  "Border_Crisis_Flows", 
  "Political_Legislative_Debate", 
  "Daily_Life_Integration", 
  "Law_Enforcement_Justice", 
  "Labor_Regularization")

data_topics$dominant_topic <- topic_names[apply(data_topics[, topic_names], 1, which.max)]


# Creating date and president variables
df_clean <- df %>%
  mutate(date_fixed = as.Date(date)) %>% 
  mutate(year = as.numeric(format(date_fixed, "%Y"))) %>%
  mutate(president = case_when(
    date_fixed < as.Date("2009-01-20") ~ "Bush",
    date_fixed < as.Date("2017-01-20") ~ "Obama",
    date_fixed < as.Date("2021-01-20") ~ "Trump",
    date_fixed < as.Date("2025-01-20") ~ "Biden",
    TRUE ~ "Trump II"
  )) %>%
  mutate(president = factor(president, levels = c("Bush", "Obama", "Trump", "Biden", "Trump II")))


#Regression model
abstract_data <- df_clean %>%
  group_by(story_id, date, year, president) %>% 
  summarise(
    sentiment_mean = mean(sentiment_norm, na.rm = TRUE),
    word_count = sum(sentiment_words, na.rm = TRUE),
    .groups = 'drop'
  )

model1 <- lm(sentiment_mean ~ year + president + word_count, 
            data = abstract_data)
summary(model1)

# 5. Test ANOVA, model 1
anova_risultati <- anova(model1)
print(anova_risultati)




##### 2.WITH TOPICS
abstract_data_topics <- abstract_data %>%
  inner_join(data_topics %>% select(story_id, dominant_topic), by = "story_id") %>%
  mutate(dominant_topic = as.factor(dominant_topic))

model_2 <- lm(sentiment_mean ~ year + president + word_count + dominant_topic, 
                   data = abstract_data_topics)

summary(model_2)

# 4. Test ANOVA, model 2
anova_topics <- anova(model_2)
print(anova_topics)



