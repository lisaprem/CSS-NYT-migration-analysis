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

tidy_nyt<- read_csv("token_nyt.csv")

#word count
count_tidy_nyt <- tidy_nyt%>% 
  count(word, sort = TRUE)
count_tidy_nyt

print(count_tidy_nyt,n=50)

#WORD CLOUD
pal <- brewer.pal(8, "Dark2")

tidy_nyt %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 30, colors = pal))

#BIGRAM NETWORK
library(tidyverse)
library(tidytext)
library(textstem)

#preparation
context_specific_vec <- unique(c(
  # Editorials
  "new","york","times","nyt","news","report","page","briefing",
  "article","column","post","update","daily","editorial",
  
  # Reporting verbs
  "said","says","reported","stated","told","according","added",
  "wrote","asked","say","reporting","case","city",
  
  # Time
  "year","years","month","months","week","weeks","day","days",
  "today","yesterday","tomorrow","now","time","ago","last","next",
  "monday","tuesday","wednesday","thursday","friday","saturday","sunday",
  "january","february","march","april","may","june","july","august",
  "september","october","november","december",
  
  # Generic titles
  "mr","ms","mrs","dr","president","senator","governor",
  "one","two","three","first","second","third","j",
  "also","people","many","much","would","could","should","might","can",
  "like","even",
  "u.s","united","states","american",
  
  # Possessives/contractions
  "trump’s","biden’s","it’s","here’s","today’s","world’s","year’s","week’s",
  "state’s","city’s","nation’s","america’s","party’s","one’s","let’s",
  "trumps","bidens","its","heres","todays",
  
  # Adverbs/fillings
  "just","still","back","well","around","among","whether","since",
  "however","although","though","already","often","perhaps","really",
  "almost","always","sometimes","per","via","within","without",
  "far","long","high","low",
  
  # NYT common expressions
  "puzzle","puzzles","crossword","spelling","bee","wordle",
  "result","results","map","maps","chart","charts","graph",
  "newsletter","live","blog","coverage",
  "story","stories","articles","opinion","essay",
  "series","episode","podcast","transcript","video","clip",
  "photo","images","review","recaps",
  "watch","listen","read","sign","subscribe",
  
  # Generic verbs
  "called","call","calls",
  "become","became","becomes",
  "use","used","using","uses",
  "see","saw","seen","seeing",
  "look","looked",
  "set","sets","setting",
  "found","find","finds","finding",
  "appear","appeared","appears",
  "seem","seemed",
  "show","shows","showed","showing",
  "led","lead","leading","leads",
  "include","includes","including","included",
  "know","known","knows","knew",
  "want","wanted","wants",
  "need","needed","needs",
  "get","got","getting",
  "go","going","gone","went",
  "make","made","makes","making",
  "take","took","taken","taking",
  "come","came","coming",
  "give","given","gave",
  "help","helped","helping",
  
  # Numbers/quantities
  "four","five","six","seven","eight","nine","ten",
  "past","future",
  "million","billion","trillion","hundred","thousand",
  "several","part","parts","half","full","whole","top","bottom"
))

custom_stop_words <- tibble(word = context_specific_vec)

stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)


all_stop_words <- unique(c(
  context_specific_vec,           
  stop_words$word,                
  stopwords::stopwords("en")     
))

#bigrams pipeline
tidy_bigrams <- dataMIG0525 %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!str_detect(word1, "^[0-9]+$"), !str_detect(word2, "^[0-9]+$")) %>%
  filter(str_length(word1) > 1, str_length(word2) > 1) %>%
  mutate(word1 = lemmatize_words(word1),
         word2 = lemmatize_words(word2)) %>%
  filter(!word1 %in% all_stop_words) %>%
  filter(!word2 %in% all_stop_words) 

head(tidy_bigrams)

#most frequent bigrams
bigram_counts <- tidy_bigrams %>% 
  filter(!is.na(word1) & !is.na(word2)) %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

#graph
library(igraph)
library(ggraph)

bigram_graph <- bigram_counts %>%
  filter(n > 50) %>% 
  graph_from_data_frame()

set.seed(2026)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(.15, "inches")),
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "darkred", size = 5)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void() +
  labs(title = "Bigrams Network Graph",
       subtitle = "Frequencies > 50")
