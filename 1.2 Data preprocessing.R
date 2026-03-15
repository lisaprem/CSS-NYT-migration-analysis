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

###UPLOAD DATASET
nyt_data<- read_csv("MIG05-25clean.csv")
data_listed <- nyt_data %>% 
  mutate(story_id = row_number())

###PREPROCESSING
 #Token
 tidy_nyt<- data_listed %>%
   unnest_tokens(word, abstract)
 
 #stopwords BASIC
stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)
 
 #context specific words
context_specific_stops <- unique(c(
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

context_specific_stops<- data.frame(word = context_specific_stops, stringsAsFactors = FALSE)
  
tidy_nyt <- tidy_nyt %>%
    anti_join(stop_english, by = "word") %>%       
    anti_join(context_specific_stops, by = "word") %>% 
    filter(!str_detect(word, "^[0-9]+$"))        
  
write.csv(tidy_nyt, "token_nyt.csv", row.names = FALSE)

