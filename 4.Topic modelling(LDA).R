setwd("~/Desktop/NYTimes")

######TOPIC DETECTION#####
install.packages(c("topicmodels"),("ggcorrplot"))
library(topicmodels)
library(quanteda)
library(ggcorrplot)

#datasets needed
dataMIG0525 <- read_csv("MIG05-25clean.csv")
data_sentiment<- read_csv("Final_sentiment_AFINN.csv")

# PREPARAZIONE
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

custom_stop_words <- tibble(word = context_specific_stops )

stop_english <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)

root_stopwords <- c("immigr", "migr", "tri")

dataMIG0525 <- dataMIG0525 %>%
  mutate(story_id = row_number())

##Document-Term Matrix
corpus_nyt <- corpus(dataMIG0525, 
                      docid_field = "story_id",
                      text_field = "abstract") 

class(corpus_nyt)

#tokenization
tokens_nyt <- quanteda::tokens(corpus_nyt, 
                               remove_punct = TRUE, 
                               remove_numbers = TRUE, 
                               remove_symbols = TRUE) %>% 
  tokens_tolower() %>%
  tokens_remove(c(stopwords("english"), custom_stop_words$word)) %>%
 tokens_wordstem(language = "en")%>%
  tokens_remove(pattern = root_stopwords, valuetype = "glob")

# Document-Feature Matrix and trimming
DTM <- dfm(tokens_nyt)
DTM_trimmed <- dfm_trim(DTM, min_docfreq = 5)

##LDA
dtm_lda <- quanteda::convert(DTM_trimmed, to = "topicmodels")
set.seed(1234) 
lda_model <- LDA(dtm_lda, k = 5, method = "Gibbs", 
                 control = list(iter = 2000))
terms(lda_model, 10)

topics_probs <- as.data.frame(posterior(lda_model)$topics)
topics_probs$story_id <- as.numeric(rownames(dtm_lda)) 

data_topics <- data_sentiment %>%
  left_join(topics_probs, by = "story_id")

data_topics <- data_topics %>%
  rename_at(vars(as.character(1:5)), ~paste0("Topic", .))

topic_names <- c(
  "Border_Crisis_Flows", 
  "Political_Legislative_Debate", 
  "Daily_Life_Integration", 
  "Law_Enforcement_Justice", 
  "Labor_Regularization")

data_topics <- data_topics %>%
  rename(
    Border_Crisis_Flows = Topic1,
    Political_Legislative_Debate = Topic2,
    Daily_Life_Integration = Topic3,
    Law_Enforcement_Justice = Topic4,
    Labor_Regularization = Topic5
  )

data_topics$dominant_topic <- topic_names[apply(data_topics[, topic_names], 1, which.max)]

write.csv(data_topics,"data_topics.csv",row.names = FALSE)

#Correlation analysis between topic and sentiment 
cor_matrix <- data_topics %>%
  select(sentiment_norm, Border_Crisis_Flows, Political_Legislative_Debate,
         Daily_Life_Integration,  Law_Enforcement_Justice, Labor_Regularization ) %>%
  cor(use = "complete.obs")

# Heatmap
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           title = "Correlation between topics and sentiment",
           colors = c("#CD1221", "white", "#228B22"))

