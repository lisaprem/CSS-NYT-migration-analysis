setwd("~/Desktop/NYTimes")

#libraries
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

####################################
######## 1. DATA COLLECTION ########
####################################

#API key
#usethis::edit_r_environ()
api_key <- Sys.getenv("NYT_API_KEY")

# The NYT API returned incomplete results when querying 2005–2025 at once due to excessive data request,
# so the data was retrieved by splitting the requests by year and month, and repeating the code year by year.

########Extracting year by year#########
 
year <- 2025 #year to change from 2005 to 2025

###
month <- 1

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 2

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 3

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 4

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 5

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 6

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 7

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 8

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)



###
month <- 9

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 10

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 11

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###
month <- 12

# Download data
url <- paste0("https://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key)
res <- GET(url)
if (status_code(res) != 200) stop("Error downloading data")
data_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
articles <- data_json$response$docs
if (length(articles) == 0) stop("No articles found for this month")

# Process data and select columns
df <- articles %>%
  transmute(
    date = as.Date(substr(pub_date, 1, 10)),
    headline = headline$main,
    abstract = abstract,
    author = byline$original,
    news_desk=news_desk,
    section = section_name
  )

# Create dynamic dataset name
dataset_name <- paste0("nyt_", year, "_", sprintf("%02d", month))
assign(dataset_name, df)


###Binding months for year 202_
datasets <- paste0("nyt_", year, "_", sprintf("%02d", 1:12))
nyt_2025 <- do.call(bind_rows, mget(datasets)) #change year in the name before running code





###########EXTRACT FINAL############
nyt_all <- bind_rows(nyt_2005,nyt_2006,nyt_2007,nyt_2008,nyt_2009,nyt_2010,nyt_2011,
                     nyt_2012,nyt_2013,nyt_2014,nyt_2015,nyt_2016,nyt_2017,nyt_2018,
                     nyt_2019,nyt_2020,nyt_2021,nyt_2022,nyt_2023,nyt_2024,nyt_2025)
write.csv(nyt_all, "nyt_data_2005_25.csv", row.names = FALSE)



##########EXTRACT FINAL, FOR MIGRATION#####
library(dplyr)
library(stringr)

keywords <- c("migration", "immigration",
              "immigrant", "immigrants",
              "migrant", "migrants")


pattern <- paste(keywords, collapse = "|")

nyt_all_MIG <- nyt_all %>%
  filter(str_detect(tolower(abstract), pattern) | str_detect(tolower(headline), pattern))

#Check
head(nyt_all_MIG)

write.csv(nyt_all_MIG, "nyt_dataMIG_2005_25.csv", row.names = FALSE)



####################################
######## 2. DATA CLEANING #########
####################################

#More than one abstract in a raw, wrong filtering for 'migration':
#splitting concatenated abstracts and filtering again

library(tidyverse)

data <- read_csv("nyt_dataMIG_2005_25.csv")
keywords <- "immigra|migra"

data_clean <- data %>%
  # 1. Handle missing values and split concatenated abstracts
  filter(!is.na(abstract)) %>%
  mutate(multi_abstract = str_detect(abstract, "\\s{3,}")) %>%
  separate_rows(abstract, sep = "\\s{3,}") %>%
  
  # 2. Trim whitespace and remove metadata artifacts
  mutate(abstract = str_trim(abstract)) %>%
  filter(str_length(abstract) > 5) %>%
  filter(!str_detect(abstract, "^(PAGE|BUSINESS DAY|WEEK IN REVIEW).*")) %>%
  filter(!str_detect(abstract, "^(?:[A-Z\\s&]+,\\s*)?PAGE\\s+[A-Z0-9]+$")) %>%
  
  # 3. Filter out long transcripts
  # (exclude entries exceeding 200 words to prevent length-driven bias in sentiment analysis)
  mutate(n_words = str_count(abstract, "\\w+")) %>%
  filter(n_words < 200) %>%
  
  # 4. Apply keyword search
  filter(
    str_detect(abstract, regex(keywords, ignore_case = TRUE)) |
      (!multi_abstract & str_detect(headline, regex(keywords, ignore_case = TRUE)))
  ) %>%
  
  # 5. Remove auxiliary columns
  select(-multi_abstract, -n_words)

# Check
print(nrow(data_clean))
head(data_clean)

#Saving
write_csv(data_clean, "MIG05-25clean.csv")
