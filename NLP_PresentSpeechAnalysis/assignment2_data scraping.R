# libraries
library(ldatuning)
library(tidyverse)  
library(tidytext)  
library(topicmodels)  
library(tm)         
library(SnowballC)  
library(reticulate)

# read data
url3 <- "C:/Users/rubing/Desktop/研一下/social data science/exam/project2/data/FullData1.csv"
FullData <- read.csv(url3)
names(FullData)[1] <- "id"  

# define text cleaning function
Clean_Text <- function(text) {
  temp <- tolower(text)  #lowercase
  temp <- gsub("[[:punct:][:blank:]]+", " ", temp)  #remove punctuation
  temp <- gsub("[[:digit:]]", "", temp)  #remove numbers
  temp <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", temp)  #remove regular URLs
  temp <- gsub("[A-Za-z]{1,5}[.][A-Za-z]{2,3}/[A-Za-z0-9]+\\b", "", temp)  #remove tiny URLs
  temp <- gsub("review", "", temp)  #remove the word "review"
  temp <- gsub("reviews", "", temp)  #remove the word "reviews"
  temp <- gsub(" mm | pp | bb | q | hn ", "", temp)  #remove specific symbols
  return(temp)
}

#extract and clean the content column
corpus <- FullData$content
corpus <- iconv(corpus, "WINDOWS-1252", "UTF-8")  # Convert encoding
corpus <- Clean_Text(corpus)

#create term matrix
text <- Corpus(VectorSource(corpus))
Content <- TermDocumentMatrix(text)
Content_tidy <- tidy(Content)

#define additional stop words
add_stop_words <- c(
  'like', 'youre', 'ive', 'im', 'really', 'id', 'just', 'dont', 'didnt', 'thi', 'wa',
  'say', 'know', 'make', 'people', "today", "way", "day", "time", "year", 'tonight',
  'live', 'youll', 'youve', 'things', 'thing', 'youre', 'right', 'really', 'lot',
  'little', 'maybe', 'men', "americans", "america",
  'kind', 'heart', "american", "president", "united", "states", "doesn", "obama:well", "Trump", "obama:i", "youtube",
  "thatâ", "â", "."
)
custom_stop_words <- tibble(add_stop_words)
names(custom_stop_words)[1] <- "word"

#remove stop words
newsDTM_tidy_cleaned <- Content_tidy %>% 
  anti_join(stop_words, by = c("term" = "word")) %>%  # Remove English stop words
  anti_join(custom_stop_words, by = c("term" = "word"))  # Remove custom stop words

#reconstruct cleaned documents
stops_documents <- newsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()
names(stops_documents) <- c("stop_document_id", "document_stop_word")

#stem the words
newsDTM_tidy_cleaned <- newsDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

#reconstruct stemmed documents
cleaned_documents <- newsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()
names(cleaned_documents) <- c("stemmed_document_id", "document_stemmed")

#combine cleaned and stemmed data 
FullData_cleaned <- cbind(FullData, cleaned_documents)
FullData_cleaned <- cbind(FullData_cleaned, stops_documents)

#separateby parties 
FullData_cleaned_R <- subset(FullData_cleaned, Party == "R")
FullData_cleaned_D <- subset(FullData_cleaned, Party == "D")

