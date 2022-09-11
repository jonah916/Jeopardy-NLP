# loading libraries

library(readr)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

# importing data

jeopardy <- read_delim("~/Jeopardy/master_season1-35.tsv/master_season1-35.tsv", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)

# filter to clues used in past college tournaments
college <- jeopardy[grepl("College", jeopardy$notes),]

# finding the most common categories in college tournaments

common_categories <- college %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# getting the data for a given category

regex <- "" # EDIT THIS WITH REGEX FOR DESIRED CATEGORY/CATEGORIES
categories_to_use <- common_categories$category[grepl(regex, common_categories$category)]
# filter down to just the clues for the desired category/categories
clues_to_use <- jeopardy[jeopardy$category %in% categories_to_use,]
# get rid of parentheses
clues_to_use$question <- gsub("[()]", "", clues_to_use$question)
# get rid of "a," "an," and "the" (except for categories like Books that deal with titles of works)
clues_to_use$question <- trimws(tolower(removeWords(clues_to_use$question, c("a","an", "the")))) # comment out when dealing with titles
# trimming whitespace
clues_to_use$question <- trimws(removePunctuation(tolower(clues_to_use$question)))

# getting the most common questions for the given category and only keep those appearing at least twice

questions_to_use <- clues_to_use %>%
  group_by(question) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  filter(count >= 2)

# initializing dataframe to hold each pair of questions and file paths to word clouds

anki_df <- data.frame(character(nrow(questions_to_use)), character(nrow(questions_to_use)), stringsAsFactors = FALSE)
j <- 1

# word clouds

# for each question in the list of common questions for this category
for (i in questions_to_use$question){
  
  ## create corpus
  text <- jeopardy$answer[which(jeopardy$question == i)]
  docs <- Corpus(VectorSource(text))

  ## clean text and remove stop words
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))

  ## create document term matrix
  dtm <- TermDocumentMatrix(docs, control = list( wordLengths = c(0,Inf) ))
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix),decreasing=TRUE)
  df <- data.frame(word = names(words),freq=words)

  ## create word cloud & save to jeopardy folder
  set.seed(1234) # for reproducibility
  png(filename = paste0("wordcloud_", removePunctuation(i), ".png"), width = 350, height = 350)
  wordcloud(words = df$word, freq = df$freq, min.freq = 0, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"), scale = c(3.5,0.6))
  dev.off()
  
  ## create word cloud - save to Anki folder
  set.seed(1234) # for reproducibility
  png(filename = paste0("C:/Users/Lenovo/AppData/Roaming/Anki2/User 1/collection.media/", "wordcloud_", removePunctuation(i), ".png"), width = 350, height = 350)
  wordcloud(words = df$word, freq = df$freq, min.freq = 2, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"), scale = c(3.5,0.6))
  dev.off()
  
  ## save to dataframe of questions and word cloud paths that will be used by Anki
  anki_df[j,] <- c(i, paste0("<img src='wordcloud_", removePunctuation(i), ".png' />")) 
  j <- j + 1
}

##
csv_name <- "" ## EDIT WITH NAME OF CURRENT CSV FOR ANKI
write.table(anki_df, csv_name, col.names = FALSE, row.names = FALSE, sep = ",")

# FUNCTION

# inputs:
## CATEGORY: use grepl to look for a common string (e.g. "POET" for "POETRY" and "POETS")
## MIN FREQUENCY: use to filter out uncommon questions (e.g. those that have only appeared in college jeopardy 1 time)
## OMIT WORDS: use to filter out redundant words (e.g. "city" for the category "WORLD CAPITALS")

### The idea is you filter down the big dataset first, look at what some of the common questions are and their frequencies, and
### use that to decide values for MIN FREQUENCY and OMIT WORDS

get_wordclouds <- function(categ, min.frequency = 2, omit.words = NULL, anki.name){
 
  # categories and questions filtering
  categories_to_use <- common_categories$category[grepl(categ, common_categories$category)]
  ## filter down to just the clues for the desired category/categories
  clues_to_use <- jeopardy[jeopardy$category %in% categories_to_use,]
  ## get rid of parentheses
  clues_to_use$question <- gsub("[()]", "", clues_to_use$question)
  clues_to_use$question_cleaned <- clues_to_use$question
  ## get rid of "a," "an," and "the" (except for categories like Books that deal with titles of works)
  clues_to_use$question_cleaned <- removeWords(clues_to_use$question, c("a","an","the")) # comment out when dealing with titles
  ## getting ride of punctuation
  clues_to_use$question_cleaned <- trimws(removePunctuation(tolower(clues_to_use$question_cleaned)))
  ## stripping whitespace
  clues_to_use$question_cleaned <- trimws(removePunctuation(tolower(clues_to_use$question)))
  
  # getting the most common questions for the given category and only keep those appearing at least MIN FREQUENCY times
  
  questions_to_use <- clues_to_use %>%
    group_by(question_cleaned) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>% 
    filter(count >= min.frequency)
  
  # initialize data frame for Anki, which will have question topics and file paths for their respective word clouds
  anki_df <- data.frame(character(nrow(questions_to_use)), character(nrow(questions_to_use)), stringsAsFactors = FALSE)
  j <- 1
  
  # loop through questions to use
  
  for (i in questions_to_use$question_cleaned){
    
    ## create corpus
    text <- jeopardy$answer[which(trimws(removePunctuation(tolower(jeopardy$question)))==i)]
    docs <- Corpus(VectorSource(text))
    
    ## clean text
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    ## document term matrix
    dtm <- TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix),decreasing=TRUE)
    df <- data.frame(word = names(words),freq=words)
    ### remove unnecessary words specified in the function call
    if(!is.null(tolower(omit.words))){df <- filter(df, !(word %in% tolower(omit.words)))}
    
    ## create word cloud & save to jeopardy folder
    set.seed(1234) # for reproducibility
    png(filename = paste0("wordcloud_", removePunctuation(i), ".png"), width = 350, height = 350)
    wordcloud(words = df$word, freq = df$freq, min.freq = 3, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"), scale = c(3.5,0.6))
    dev.off()
    
    ## create word cloud - save to Anki folder
    set.seed(1234) # for reproducibility
    png(filename = paste0("C:/Users/Lenovo/AppData/Roaming/Anki2/User 1/collection.media/", "wordcloud_", removePunctuation(i), ".png"), width = 350, height = 350)
    wordcloud(words = df$word, freq = df$freq, min.freq = 3, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"), scale = c(3.5,0.6))
    dev.off()
    
    ## update csv for anki
    anki_df[j,] <- c(i, paste0("<img src='wordcloud_", removePunctuation(i), ".png' />")) 
    j <- j + 1
  }
  
  # save csv to be used by anki
  write.table(anki_df, paste0(anki.name,".csv"), col.names = FALSE, row.names = FALSE, sep = ",")
 
}

# example: BIOLOGY

get_wordclouds(categ = "BIOLOGY", min.frequency = 3, anki.name = "BIOLOGY")
