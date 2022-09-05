library(readr)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

jeopardy <- read_delim("~/Jeopardy/master_season1-35.tsv/master_season1-35.tsv", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)

college <- jeopardy[grepl("College", jeopardy$notes),]

# daily doubles

daily_double_j <- college %>% 
  filter(daily_double == "yes",
         air_date > "2001-11-26",
         round == 1,
         value %in% c(seq(200, 1000, by = 200))) %>% 
  group_by(value) %>% 
  summarise(count = n())
daily_double_j <- daily_double_j %>% mutate(prop = count/sum(count))
daily_double_dj <- college %>% 
  filter(daily_double == "yes",
         air_date > "2001-11-26",
         round == 1,
         value %in% c(seq(400, 2000, by = 400))) %>% 
  group_by(value) %>% 
  summarise(count = n())
daily_double_dj <- daily_double_dj %>% mutate(prop = count/sum(count))

##### 

college %>% filter(round != 3) %>% group_by(notes, category) %>% summarise(count = n()) %>% filter(category == "SHAKESPEARE")

# most common questions



# most common categories

common_categories <- college %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# POETRY

categories_to_use <- common_categories$category[grepl("MOVIE|FILM|CINEMA", common_categories$category)]
clues_to_use <- jeopardy[jeopardy$category %in% categories_to_use,]
clues_to_use$question <- gsub("[()]", "", clues_to_use$question)
clues_to_use$question <- trimws(tolower(removeWords(clues_to_use$question, c("a","an", "the")))) # comment out when dealing with titles
clues_to_use$question <- trimws(removePunctuation(tolower(clues_to_use$question)))
questions_to_use <- clues_to_use %>%
  group_by(question) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  filter(count >= 2)

anki_df <- data.frame(character(nrow(questions_to_use)), character(nrow(questions_to_use)), stringsAsFactors = FALSE)
j <- 1

# word clouds


for (i in questions_to_use$question){
  
  ## create corpus
  text <- jeopardy$answer[which(jeopardy$question == i)]
  docs <- Corpus(VectorSource(text))

  ## clean text
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))

  ## document term matrix
  dtm <- TermDocumentMatrix(docs, control = list( wordLengths = c(0,Inf) ))
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix),decreasing=TRUE)
  df <- data.frame(word = names(words),freq=words)
  #  filter(!(word %in% c("shakespeare", "shakespeares", "shakespearean", "city", "citys", "cities", "play", "plays", "character", "characters")))
  #df <- df[-which(df$word %in% c("country", "state", "countrys", "continent", "called")),]
  #df <- df[-which(df$word %in% c("poet", "wrote", "poem", "poets", "poems", "story", "stories")),]
  #df <- df[-which(df$word %in% c("city", "citys", "country", "countries", "countrys", "cities", "capital", "capitals")),]
  #df <- df[-which(df$word %in% c("shakespeare", "shakespeares", "shakespearean", "city", "citys", "cities")),]

  ## create word cloud - save to jeopardy folder
  set.seed(1234) # for reproducibility
  png(filename = paste0("wordcloud_", removePunctuation(i), ".png"), width = 350, height = 350)
  wordcloud(words = df$word, freq = df$freq, min.freq = 0, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"), scale = c(3.5,0.6))
  dev.off()
  
  ## create word cloud - save to Anki folder
  set.seed(1234) # for reproducibility
  png(filename = paste0("C:/Users/Lenovo/AppData/Roaming/Anki2/User 1/collection.media/", "wordcloud_", removePunctuation(i), ".png"), width = 350, height = 350)
  wordcloud(words = df$word, freq = df$freq, min.freq = 2, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"), scale = c(3.5,0.6))
  dev.off()
  
  ## save to csv
  anki_df[j,] <- c(i, paste0("<img src='wordcloud_", removePunctuation(i), ".png' />")) 
  j <- j + 1
}

write.table(anki_df, "BIOLOGY.csv", col.names = FALSE, row.names = FALSE, sep = ",")

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
  clues_to_use <- jeopardy[jeopardy$category %in% categories_to_use,]
  clues_to_use$question <- gsub("[()]", "", clues_to_use$question)
  clues_to_use$question_cleaned <- clues_to_use$question
  #clues_to_use$question_cleaned <- removeWords(clues_to_use$question, c("a","an","the")) # comment out when dealing with titles
  clues_to_use$question_cleaned <- trimws(removePunctuation(tolower(clues_to_use$question_cleaned)))
  #clues_to_use$question_cleaned <- trimws(removePunctuation(tolower(clues_to_use$question)))
  questions_to_use <- clues_to_use %>%
    group_by(question_cleaned) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>% 
    filter(count >= min.frequency)
  
  # initialize data frame for Anki
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
    if(!is.null(tolower(omit.words))){df <- filter(df, !(word %in% tolower(omit.words)))}
    
    ## create word cloud - save to jeopardy folder
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
  
  # save csv for anki
  write.table(anki_df, paste0(anki.name,".csv"), col.names = FALSE, row.names = FALSE, sep = ",")
  
  
}

# BIOLOGY

get_wordclouds(categ = "BIOLOGY", min.frequency = 3, anki.name = "BIOLOGY")

# NONFICTION

get_wordclouds(categ = "NONFICTION", min.frequency = 2, anki.name = "NONFICTION")

# POETS AND POETRY

get_wordclouds(categ = "POET", min.frequency = 3, omit.words = c("POET","POETS","WROTE","WRITTEN","POETRY"), anki.name = "POETS & POETRY")

# WORD ORIGINS

get_wordclouds(categ = "WORD.+ORIGINS", min.frequency = 2, anki.name = "WORD ORIGINS")

# COLLEGES & UNIVERSITIES

get_wordclouds(categ = "COLLEG", min.frequency = 2, anki.name = "COLLEGES & UNIVERSITIES")

# US HISTORY

get_wordclouds(categ = "(U.S.|AMERICAN) HISTOR", min.frequency = 3, omit.words = c("STATE", "STATES", "CITY", "CITIES"), anki.name = "US HISTORY")

# CLIFF'S NOTES

get_wordclouds(categ = "CLIFF", min.frequency = 1, anki.name = "CLIFFS NOTES")

# THE BIBLE

get_wordclouds(categ = "BIBL", min.frequency = 3, anki.name = "THE BIBLE")

# ISLANDS

get_wordclouds(categ = "ISLAND", min.frequency = 2, omit.words = c("ISLAND", "ISLANDS", "COUNTRY", "COUNTRIES", "NATION", "NATIONs"), anki.name = "ISLANDS")

# MOVIES

get_wordclouds(categ = "MOVIE|FILM|CINEMA", min.frequency = 2, omit.words = c("MOVIE", "MOVIES", "FILM", "FILMS", "CINEMATIC", "CINEMA", "ACTOR", "ACTORS", "ACTRESS"), anki.name = "MOVIES")
