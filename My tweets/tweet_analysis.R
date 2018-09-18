#Load the libraries
library(tidyverse)
library(lubridate)
library(wordcloud)
library(tm)
library(syuzhet)
library(spacyr)
library(openNLP)
library(NLP)
library(stringr)
library(RDRPOSTagger)

tweets <- read.csv("/Users/Ronak Shah/Google Drive/Analysis-On-Public-Datasets/My tweets/Data/__ROOT__.tsv", sep = "\t", stringsAsFactors = FALSE)
#Convert tweet class to posixct format
tweets$tweet_time <- as.POSIXct(tweets$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")

#Range of tweet time
range(tweets$tweet_time, na.rm = TRUE)

#Most number of people I have replied to 
#Removing the ones whoch are not in reply to anyone

max_replies <- data.frame(head(sort(table(tweets$in_reply_to_screen_name[tweets$in_reply_to_screen_name != ""]), decreasing = TRUE), 10))

ggplot(max_replies, aes(Var1, Freq)) + 
  geom_bar(stat="identity", color = "blue", fill = "black") + 
  xlab("Users") + ylab("Number of mentions")


#Time of the tweets
tweet_hours <- data.frame(table(format(tweets$tweet_time, "%H")))

ggplot(tweet_hours, aes(Var1, Freq, group = 1)) + 
  geom_line(color = "blue") + 
  xlab("Hours") + ylab("Number of tweets")


#Most used words using wordcloud
#Split strings into words
all_words <- unlist(strsplit(tweets$full_text, "\\s+"))
#Remove @ mention
all_words = all_words[!grepl("^@", all_words)]
#Remove stopwords
all_words <- all_words[!tolower(all_words) %in% stopwords()]

wordcloud(all_words)

#Applying the same for every sentence, cleaning every sentence
tweets$clean_text <- unname(sapply(tweets$full_text, function(x) {
  without_at = sub("@\\S+ ", "",x)
  all_words = unlist(strsplit(tolower(without_at), "\\s+"))
  paste(all_words[!all_words %in% stopwords()], collapse = " ")
}))


#Sentiment score for every sentence
tweets$sentiment_score <- get_sentiment(tweets$clean_text)

#plot taking the mean of each hour
tweets %>%
  group_by(hour = hour(tweet_time)) %>%
  summarise(hourly_sentiment_mean = mean(sentiment_score, na.rm = TRUE)) %>%
  ggplot() + aes(hour, hourly_sentiment_mean) + 
  geom_line()

#Which things I talk the most
tagger <- rdr_model(language = "English", annotation = "POS")

proper_nouns <- character()
i <- 1
while(i <= nrow(tweets)) {
  df <- rdr_pos(tagger, removePunctuation(tweets$clean_text[i]))
  proper_nouns <- c(proper_nouns, df$token[df$pos == "NNP"])
  i = i + 1
  print(i)
}
