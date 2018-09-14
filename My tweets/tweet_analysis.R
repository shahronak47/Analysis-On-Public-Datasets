#Load the libraries
library(tidyverse)
library(wordcloud)
library(tm)


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


#Most used words
#Split strings into words
all_words <- unlist(strsplit(tweets$full_text, "\\s+"))
#Remove @ mention
all_words = all_words[!grepl("^@", all_words)]
#Remove stopwords
all_words <- all_words[!tolower(all_words) %in% stopwords()]

head(sort(table(all_words), decreasing = TRUE), 20)
wordcloud(all_words)

tweets %>%
  filter(in_reply_to_screen_name == "PangebazPorgi") %>%
  select(full_text, tweet_time, in_reply_to_status_id_str) %>%
  View()

tweets$full_text[grepl("#LifeProblems", tweets$full_text)]
