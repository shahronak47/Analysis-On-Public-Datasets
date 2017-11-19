Questions <- read.csv('C:\\Users\\Ronak Shah\\Downloads\\Questions.csv')
Answers <- read.csv('C:\\Users\\Ronak Shah\\Downloads\\Answers.csv')
Tags <- read.csv('C:\\Users\\Ronak Shah\\Downloads\\Tags.csv')

table(Answers$IsAcceptedAnswer)
#FALSE   TRUE 
#140259 110529 

sum(Questions$Id %in% Answers$ParentId)
#[1] 159337

sum(Questions$Id %in% Answers$ParentId)/nrow(Questions)
#[1] 0.8389249

Unanswered_questions <- Questions[!Questions$Id %in% Answers$ParentId, ]
Answered_questions <- Questions[Questions$Id %in% Answers$ParentId, ]
Unanswered_questions <- read.csv("C:\\Users\\Ronak Shah\\Downloads\\Unanswered_Questions.csv")
Answered_questions <- read.csv("C:\\Users\\Ronak Shah\\Downloads\\Answered_questions.csv")
Unanswered_questions$tags <- sapply(Unanswered_questions$Id, function(x) Tags$Tag[Tags$Id %in% x])
hist(unlist(Unanswered_questions$tags))

head(sort(table(unlist(Unanswered_questions$tags)), decreasing = TRUE), 10)

#ggplot2    shiny        plot     rstudio   dataframe  r-markdown       dplyr  data.table       knitr time-series 
#2314        2172        1275         867         769         613         590         578         567         525 

sample(Unanswered_questions$Title[sapply(Unanswered_questions$tags, function(x) "rstudio" %in% x)], 10)

sum(nchar(Unanswered_questions$Body))/nrow(Unanswered_questions)
#[1] 1639.612

sum(nchar(Questions$Body))/nrow(Questions)
#[1] 1456.157

sum(nchar(Answered_questions$Body))/nrow(Answered_questions)
#[1] 1420.619


pos_words <- read.csv('C:\\Users\\Ronak Shah\\Downloads\\Positive_words.csv')
neg_words <- read.csv('C:\\Users\\Ronak Shah\\Downloads\\Negative_words.csv')


pos_score <- sapply(strsplit(as.character(Answered_questions$Body), " "), function(x) sum(x %in% pos_words$words))
neg_score <- sapply(strsplit(as.character(Answered_questions$Body), " "), function(x) sum(x %in% neg_words$words))
mean(pos_score - neg_score)
#[1] 0.3726567

pos_score <- sapply(strsplit(as.character(Unanswered_questions$Body), " "), function(x) sum(x %in% pos_words$words))
neg_score <- sapply(strsplit(as.character(Unanswered_questions$Body), " "), function(x) sum(x %in% neg_words$words))
mean(pos_score - neg_score)

#[1] 0.04543523
