
# basic intuition:
# a. documents are represented as random mixtures over latent topics.
# b. a topic is characterized by a distribution over words.
# we now propose a GENERATIVE MODEL OF THE DATA
# want to maximize the probability of a corpus as a function of our parameters (of the dirichlets) and latent variables (doc topic mixtures and topic word distributions).

rm(list = ls())


setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")
set.seed(1234)

# Check for these packages, install them if you don't have them
# install.packages("tidytext")
# install.packages("topicmodels")
# install.packages("stringi")
# install.packages("doParallel")
#install.packages("ldatuning")
library(ldatuning)

libraries <- c("ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi", "tidyr")
lapply(libraries, require, character.only = TRUE)

## 1 Preprocessing

# Load data
blm_tweets <- read.csv("blm_samp.csv", stringsAsFactors = F)


# Create date vectors
blm_tweets$datetime <- as.POSIXct(strptime(blm_tweets$created_at, "%a %b %d %T %z %Y",tz = "GMT")) # full date/timestamp
blm_tweets$date <- mdy(paste(month(blm_tweets$datetime), day(blm_tweets$datetime), year(blm_tweets$datetime), sep = "-")) # date only

# Collapse tweets so we are looking at the total tweets at the day level
blm_tweets_sum <- blm_tweets %>% group_by(date) %>% summarise(text = paste(text, collapse = " "))

# Remove non ASCII characters
blm_tweets_sum$text <- stringi::stri_trans_general(blm_tweets_sum$text, "latin-ascii")

# Removes solitary letters
blm_tweets_sum$text <- gsub(" [A-z] ", " ", blm_tweets_sum$text)

# As always we begin with a DFM.
# Create DFM
blm_dfm <-dfm(blm_tweets_sum$text, stem = F, remove_punct = T, tolower = T, remove_twitter = T, remove_numbers = TRUE, remove = c(stopwords("english"), "http","https","rt", "t.co"))

# Topic models
## 2 Selecting K

# Identify an appropriate number of topics (FYI, this function takes a while)
k_optimize_blm <- FindTopicsNumber(
  blm_dfm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)

dev.new()
FindTopicsNumber_plot(k_optimize_blm)


# What does robustness mean here?

## 3 Visualizing Word weights

# Set number of topics
k <- 19

# Fit the topic model with the chosen k
system.time(
  blm_tm <- LDA(blm_dfm, k = k, method = "Gibbs",  control = list(seed = 1234)))



##how does the model perform statistically?
blm_tm@loglikelihood


# gamma = posterior document distribution over topics
# what are the dimensions of gamma?

topics(blm_tm)

# Per topic per word proabilities matrix (beta)
get_terms(blm_tm, k = 5)

## 4 Visualizing topic trends over time

# Store the results of the mixture of documents over topics 
doc_topics <- blm_tm@gamma

# Store the results of words over topics
#words_topics <- blm_tm@beta

# Transpose the data so that the days are columns
doc_topics <- t(doc_topics)
dim(doc_topics)
doc_topics[1:5,1:5]

# Arrange topics
# Find the top topic per column (day)
max <- apply(doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Freddie Gray", "Sandra Bland")
shootings <- mdy(c("04/12/2015","7/13/2015"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(blm_tweets_sum$date))

# Plot
blm_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

blm_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() + 
  ylab("Topic Number") + ggtitle("BLM-Related Tweets from 2014 to 2016 over Topics") + geom_point() + xlab(NULL) + 
  geom_vline(xintercept=as.numeric(shootings[1]), color = "blue", linetype=4) + # Freddie Gray (Topic)
  geom_vline(xintercept=as.numeric(shootings[2]), color = "black", linetype=4)  + # Sandra Bland
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 



#### ## Modification for in-class assignment

###Sample just the first 5000 of the initial tweets. Re-run the topic selection algorithm. What does the best number of topics look like now?


#### Do you think that the number of topics is linear in the number of documents??



