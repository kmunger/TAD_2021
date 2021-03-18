

#----------------------------------------
# go over concepts                       ---
#----------------------------------------
# Ensemble methods 
# Ensemble = a group of predictors
# intuition: wisdom of the crowds (best if predictions are independent)
# Bagging: use different random subsets of the training set w. replacement

# Random Forest is an ensemble of decision trees (generally use bagging)
# Random Forest introduces additional source of randomness when growing trees: 
# non RF-decision trees search for the best feature among ALL features when splitting a node
# decision-trees in a RF algorithm search for the best feature among a random subset of features

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())

# load required libraries
library(dplyr)
library(randomForest)
library(mlbench)
library(caret)

# set working directory
setwd("C:/Users/kmm7999/Documents/GitHub/TAD_2019/R_lessons/intro_R")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
table(news_data$category)

# let's work with 2 categories
set.seed(1984)
news_samp <- news_data %>% 
  filter(category %in% c("MONEY", "LATINO VOICES")) %>% 
  group_by(category) %>%
  sample_n(500) %>%  # sample 250 of each to reduce computation time (for lab purposes)
  ungroup() %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "MONEY"])
head(news_samp$text[news_samp$class == "LATINO VOICES"])

# some pre-processing (the rest we'll let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "MONEY" = "money", "LATINO VOICES" = "latino")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
set.seed(1984)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL

#----------------------------------------
# 2. Prepare Data                        ---
#----------------------------------------
library(caret)
library(quanteda)

# create document feature matrix
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix")

# keep tokens that appear in at least 5 headlines
presen_absent <- news_dfm 
presen_absent[presen_absent > 0] <- 1
feature_count <- apply(presen_absent, 2, sum)
features <- names(which(feature_count > 5))
news_dfm <- news_dfm[,features]

# caret package has it's own partitioning function
set.seed(1984)
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

#----------------------------------------
# 3. Using RandomForest                  ---
#----------------------------------------
library(randomForest)
mtry = sqrt(ncol(train_x))  # number of features to sample at each split
ntree = 51  # numbre of trees to grow
# more trees generally improve accuracy but at the cost of computation time
# odd numbers avoid ties (recall default aggregation is "majority voting")
set.seed(1984)
system.time(rf.base <- randomForest(x = train_x, y = train_y, ntree = ntree, mtry = mtry))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])

# print results
print(rf.base)

# plot importance
# gini impurity = how "pure" is given node ~ class distribution
# = 0 if all instances the node applies to are of the same class
# upper bound depends on number of instances
varImpPlot(rf.base, n.var = 10, main = "Variable Importance")



####Be sure to save it as a new file, with a new filename!


# 1. Choose one pre-processing step to modify. Predict whether it will it will change the variable importance plot, and why.

#2. Re-run the model with this alternative specification. Were you right?









