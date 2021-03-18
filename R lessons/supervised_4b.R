
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
news_samp$class <- dplyr::recode(news_samp$class,  "MONEY" = "money", "LATINO VOICES" = "latino")

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
# 4. RandomForest Using Caret            ---
#----------------------------------------
trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
mtry <- sqrt(ncol(train_x))
ntree = 51  
tunegrid <- expand.grid(.mtry = mtry)
set.seed(1984)
system.time(rf.caret <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl,
                              ntree = ntree))

# print results
print(rf.caret)

# plot importance
varImpPlot(rf.caret$finalModel, n.var = 10, main = "Variable Importance")

#----------------------------------------
# 5. RandomForest Using Caret + tuning   ---
#----------------------------------------
# note: the package RandomForest also has its own tuning function: tuneRF
trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = c(0.5*mtry, mtry, 1.5*mtry))  # at the moment caret only allows tuning of mtry (partly b/c ntree is just a matter of computational constratints)
set.seed(1984)
system.time(rf.grid <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = ntree))
# print grid search results
print(rf.grid)

# plot grid search results
plot(rf.grid)

#----------------------------------------
# 6. RandomForest Using Caret + manual tuning ---
#----------------------------------------
mtry <- sqrt(ncol(train_x))
tunegrid <- expand.grid(.mtry = mtry)
# ntree = 1
set.seed(1984)
system.time(rf.man1 <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, ntree = 1))

# ntree = 5
set.seed(1984)
system.time(rf.man2 <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, ntree = 5))

# ntree = 51
set.seed(1984)
system.time(rf.man3 <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, ntree = 51))

# collect results & summarize
results <- resamples(list(rf1 = rf.man1, rf5 = rf.man2, rf51 = rf.man3))
summary(results)

# test set accuracy
confusionMatrix(predict(rf.man1, newdata = test_x), test_y)
confusionMatrix(predict(rf.man2, newdata = test_x), test_y)
confusionMatrix(predict(rf.man3, newdata = test_x), test_y)

# box and whisker plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
dev.new()
bwplot(results, scales = scales)



####Be sure to save it as a new file, with a new filename!


# 1.Look at the results from the manual tuning, compared to the automatic tuning in section 5. 

# 2. Why does the accuracy always increase across the three levels of the parameter in the manual tuning but not the 
# automatic tuning? 








