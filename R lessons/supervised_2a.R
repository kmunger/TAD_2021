----------------------
# Set up environment                   ---
#----------------------------------------
# clear global environment
rm(list = ls())

setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")

# load required libraries
library(quanteda)
install.packages('quanteda.textmodels')
#The textmodel_*() functions formerly in quanteda have 
#now been moved to the quanteda.textmodels package.
library(quanteda.textmodels)
library(readtext)
library(dplyr)

#----------------------------------------
# 1 Supervised Learning: Naive Bayes     ---
#----------------------------------------
#source of data: https://www.kaggle.com/rmisra/news-category-dataset#
News_Category_Dataset_v2.json
# load data
news_data <- readRDS("news_data.rds")

## BIG Dataset!

# subset data and keep relevant variables
#filter-- restricts dataset, dropping rows

news_samp <- filter(news_data, category %in% c("CRIME", "SPORTS"))
  
##select --- keeps only named columns

news_samp1 <- select(news_samp, headline, category)


##setNames --- rename the variables in our new dataset

news_samp2<-setNames(object = news_samp1, nm = c("text", "class"))


# get a sense of how the text looks
dim(news_samp2)
head(news_samp2$text[news_samp2$class == "CRIME"])
head(news_samp2$text[news_samp2$class == "SPORTS"])

# some pre-processing (the rest will let dfm do)
news_samp2$text <- gsub(pattern = "'", "", news_samp2$text)  # replace apostrophes
head(news_samp2$text[news_samp2$class == "SPORTS"])

# what's the distribution of classes?
prop.table(table(news_samp2$class))

# split sample into training & test sets
set.seed(1984L)

###what does this do??

prop_train <- 0.8

###need to create a training set that's 80% of the data
ids <- 1:nrow(news_samp2)

ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)

## the - sign is "not"
ids_test <- ids[-ids_train]
train_set <- news_samp2[ids_train,]
test_set <- news_samp2[ids_test,]

# get dfm for each set
train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))

# how does this look?
as.matrix(train_dfm)[1:5,1:5]

# match test set dfm to train set dfm features
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))

# w/o smoothing ----------------

# train model on the training set
nb_model <- textmodel_nb(train_dfm, train_set$class, smooth = 0, prior = "uniform")

# evaluate on test set
predicted_class <- predict(nb_model, newdata = test_dfm)

# baseline --- This is important, to see how much our model beats a model that just picks the modal class 
baseline_acc <- max(prop.table(table(test_set$class)))

# get confusion matrix
cmat <- table(test_set$class, predicted_class)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

# w smoothing ----------------

# train model on the training set using Laplace smoothing
##Recall what this does --- want to avoid having any zeroes, which happens if there's a novel word in the test set

nb_model_sm <- textmodel_nb(train_dfm, train_set$class, smooth = 1, prior = "uniform")

# evaluate on test set
predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm)

# get confusion matrix
cmat_sm <- table(test_set$class, predicted_class_sm)
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)

# take a look at the most discriminant features (get some face validity)
posterior <- data.frame(feature = rownames(t(nb_model_sm$param)), 
                        post_CRIME = t(nb_model_sm$param)[,1],
                        post_SPORTS = t(nb_model_sm$param)[,2])


head(arrange(posterior, -post_SPORTS))
head(arrange(posterior, -post_CRIME))


# what does smoothing do? More generally, reduces the "weight" place on new information (the likelihood) vis-a-vis the prior. 
plot(nb_model$param[1,], nb_model_sm$param[1,], xlim = c(0,0.02), ylim = c(0,0.02), xlab="No Smooth", ylab="Smooth") + abline(a = 0, b = 1, col = "red")

##don't worry about that code






#########Practice questions:

# 1. We originally set the proportion of the training set to be .8 --- what happens to performance when we set it at .2? Re-run all the code 
##after that point (with smoothing)



# 2. Read the help file about textmodel_nb . What does the "prior" argument do in Naive Bayes? What is the default value for this 
# argument in quanteda?


#3. Re-run the code with prior = "docfreq". Think about the proportions of classes in the dataset. How should this affect your outcome?
# Looking at the results of the code, were you right?



