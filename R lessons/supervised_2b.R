----------------------
# Set up environment                   ---
#----------------------------------------
# clear global environment
rm(list = ls())

setwd("C:/Users/kevin/Dropbox/TAD_2019/R_practice_staging")

# load required libraries
library(quanteda)
library(readtext)
library(dplyr)
#install.packages('quanteda.textmodels')
#The textmodel_*() functions formerly in quanteda have 
#now been moved to the quanteda.textmodels package.
library(quanteda.textmodels)

#----------------------------------------
# 2 Classification using Word Scores     ---
#----------------------------------------
# Read in conservative and labour manifestos
filenames <- list.files(path = "cons_labour_manifestos")

# Party name and year are in the filename -- we can use substr to extract these to use as our docvars

party <- substr(filenames, 1, 3)
year <- substr(filenames, 4, 7)

# This is how you would make a corpus with docvars from this data
cons_labour_manifestos <- corpus(readtext("cons_labour_manifestos/*.txt"))
docvars(cons_labour_manifestos, field = c("party", "year") ) <- data.frame(cbind(party, year))

# But we're going to use a dataframe
cons_labour_df <- data.frame(text = texts(cons_labour_manifestos),
                         party = party,
                         year = as.integer(year))
colnames(cons_labour_df)

# keep vars of interest --- same as last time, but using the %>$ operator
cons_labour_df <- cons_labour_df %>% select(text, party) %>% setNames(c("text", "class"))


# what's the class distribution?
prop.table(table(cons_labour_df$class))

# randomly sample a test speech
set.seed(1969L)
ids <- 1:nrow(cons_labour_df)
ids_test <- sample(ids, 1, replace = FALSE)
ids_train <- ids[-ids_test]
train_set <- cons_labour_df[ids_train,]
test_set <- cons_labour_df[ids_test,]

# create DFMs
train_dfm <- dfm(train_set$text, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, remove_punct = TRUE, remove = stopwords("english"))

# Word Score model w/o smoothing ----------------
ws_base <- textmodel_wordscores(train_dfm, 
                                y = (2 * as.numeric(train_set$class == "Lab")) - 1 # Y variable must be coded on a binary x in {-1,1} scale, so -1 = Conservative and 1 = Labour
)

# Look at strongest features
lab_features <- sort(ws_base$wordscores, decreasing = TRUE)  # for labor
lab_features[1:10]

con_features <- sort(ws_base$wordscores, decreasing = FALSE)  # for conservative
con_features[1:10]

# Can also check the score for specific features
ws_base$wordscores[c("drugs", "minorities", "unemployment")]

# predict that last speech
test_set$class
predict(ws_base, newdata = test_dfm,
        rescaling = "none", level = 0.95) 

# Word Score model w smoothing ---------------- not that this is NOT the same smoothing as in the NB model
?textmodel_wordscores
ws_sm <- textmodel_wordscores(train_dfm, 
                              y = (2 * as.numeric(train_set$class == "Lab")) - 1, # Y variable must be coded on a binary x in {-1,1} scale, so -1 = Conservative and 1 = Labour
                              smooth = 1
)

# Look at strongest features
lab_features_sm <- sort(ws_sm$wordscores, decreasing = TRUE)  # for labor
lab_features_sm[1:10]

con_features_sm <- sort(ws_sm$wordscores, decreasing = FALSE)  # for conservative
con_features_sm[1:10]

# predict that last speech
test_set$class
predict(ws_sm, newdata = test_dfm,
        rescaling = "none", level = 0.95) 

# Smoothing  ---not as big a deal
plot(ws_base$wordscores, ws_sm$wordscores, xlim=c(-1, 1), ylim=c(-1, 1),
     xlab="No Smooth", ylab="Smooth")

####Be sure to save it as a new file, with a new filename!



# We're using all but one manifesto as a "reference text" --- this isn't how wordscores is supposed to work!


# 1. The farthest left Labour manifesto was in 1983. Re-run the model using only the labour and conservative manifestos from 1983 as 
# reference texts


#2. Predict the rest of the manifestos this way

#3. Other than the reference texts, which are the most extreme?




