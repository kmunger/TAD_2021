
library("quanteda")


setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")

wiki_corp<-readRDS(file = "wiki.RDS")



## Select features

#First, we tokenize the corpus, and then get the names of the features that occur five times or more.
#Trimming the features before constructing the fcm:
#  {r}
wiki_toks <- tokens(wiki_corp)
feats <- dfm(wiki_toks, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 5) %>%
  featnames()
# leave the pads so that non-adjacent words will not become adjacent
wiki_toks <- tokens_select(wiki_toks, feats, padding = TRUE)


## Construct the feature co-occurrence matrix

#{r}
wiki_fcm <- fcm(wiki_toks, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)


## Fit word embedding model

#Fit the [GloVe model](https://nlp.stanford.edu/pubs/glove.pdf) using [**rsparse**](http://text2vec.org).

library("text2vec")


#GloVe is an unsupervised learning algorithm for obtaining vector representations for words. Training is performed on aggregated global word-word co-occurrence statistics from a corpus, and the resulting representations showcase interesting linear substructures of the word vector space.

#GloVe encodes the ratios of word-word co-occurrence probabilities, which is thought to represent some crude form of meaning associated with the abstract concept of the word, as vector difference. The training objective of GloVe is to learn word vectors such that their dot product equals the logarithm of the words' probability of co-occurrence.  

glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(wiki_fcm, n_iter = 10,
convergence_tol = 0.01, n_threads = 8)
dim(wv_main)


## Averaging learned word vectors

#The two vectors are main and context. According to the Glove paper, averaging the two word vectors results in more accurate representation.


wv_context <- glove$components
dim(wv_context)
word_vectors <- wv_main + t(wv_context)


## Examining term representations

#Now we can find the closest word vectors for `paris - france + germany`

#{r}
berlin <- word_vectors["paris", , drop = FALSE] -
word_vectors["france", , drop = FALSE] +
library("quanteda.textstats")
cos_sim <- textstat_simil(x = as.dfm(word_vectors), y = as.dfm(berlin),
method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 5)


#Here is another example for `london = paris - france + uk + england`

london <-  word_vectors["paris", , drop = FALSE] -
word_vectors["france", , drop = FALSE] +
word_vectors["uk", , drop = FALSE] +
word_vectors["england", , drop = FALSE]
cos_sim <- textstat_simil(as.dfm(word_vectors), y = as.dfm(london),
margin = "documents", method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 5)



#Here is another example 

author<-  word_vectors["philadelphia", , drop = FALSE] - 
  word_vectors["pennsylvania", , drop = FALSE] +
  word_vectors["ohio", , drop = FALSE]
cos_sim <- textstat_simil(as.dfm(word_vectors), y = as.dfm(author),
                          margin = "documents", method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 5)


##play around with this --- make sure your word is in the actual list 


author<-  word_vectors["cher", , drop = FALSE] - 
  word_vectors["diva", , drop = FALSE] +
  word_vectors["drama", , drop = FALSE]
cos_sim <- textstat_simil(as.dfm(word_vectors), y = as.dfm(author),
                          margin = "documents", method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 5)


