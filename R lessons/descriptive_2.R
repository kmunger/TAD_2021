####Recitation 3

install.packages("quanteda")
library(quanteda)


##toy data

txt <- c(sent1 = "This is an example of the summary method for character objects.",
         sent2 = "The cat in the hat swung the bat.")
corpus_txt<-corpus(txt)

dfm_txt<-dfm(corpus_txt)




##load in data
data("data_corpus_inaugural")


df <- texts(data_corpus_inaugural)

## Lexical diversity measures

# TTR 

toks <- tokens(data_corpus_inaugural) 

tokenz <- lengths(toks)

# typez <- lapply(lapply(tokens,  unique ), length)
typez <- ntype(df)


ttr <- typez / tokenz


##basic plot

plot(ttr)

## 

#df$year<-as.numeric(df$year)

#aggregate(df$ttr, by=list(df$year), FUN=mean)
aggregate(ttr, by = list(docvars(data_corpus_inaugural)$President), FUN = mean)
(df$party)

# another way:
textstat_lexdiv(dfm(data_corpus_inaugural, groups = "President", verbose = FALSE))


##let's look at FRE
textstat_readability(data_corpus_inaugural, "Flesch")




#read<-readability(df$texts)

corr_matrix<-textstat_readability(data_corpus_inaugural, c("Flesch", "Dale.Chall", "SMOG", "Coleman.Liau" ))


corr_matrix_nums<- corr_matrix[,c(2:5)]

cor(corr_matrix_nums)

