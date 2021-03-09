
#----------------------------------------
# 1 Set up environment                   ---
#----------------------------------------
# clear global environment
rm(list = ls())

# set path where our data are stored
setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")



# load required libraries
library(quanteda)
library(dplyr)


#----------------------------------------
# 2 Load data: conservative manifestos ---
#----------------------------------------
# read in the files
filenames <- list.files(path = "conservative_manifestos", full.names=TRUE)
cons_manifestos <- lapply(filenames, readLines)
cons_manifestos <- unlist(lapply(cons_manifestos, function(x) paste(x, collapse = " "))) # because readLines returns a vector with each elements = lines

cons_manifestos[1]


# construct data frame
manifestos_df <- data.frame( text = cons_manifestos, stringsAsFactors = F)


#----------------------------------------
# 3 Dictionaries                         ---
#----------------------------------------
# Here, dictionary = list of words, not the data structure.
# Python users: there is no dictionary object in R :( :( :( (Note: you can create dictionary-like objects using lists)

mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
             "New York City has raised a taxes: an income tax and a sales tax.")

mydict <- c("tax", "income", "capital", "gains", "inheritance")

print(dfm(mytexts, select = mydict))

# Example: Laver Garry dictionary
# https://rdrr.io/github/kbenoit/quanteda.dictionaries/man/data_dictionary_LaverGarry.html
# https://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/laver-garry-dictionary-of-policy-position/
lgdict <- dictionary(file = "LaverGarry.cat", format = "wordstat",encoding = "utf-8")
#  encoding `auto` is no longer supported

# What's in this thing?
lgdict

# Run the conservative manifestos through this dictionary
manifestos_lg <- dfm(manifestos_df$text, dictionary = lgdict)



# how does this look
as.matrix(manifestos_lg)[1:5, 1:5]
featnames(manifestos_lg)

# plot it
plot(     manifestos_lg[,"CULTURE.SPORT"],
     xlab="Year", ylab="SPORTS", type="b", pch=19)

plot( 
     manifestos_lg[,"VALUES.CONSERVATIVE"],
     xlab="Year", ylab="Conservative values", type="b", pch=7)

plot(
     manifestos_lg[,"INSTITUTIONS.CONSERVATIVE"] - manifestos_lg[,"INSTITUTIONS.RADICAL"],
     xlab="Year", ylab="Net Conservative Institutions", type="b", pch=19)

# RID Dictionary--Regressive Imagery Dictionary
# https://www.kovcomp.co.uk/wordstat/RID.html
rid_dict <- dictionary(file = "RID.cat", format = "wordstat",encoding = "utf-8")

data("data_corpus_inaugural")

inaugurals_texts <- texts(data_corpus_inaugural)

# Get the docvars from the corpus object
#year <- (data_corpus_inaugural$documents$Year)
#pres <- (data_corpus_inaugural$documents$President)

year <- (data_corpus_inaugural$Year)
pres <- (data_corpus_inaugural$President)

inaugural_rid_dfm <- dfm(data_corpus_inaugural, dictionary = rid_dict)

# Look at the categories
featnames(inaugural_rid_dfm)

# Inspect the results graphically
plot(year, 
     inaugural_rid_dfm[,"PRIMARY.REGR_KNOL.NARCISSISM"],
     xlab="Year", ylab="Narcissism", type="b", pch=19)

plot(year, 
     inaugural_rid_dfm[,"EMOTIONS.POSITIVE_AFFECT"],
     xlab="Year", ylab="Positivity", type="b", pch=19)





plot(year, 
     inaugural_rid_dfm[,"PRIMARY.ICARIAN_IM.FIRE"] + inaugural_rid_dfm[,"PRIMARY.ICARIAN_IM.ASCEND"] +inaugural_rid_dfm[,"PRIMARY.ICARIAN_IM.DESCENT"] +
       inaugural_rid_dfm[,"PRIMARY.ICARIAN_IM.DEPTH"] + inaugural_rid_dfm[,"PRIMARY.ICARIAN_IM.HEIGHT"] + inaugural_rid_dfm[,"PRIMARY.ICARIAN_IM.WATER"],
     xlab="Year", ylab="Icarian-ness", type="b", pch=19)






