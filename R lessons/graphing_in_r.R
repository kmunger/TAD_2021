library(quanteda)

##taken from Ken Benoit's Quanteda docs


# Create a dfm from a somewhat smaller corpus
inaugDfm <- dfm(data_corpus_inaugural[0:10], remove =  stopwords('english') , remove_punct = TRUE)


# Some words will not fit on a plot this size, so suppress those warings
textplot_wordcloud(dfm_trim(inaugDfm, min_termfreq = 10, verbose = FALSE))

#You can also plot a “comparison cloud”, but this can only be done with fewer than eight documents:
  
  compDfm <- dfm(corpus_subset(data_corpus_inaugural, President %in% c("Washington", "Jefferson", "Madison")),
                 groups = "President", remove = stopwords("english"), remove_punct = TRUE)
textplot_wordcloud(dfm_trim(compDfm, min_termfreq = 5, verbose = FALSE), comparison = TRUE)

#Plot will pass through additional arguments to the underlying call to wordcloud.

textplot_wordcloud(inaugDfm, min_termfreq = 10,
                   colors = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))

#2. Lexical dispersion plot
#Plotting a kwic object produces a lexical dispersion plot which allows us to visualize the occurrences of particular terms throughout the text. We call these “x-ray” plots due to their similarity to the data produced by Amazon’s “x-ray” feature for Kindle books.

textplot_xray(kwic(data_corpus_inaugural[50:57], "american"))

#You can also pass multiple kwic objects to plot to compare the dispersion of different terms:
  
  textplot_xray(
    kwic(data_corpus_inaugural[50:57], "american"),
    kwic(data_corpus_inaugural[50:57], "people")
  )

#If you’re only plotting a single document, but with multiple keywords, then the keywords are displayed one below the other rather than side-by-side.
library(readtext)
  data_char_mobydick <- texts(readtext("http://www.gutenberg.org/cache/epub/2701/pg2701.txt"))
  
textplot_xray(
  kwic(data_char_mobydick, "whale"),
  kwic(data_char_mobydick, "ahab")
)

#You might also have noticed that the x-axis scale is the absolute token index for single texts and relative token index when multiple texts are being compared. If you prefer, you can specify that you want an absolute scale:
  
  textplot_xray(
    kwic(data_corpus_inaugural[50:57], "american"),
    kwic(data_corpus_inaugural[50:57], "people"),
    scale = 'absolute'
  )

#In this case, the texts may not have the same length, so the tokens that don’t exist in a particular text are shaded in grey.

#Modifying lexical dispersion plots
#The object returned is a ggplot object, which can be modified using ggplot:
  
  library(ggplot2)
theme_set(theme_bw())
g <- textplot_xray(
  kwic(data_corpus_inaugural[50:57], "american"),
  kwic(data_corpus_inaugural[50:57], "people"),
  kwic(data_corpus_inaugural[50:57], "communist")
)
g + aes(color = keyword) + scale_color_manual(values = c('blue', 'red', 'green'))

#3. Frequency plots
#You can plot the frequency of the top features in a text using topfeatures.

inaugFeatures <- topfeatures(inaugDfm, 100)

# Create a data.frame for ggplot
topDf <- data.frame(
  list(
    term = names(inaugFeatures),
    frequency = unname(inaugFeatures)
  )
)

# Sort by reverse frequency order
topDf$term <- with(topDf, reorder(term, -frequency))

ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#If you wanted to compare the frequency of a single term across different texts, you could plot the dfm matrix like this:
  
  americanFreq <- data.frame(list(
    document = rownames(inaugDfm[, 'american']),
    frequency = unname(as.matrix(inaugDfm[, 'american']))
  ))

ggplot(americanFreq) + geom_point(aes(x=document,y=frequency)) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

#The above plots are raw frequency plots. For relative frequency plots, (word count divided by the length of the chapter) we can weight the document-frequency matrix. To obtain expected word frequency per 100 words, we multiply by 100.

relDfm <- dfm_weight(inaugDfm, scheme='prop') * 100
head(relDfm)

relFreq <- data.frame(list(
  document = rownames(inaugDfm[, 'american']),
  frequency = unname(as.matrix(relDfm[, 'american']))
))

ggplot(relFreq) + geom_point(aes(x=document,y=frequency)) +
  theme(axis.text.x = element_text(angle=90, hjust=1))


#######intro to ggplot2

##taken from here https://ggplot2-book.org/getting-started.html
mpg

# create canvas
ggplot(mpg)

# variables of interest mapped
ggplot(mpg, aes(x = displ, y = hwy))

# data plotted
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

##aesthetics

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

##geometries
# Left column: x and y mapping needed!
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()

# Right column: no y mapping needed!
ggplot(data = mpg, aes(x = class)) +
  geom_bar()  

ggplot(data = mpg, aes(x = hwy)) +
  geom_histogram() 



# plot with both points and smoothed line
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue") +
  geom_smooth(color = "red")

# color aesthetic passed to each geom layer
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth(se = FALSE)

# color aesthetic specified for only the geom_point layer
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE)



### in -class exercise 


##modify the plot on line 83 of the top terms so that it plots the top terms from Moby Dick. Instead of black dots, visualize
### this with a blue line.

##
