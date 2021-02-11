

# Manupulating text in R

### Kenneth Benoit

#In this section we will work through some basic string manipulation functions in R.

## String handling in base R

#There are several useful string manipulation functions in the R base library. In addition, we will look at the `stringr` package which provides an additional interface for simple text manipulation.

#The fundamental type (or `mode`) in which R stores text is the character vector. The most simple case is a character vector of length one. The `nchar` function returns the number of characters in a character vector. 

require(quanteda)
s1 <- 'my example text'
length(s1)
nchar(s1)

#The `nchar` function is vectorized, meaning that when called on a vector it returns a value for each element of the vector.

s2 <- c('This is', 'my example text.', 'So imaginative.')
length(s2)
nchar(s2)
sum(nchar(s2))

#We can use this to answer some simple questions about the inaugural addresses.

#Which were the longest and shortest speeches?

inaugTexts <- texts(data_corpus_inaugural)
which.max(nchar(inaugTexts))
which.min(nchar(inaugTexts))

#Unlike in some other programming languages, it is not possible to index into a "string" -- where a string is defined as a sequence of text characters -- in R:
  
s1 <- 'This file contains many fascinating example sentences.'
s1[6:9]

#To extract a substring, instead we use the `substr` function. 

s1 <- 'This file contains many fascinating example sentences.'
substr(s1, 6,9)

#Often we would like to split character vectors to extract a term of interest. This is possible using the `strsplit` function. Consider the names of the inaugural texts:
  
names(inaugTexts)
# returns a list of parts
s1 <- 'split this string'
strsplit(s1, 'this')
parts <- strsplit(names(inaugTexts), '-')
years <- sapply(parts, function(x) x[1])
pres <-  sapply(parts, function(x) x[2])

#The `paste` function is used to join character vectors together. The way in which the elements are combined depends on the values of the `sep` and `collapse` arguments:
  
paste('one','two','three')
paste('one','two','three', sep='_')
paste(years, pres, sep='-')
paste(years, pres, collapse='-')

#`tolower` and `toupper` change the case of character vectors.
tolower(s1)
toupper(s1)

#Character vectors can be compared using the `==`  and `%in%` operators:
tolower(s1) == toupper(s1)
'apples'=='oranges'
tolower(s1) == tolower(s1)
'pears' == 'pears'

c1 <- c('apples', 'oranges', 'pears')
'pears' %in% c1
c2 <- c('bananas', 'pears')
c2 %in% c1

## Pattern matching and regular expressions

#Matching texts based on generalized patterns is one of the most common, and the most useful, operations in text processing.  The most powerful variant of these is known as a _regular expression_.

#A regular  expression (or "regex"" for short) is a special text string for describing a search pattern. You may have probably already used a simple form of regular expression, called a ["glob"](https://en.wikipedia.org/wiki/Glob_(programming)), that uses wildcards for pattern matching.  For instance, `*.txt` in a command prompt or Terminal window will find all files ending in `.txt`.  Regular expressions are like glob wildcards on steroids.  The regex equivalent is `^.*\.txt$`.  R even has a function to convert from glob expressions to regular expressions:  `glob2rx()`.  

#In **quanteda**, all functions that take pattern matches allow [three types of matching](http://quanteda.io/reference/valuetype.html): fixed matching, where the match is exact and no wildcard characters are used; "glob" matching, which is simple but often sufficient for a user's needs; and regular expressions, which unleash the full power of highly sophisticated (but also complicated) pattern matches.

### Regular expressions in base R

#The base R functions for searching and replacing within text are similar to familiar commands from the other text manipulation environments, `grep` and `gsub`. The `grep` manual page provides an overview of these functions.

#The `grep` command tests whether a pattern occurs within a string:

grep('orangef', 'these are oranges')
grep('pear', 'these are oranges')
grep('orange', c('apples', 'oranges', 'pears'))
grep('pears', c('apples', 'oranges', 'pears'))

#The `gsub` command substitutes one pattern for another within a string:

gsub('oranges', 'apples', 'these are oranges')

### Regular expressions in **stringi** and **stringr**

#### Matching

#Using some **stringr** functions, we can see more about how regular expression pattern matching works.  In a regular expression, `.` means "any character".  So using regular expressions with **stringr**, we have:

require(stringr)
pattern <- "a.b"
strings <- c("abb", "a.b")
str_detect(strings, pattern)

#Some variations

# Regular expression variations
str_extract_all("The Cat in the Hat", "[a-z]+")
str_extract_all("The Cat in the Hat", regex("[a-z]+", TRUE))

str_extract_all("a\nb\nc", "^.")
str_extract_all("a\nb\nc", regex("^.", multiline = TRUE))

str_extract_all("a\nb\nc", "a.")
str_extract_all("a\nb\nc", regex("a.", dotall = TRUE))

#### Replacing

#Besides extracting strings, we can also replace them:

fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")

str_replace(fruits, "([aeiou])", "")
str_replace(fruits, "([aeiou])", "\\1\\1")
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
str_replace(fruits, c("a", "e", "i"), "-")

#### Detecting

#Functions also exist for word detection:

fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "e")
fruit[str_detect(fruit, "e")]
str_detect(fruit, "^a")
str_detect(fruit, "a$")
str_detect(fruit, "b")
str_detect(fruit, "[aeiou]")

# Also vectorised over pattern
str_detect("aecfg", letters)

#We can override the default regular expression matching using wrapper functions.  See the difference in behaviour:

str_detect(strings, fixed(pattern))
str_detect(strings, coll(pattern))

#### Segmentation

#We can also segment words by their boundary definitions, which is part of the Unicode definition.  **quanteda** relies heavily on this for _tokenization_, which is the segmentation of texts into sub-units (normally, terms).

# Word boundaries
words <- c("These are   some words.")
str_count(words, boundary("word"))
str_split(words, " ")[[1]]
str_split(words, boundary("word"))[[1]]

