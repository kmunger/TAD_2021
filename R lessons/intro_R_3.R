
#title: "Example R task"
#author: "Taken from code prepared by Pablo Barbera, Dan Cervone"
  
#Read in the data from `humansofnewyork.csv` into `R` and perform some sample tasks.

### Sample solutions

getwd()
hony <- read.csv("humansofnewyork.csv", stringsAsFactors=FALSE) # read in data, character vectors treated as character vectors (not factors)
str(hony) # examine its structure


#1. How many status updates have been posted on this page?
  
table(hony$type) # type of facebook post
sum(hony$type == "status")

#2. What is the total number of likes, comments, and shares it received?
  
total.likes <- sum(hony$likes_count)
total.comm <- sum(hony$comments_count)
total.shares <- sum(hony$shares_count)
total.likes + total.comm + total.shares # wow!

#3. What is the content of the post with the highest number of shares?
  
max(hony$shares_count) # maximum num shares
top.post <- which.max(hony$shares_count)
hony$message[top.post]

#4. What was the date in which the first photo was posted?
  
#The dates we have are characters, so we can't sort them. However, we can notice that the rows in our data frame are ordered by date. Thus let's find the first row that includes a photo.

head(hony$created_time) # in order
tail(hony$created_time) # in order
first.photo <- min(which(hony$type == "photo"))
hony$created_time[first.photo] # October 1, 2011

#5. What is the total number of likes that the page has ever received, excluding its most popular post?
  
max.likes <- max(hony$likes_count) # likes on most popular page
sum(hony$likes_count) - max.likes

#6. How many posts have received more than 1,000,000 likes?
  
sum(hony$likes_count > 1000000)

#7. What was the total number of shares received by posts published each year?
  
year <- substr(hony$created_time, 1, 4) # extracts year from date created variable
tapply(hony$shares_count, year, sum) # sum of shares by year

#8. What was the total number of likes received by posts published each month?
  
month <- substr(hony$created_time, 6, 7) # extracts month from date created variable
tapply(hony$likes_count, month, sum) # sum of likes by month

#answer:
#      01       02       03       04       05       06       07       08 
#44752181 37852600 43590604 53458837 53624742 64106134 64144832 74450372 
#09       10       11       12 
#63820255 63112716 54466901 31307333 

