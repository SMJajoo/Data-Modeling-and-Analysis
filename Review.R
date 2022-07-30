review  =  read.csv(file="reviews.csv",  header=TRUE,  sep=",",  stringsAsFactors  =  FALSE)
#First, let's take a quick look:
summary(review)

sapply(review,  function(x)  sum(is.na(x)))
pre_proc = review[review$movieGenre!='(no genres listed)',]  #  Remove the records with no genre


install.packages("ggrepel")
library(ggrepel)

review %>% count(movieGenre)

library(ggplot2)

sapply(review,class)
head(review)
summary(review)


# 2.	Use analytics to answer the following questions:
#   
#   a.	How many reviewers have participated?
length(unique(review$userId))

# b. How many movies have been reviewed?
length(unique(review$movieId))
 
# c. Which reviewer has the most reviews? And the least?
user_reviews  =  aggregate(review$movieId,  by=list(review$userId),  FUN=  length)
names(user_reviews) = c("userID","Reviews") 
user_reviews[user_reviews$Reviews == max(user_reviews$Reviews),]$userID

#And the least?
user_reviews[user_reviews$Reviews  ==  min(user_reviews$Reviews),]$userID

# d. Which genre has the most reviews? And the least?
genre_reviews  =  aggregate(pre_proc$movieId,  by=list(pre_proc$movieGenre),  FUN=  length)
names(genre_reviews) = c("movieGenre","Reviews") 
genre_reviews[genre_reviews$Reviews == max(genre_reviews$Reviews),]$movieGenre

#And the least?
genre_reviews[genre_reviews$Reviews  ==  min(genre_reviews$Reviews),]$movieGenre

# e. Which is the oldest movie reviewed? Who reviewed it 
# (if more than one reviewer rated it, return all of them)?
review[which.min(review$movieYear),"movieTitle"]
review[review$movieYear==min(review$movieYear),"userId"]

#f. Which one(s) is (are) the highest-rated genres of the 1990s?
gender90 = review[review$movieYear>=1990 & review$movieYear<=1999 ,]
gender90_rating =  aggregate(gender90$rating,  by=list(gender90$movieGenre),  FUN=mean)
names(gender90_rating) = c("Genre","Rating") 
gender90_rating[gender90_rating$Rating == max(gender90_rating$Rating),]

#g. What is the most reviewed movie? What is its average score?
most_rev=aggregate(x=review$rating,  by=list(review$movieTitle), function(x){c("num"= length(x), "mean"= mean(x))}) 
m=most_rev[order(-most_rev$x[,1]),]
head(m,1)

# 3. Use visualisation to answer the following questions:
#   a. What is the distribution of ratings for all movie genres?

ggplot(review)  +  
  geom_histogram(bins=25,aes(x=rating,  fill=movieGenre))  +
  facet_wrap(~movieGenre)+ 
  ggtitle("Distribution by Genre") 

#add facet wrap for clear view on each movie genre instead of visualizing in only one histogram

# b. What is the distribution of Comedies according to release decade?
ggplot(review[review$movieGenre=="Comedy",],  aes(x  =  movieYear)) +
  geom_histogram(binwidth  = 10,colour="Green4",fill  =  "green") +
  scale_x_continuous(name="movieYear",breaks = seq(1910,2020,10))+
  scale_y_continuous(name="Frequency")+
  ggtitle("Distribution of Comedies by release decade")

# c. How did the average ratings of Comedies, Westerns, Dramas and 
# Horror change between 2000 and 2002?
sub_reviews=subset(review,movieGenre %in% c("Comedies", "Westerns", "Dramas","Horror"))
sub_reviews= sub_reviews[sub_reviews$date>="2000-01-01" & sub_reviews$date<="2002-12-31",]

ggplot(sub_reviews) + 
  geom_line(colour=sub_reviews$movieGenre) +
  scale_y_continuous(name="Ratings")+
  ggtitle("")



install.packages("dplyr")
library(dplyr)
origin = "2000-01-01"
end  =  "2001-12-31"
genres  =  c("Comedy",  "Western","Drama","Horror")
#Split according to dates
review_sub = review[review$date>=origin & review$date<=end,] 
review_sub$date=format(as.Date(review_sub$date), "%Y/%m")
#Split according to genres
review_sub = subset(review_sub, review_sub$movieGenre %in% genres)
#Use aggregate to get avgs ratings
res = aggregate(review_sub$rating,by=
                  list(review_sub$movieGenre,  review_sub$date),  mean)
colnames(res) = c("Genre", "Date", "Avg")
ggplot(res) + geom_line(aes(x=Date, y = Avg, group = Genre, color = Genre)) + 
  scale_y_continuous(limits=c(0,5))  +
  theme(axis.text.x  =  element_text(angle  =  75,  hjust  =  1))  + 
  ggtitle("Changes in Genres from 2000 to 2002")

ggplot(res) + geom_line(aes(x=Date, y = Avg, group = Genre, color = Genre))	+ 
  facet_wrap(~Genre) + ggtitle("Distribution by Genre") +
  theme(axis.text.x  =  element_text(angle  =  90,  hjust  =  1))



#   d.	Do a monthly comparison of the number of Horror movies and Animations of reviews uploaded between two years of your choosing.
#You can select any two years of your choosing for this exercise.
#We will choose from 2001 to 2015, which returns over 62,000 reviews. 
#Out of those, around 5k are either Animation or Horror reviews.

library(dplyr)
#Let's isolate the reviews
origin = "2001-01-01"
end =  "2015-01-01"
review_sub =   review[review$date>=origin   &   review$date<=end & (review$movieGenre=="Animation" | review$movieGenre=="Horror"),] 
#The day is irrelevant, so let's just get the month and year 
review_sub$date=format(as.Date(review_sub$date), "%Y/%m")
#Calculate how many reviews per movieGenre and per date. 
s=summarize(group_by(review_sub, movieGenre,date), tot=n()) 
#Order according to date
s= s[order(s$date),] 
#Let's take a look 
head(s,10)


ggplot(s,aes(x=date, y = tot,group = movieGenre)) + 
  geom_line(aes(color = movieGenre))	+ 
  theme(axis.text.x  =  element_text(angle  =  90,  hjust  =  1))



labels = seq(as.Date(origin), as.Date(end), length.out = 10) 
labels=format(labels, "%Y/%m")

ggplot(s, aes(x=date, y=tot, group=movieGenre)) + 
  geom_line(aes(color=movieGenre)) +
  ggtitle("Monthly frequency of reviews for Animation and Horror Movies") + 
  scale_x_discrete(name="Date",breaks=labels, labels=as.character(labels)) + 
  theme(axis.text.x  =  element_text(angle  =  90,  hjust  =  1))+
  ylab("Total Reviews")



labels = seq(as.Date(origin), as.Date(end), by="month") labels=format(labels, "%Y/%m")
#pick up dates 6 months:
labels  =  labels[seq(1,  length(labels),6)]
ggplot(s, aes(x=date, y=tot, group=movieGenre)) + 
  geom_line(aes(color=movieGenre)) +
  ggtitle("Monthly frequency of reviews for Animation and Horror Movies") + 
  xlab("Date") +
  scale_x_discrete(breaks=labels, labels=as.character(labels)) + 
  ylab("Total Reviews") +
  theme(axis.text.x  =  element_text(angle  =  90,  hjust  =  1))
       
  
