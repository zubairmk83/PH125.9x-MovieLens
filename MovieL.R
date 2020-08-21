#########################
#title: "MovieLens"     #
#author: "Zubair Khan"  #
#date: "15-Aug-2020"    #
#########################

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
 #                                          title = as.character(title),
  #                                         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
#listing all objects 
ls()
dim(edx)
dim(validation)


###Libraries#####################################################

#Working with edx library
if(!require(anytime)) install.packages("ggplot2")
if(!require(anytime)) install.packages("anytime")
if(!require(anytime)) install.packages("sjmisc")
if(!require(anytime)) install.packages("scales")
if(!require(anytime)) install.packages("dplyr") 

library(ggplot2)
library(anytime)
library(sjmisc)
library(scales)
library(dplyr)
library(ggplot2)
library(formattable)
################################################################


###Data Verification############################################
#Reviewing the edx dataset
formattable(head(edx), n=5, caption = "Edx first 5 rows Dataset")
#Reviewing summary of the edx dataset
#Title and genres are both character fields. 
summary(edx)

#Reviewing the remaining attributes. userId, movieId and rating variables can all be 
str(edx)
###################################################################


###Filtering ######################################################
#RelYear <- substr(edx$title, nchar(edx$title)-4,nchar(edx$title)-1) #Extract release year of Title
#unique(RelYear) #Visualy verify if all values are in proper format
#year <- as.factor(RelYear)

#working with genres column
#find out how max count of Genres for movie by counting occurance of "|"
max(str_count(edx$genres,"\\|")) # 
#Looking at genre's that contain more then 3 genre's in category. May not use this in actual project. 
edx %>% filter(str_count(edx$genres,"\\|") >= 4) %>% group_by(movieId) %>% unique() %>% 
  head(n=5) %>% select(title, genres) %>% formattable()

GCategory <- max(str_count(edx$genres,"\\|")) +1 

#Dont know if i will use this
Glist <- edx %>% filter(str_count(edx$genres,"\\|") >= 4) %>% group_by(movieId) %>% select(title) %>% unique() 


edx_filter <- edx %>% separate_rows(genres, sep = "\\|")
g <- unique(edx_filter$genres) #total of 20 categories of Genres

####################################################################


##Genres in descending order########################################
edx_filter %>% group_by(genres) %>% summarize(count = n()) %>%
  arrange(desc(count)) %>% formattable()


length(g)
####################################################################


##building the data martix for genres###############################
edx_final <- edx %>% select(-genres,-timestamp)
#load the below code in a sequence. wait for 1st code to complete before executing the next one or entire dataset may not be populated
edx_final$Drama <- str_detect(edx$genres, g[6])
edx_final$Comedy <- str_detect(edx$genres, g[1])
edx_final$Action <- str_detect(edx$genres, g[3])
edx_final$Thriller <- str_detect(edx$genres, g[5])
edx_final$Adventure <- str_detect(edx$genres, g[8])
edx_final$Romance <- str_detect(edx$genres, g[2])
edx_final$SciFi <- str_detect(edx$genres, g[7])
edx_final$Crime <- str_detect(edx$genres, g[4])
edx_final$Fantasy <- str_detect(edx$genres, g[10])
edx_final$Children <- str_detect(edx$genres, g[9])
edx_final$Horror <- str_detect(edx$genres, g[17])
edx_final$Mystery <- str_detect(edx$genres, g[15])
edx_final$War <- str_detect(edx$genres, g[11])
edx_final$Animation <- str_detect(edx$genres, g[12])
edx_final$Musical <- str_detect(edx$genres, g[13])
edx_final$Western <- str_detect(edx$genres, g[14])
edx_final$FilmNoir <- str_detect(edx$genres, g[16])
edx_final$Documentary <- str_detect(edx$genres, g[18])
edx_final$IMAX <- str_detect(edx$genres, g[19])
edx_final$NoGenres <- str_detect(edx$genres, g[20])
####################################################################


##Data Conversion###################################################

RelYear <- substr(edx$title, nchar(edx$title)-4,nchar(edx$title)-1) #Extract release year of Title
unique(RelYear) #Visualy verify if all values are in proper format
edx_final$RelYear <- as.factor(RelYear)

Fdate <- anydate(edx$timestamp)
edx_final$Year <- as.factor(format(Fdate, "%Y")) #Creating a column that will contain Year
#edx_final$Month <- as.factor(format(Fdate, "%m")) #Creating a column that will contain month


####################################################################

str(edx_final)

head(edx_final)

##Data Exploration & Anaytics#######################################

#Rating distribution
edxRating <- edx_final %>% group_by(rating) %>% count() #extracting the rating variable to display based on percentage.
edxRating$m <- edxRating$n/sum(edxRating$n)
ggplot(edxRating, aes(x=rating, y=m, label = percent(m))) +
  geom_bar(stat='identity', fill="forest green")+
  geom_text(size = 3, position = position_stack(vjust = .9))+
  ylab("% Count")

#Genre distribution
edxGen <- edx_filter %>% group_by(genres) %>% count() %>% arrange(desc(n)) 
edxGen$m <- edxGen$n/sum(edxGen$n)
ggplot(edxGen, aes(x=genres, y=m, label = percent(m), fill= genres)) +
  geom_bar(stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = .9))+
  ylab("% rating count")+
  theme(axis.text.x=element_blank(), legend.position = "bottom")

#User distribution
edx_filter %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 100, fill = "forest green", color = "black") +  scale_x_log10() + 
  ggtitle("Rating Count by Users")+
  xlab("User Count")+
  ylab("Rating count")


rm(edx_filter)

#Creating a dataframe with 4 variables that will hold genre's that are true in condition
edx_filter <- data.frame(matrix(ncol = 4, nrow = 0, dimnames = list(NULL, c("movieId", "genre", "Year", "ReleaseYear"))))

i <- 5
while (i <=24){
  df <- edx_final %>% filter(edx_final[[i]]==TRUE) %>% 
    mutate(movieId, genre = colnames(edx_final)[i], Year, ReleaseYear = RelYear) %>%
    select(movieId, genre, Year, ReleaseYear)
  edx_filter <- rbind(edx_filter,df)  
  
  i <- i + 1
}

#this is to calculate dynamic breaks in the plot
minimum <- as.integer(as.character(edx_filter$ReleaseYear[which.min(edx_filter$ReleaseYear)]))
maximum <- as.integer(as.character(edx_filter$ReleaseYear[which.max(edx_filter$ReleaseYear)]))


edx_filter %>% group_by(ReleaseYear) %>% count(genre)  %>% 
  ggplot() +
  geom_point(aes(x=ReleaseYear, y=n/sum(n), color = genre))+ 
  scale_x_discrete(breaks = c(seq(minimum,maximum,15)))+ labs(y = "% Count")+
  theme(legend.position = "none" , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~genre)


edx_filter %>% group_by(ReleaseYear) %>% count(genre)  %>% 
  ggplot() +
  geom_point(aes(x=ReleaseYear, y=n/sum(n), color = genre))+ 
  scale_x_discrete(breaks = c(seq(minimum,maximum,7)))+ labs(y = "% Count")+
  theme(legend.position = "bottom" , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

minimum <- as.integer(as.character(edx_filter$Year[which.min(edx_filter$Year)]))
maximum <- as.integer(as.character(edx_filter$Year[which.max(edx_filter$Year)]))



edx_filter %>% group_by(Year) %>% count(genre)  %>% 
  ggplot() + geom_point(aes(x=Year, y=n/sum(n)*100, color = genre)) + 
  scale_x_discrete(breaks = c(seq(minimum,maximum,3)))+ 
  labs(y = "Distribution")+
  theme(legend.position = "none" , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~genre)


edx_final %>% select(rating, Year) %>% arrange(rating) %>% group_by(rating) %>% distinct(rating,Year) %>%
  ggplot() + geom_point(aes(x=rating, y=Year))



edx_final %>%
  group_by(RelYear) %>%
  summarize(n = n(), sd = sd(rating), se = sd/sqrt(n) ,avg = mean(rating)) %>% 
  ggplot (aes(x=RelYear, y=avg, color = avg)) +
    geom_errorbar(aes(ymin=avg-se, ymax=avg+se), position = "dodge") +
  scale_x_discrete(breaks = c(seq(1900,maximum,8))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = mean(edx_final$rating))



####################################################################



##Cleaning Data##################################################### 

edx_final[,5:24] <- NULL
rm(df, edx_filter, edxGen,edxRating)

####### Initially wanted to use Kmean but will not be proceeding with this method as it utilizes alot of compuational resource.
####### I have had random fails and occassionally crashes. A quick google search and few people recommended using 
#
#edx_test <- edx_final %>% select(userId, movieId, RelYear)
#set.seed(20)
#
#kmean_wit <- function(k){
#  cluster <- kmeans(edx_test, k)
#  return(cluster$tot.withinss)
#}
#max_k <- 10
#wss <- sapply(2:max_k, kmean_wit)
#
#kmeans(edx_test, 4)
#plotter <- data.frame(2:max_k,wss)
#ggplot(plotter, aes(x = X2.max_k, y=wss)) +
#  geom_point() +
#  geom_line()
#
#
####################################################################


##RMSE##############################################################

lambda <- seq(0, 3, 0.25)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

  root <- sapply(lambda, function(l){
  mu <- mean(edx_final$rating)
  AdjustG <- edx_final %>% group_by(movieId) %>% summarise(AdjustG = sum(rating - mu)/(n()+l))
  
  AdjustU <- edx_final %>% left_join(AdjustG, by="movieId") %>% group_by(userId) %>% 
    summarise(AdjustU = sum(rating - AdjustG - mu)/(n()+l))
  
  AdjustM <- edx_final %>% left_join(AdjustG, by="movieId") %>% 
    left_join(AdjustU, by="userId") %>% group_by(RelYear) %>% 
    summarise(AdjustM = sum(rating - AdjustG -AdjustU - mu)/(n()+l))
  
  prediction <- edx_final %>% left_join(AdjustG, by = "movieId") %>% left_join(AdjustU, by = "userId") %>%
    left_join(AdjustM, by = "RelYear") %>% 
    mutate(predict = AdjustG + AdjustU +AdjustM + mu) %>% .$predict
  return(RMSE(prediction, edx_final$rating))
})

plot(lambda, root)

min(root)
lambda[which.min(root)]

####################################################################



##Validation Dataset################################################

lambda <- lambda[which.min(root)]

#we need to modify the validation set to include Release year

dim(validation)
head(validation)

RelYear <- substr(validation$title, nchar(validation$title)-4,nchar(validation$title)-1) #Extract release year of Title
validation$RelYear <- as.factor(RelYear)

Fdate <- anydate(validation$timestamp)
validation$Year <- as.factor(format(Fdate, "%Y")) #Creating a column that will contain Year


root <- sapply(lambda, function(l){
  mu <- mean(validation$rating)
  AdjustG <- validation %>% group_by(movieId) %>% summarise(AdjustG = sum(rating - mu)/(n()+l))
  
  AdjustU <- validation %>% left_join(AdjustG, by="movieId") %>% group_by(userId) %>% 
    summarise(AdjustU = sum(rating - AdjustG - mu)/(n()+l))
  
  AdjustM <- validation %>% left_join(AdjustG, by="movieId") %>% 
    left_join(AdjustU, by="userId") %>% group_by(RelYear) %>% 
    summarise(AdjustM = sum(rating - AdjustG -AdjustU - mu)/(n()+l))
  
  prediction <- validation %>% left_join(AdjustG, by = "movieId") %>% left_join(AdjustU, by = "userId") %>%
    left_join(AdjustM, by = "RelYear") %>% 
    mutate(predict = AdjustG + AdjustU +AdjustM + mu) %>% .$predict
  return(RMSE(prediction, validation$rating))
})

plot(lambda, root)

min(root)
lambda[which.min(root)]


write.csv(validation, file = "submission.csv", row.names = FALSE)

####################################################################
