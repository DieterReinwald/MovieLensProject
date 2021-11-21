#################################################
## Dieter Reinwald
## HarvardX - Data Science - MovieLens Project - Code
## https://github.com/DieterReinwald
## 2021
#################################################

#################################################
#### Preparations ####
#################################################

#################################################
### Load required packages ###
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(timeR)) install.packages("timeR", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(rpart)
library(matrixStats)
library(gam)
library(splines)
library(foreach)
library(timeR)

#################################################
#Create a timer object to track times for different sections
mytimer <- createTimer(verbose = FALSE)

#################################################
#### Import data ####
#################################################

# Start timer
mytimer$start("Import data")

#################################################
# Create edx set, validation set (final hold-out test set) 
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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

#Stop timer
mytimer$stop("Import data")

#################################################
#### Preprocess, explore and visualize data  ####
#################################################

#Start timer
mytimer$start("Preprocess, explore, visualize data")

#################################################
#### Preprocessing

# Overview about the structure and content
dim(edx)
summary(edx) %>%  knitr::kable()
head(edx)  %>%  knitr::kable()

# Extract movie year from title and rating year, month, and day from variable timestamp
edx <- edx %>% 
   mutate(movie_year = str_extract(title, "\\(\\d{4}\\)"), # detect parenthesis and year pattern
          movie_year = as.integer(str_replace_all(movie_year,"\\(|\\)", "")), # remove parenthesis
          rating_year = as.integer(year(as_datetime(timestamp))), # get year as integer
          rating_month = as.integer(month(as_datetime(timestamp))), #g et month as integer
          rating_day = as.integer(wday(as_datetime(timestamp))), # get day as integer
          movieId = as.integer(movieId)) # convert movieId to integer

validation <- validation %>% 
   mutate(movie_year = str_extract(title, "\\(\\d{4}\\)"), # detect parenthesis and year pattern
          movie_year = as.integer(str_replace_all(movie_year,"\\(|\\)", "")), # remove parenthesis
          rating_year = as.integer(year(as_datetime(timestamp))), # get year as integer 
          rating_month = as.integer(month(as_datetime(timestamp))), # get month as integer
          rating_day = as.integer(wday(as_datetime(timestamp))), # get day as integer
          movieId = as.integer(movieId)) #convert movieId to integer

# Separate genres for each movie
edx_separated <- edx %>% 
   select(rating, genres) %>% 
   separate_rows(genres, sep = "\\|")

#################################################
####  Data exploration and visualization

# Number of distinct users and movies
edx %>% 
   summarise(n_movies = n_distinct(movieId), n_users = n_distinct(userId))

# Overall mean rating and standard deviation
rating_mean <- mean(edx$rating)
rating_sd <- sd(edx$rating)

# Distribution of the movie ratings, including the rating mean
edx %>% 
   ggplot(aes(rating, y = ..prop..)) +
   geom_bar(color = "black") + 
   labs(title = "Distribution of ratings", x = "rating", y = "relative frequency") + 
   scale_x_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
   geom_vline(xintercept = rating_mean, col = "red", lty = 2) #show rating mean in histogram

# Number of movies rated only once
edx %>% 
   group_by(movieId) %>% 
   summarise(count = n()) %>% 
   filter(count == 1) %>% 
   nrow()

# Top 20 most rated movies due to number of ratings
edx %>% 
   group_by(title) %>% 
   summarise(count = n()) %>% 
   arrange(desc(count)) %>% 
   top_n(20, count) %>% 
   ggplot(aes(count, reorder(title, count))) +
   geom_bar(color = "black", stat = "identity") +
   labs(title = "Top 20 most rated movies", y = "movie")

# Distribution of movies rated over time
edx %>% ggplot(aes(rating_year)) +
   geom_histogram(binwidth = 1, color = "black") +
   labs(title="Ratings per year")

# Distribution of movies released over time
edx %>% ggplot(aes(movie_year))+
   geom_histogram(binwidth = 1, color = "black")+
   labs(title="Released movies per year")

# Total movie ratings per genre
edx_separated %>%
   group_by(genres) %>%
   summarise(count = n()) %>%
   mutate(genres = reorder(genres, count)) %>%
   ggplot(aes(genres, count)) +
   geom_bar(color = "black", stat = "identity") +
   labs(title ="Total movie ratings per genre", y = "count", x = "genre") +
   coord_flip() +
   theme(axis.text.y = element_text(size = 8)) +
   scale_y_continuous(breaks = c(seq(0, 4000000, 1000000)))

# Distribution of movie ratings per genre
edx_separated %>%
   ggplot(aes(rating)) +
   geom_bar(color = "black") +
   labs(title = "Distribution of ratings per genre", x = "rating", y = "count") +
   scale_x_continuous(breaks = c(seq(1, 5, 1))) +
   scale_y_continuous(breaks = c(seq(0, 1250000, 300000))) +
   facet_wrap(genres ~ .)

# Top 10 most popular movies (best rated, with at least 1000 ratings)
edx %>% 
   group_by(title) %>% 
   summarise(mean_rating = mean(rating), count_rating = n()) %>% 
   filter(count_rating >= 1000) %>% 
   top_n(10, mean_rating) %>% 
   arrange(desc(mean_rating)) %>% 
   mutate(title = reorder(title, mean_rating)) %>% 
   ggplot(aes(title, mean_rating)) + 
   geom_bar(color = "black", stat = "identity") +
   labs(title = "Top 10 most popular movies \n (with >= 1000 ratings)", 
        x = "movie", y = "mean rating") + 
   coord_flip() +
   theme(axis.text.y = element_text(size = 8)) + 
   ylim(0,5)

# Top 10 most unpopular movies (worst rated, with at least 1000 ratings)
edx %>% 
   group_by(title) %>% 
   summarise(mean_rating = mean(rating), count_rating = n()) %>% 
   filter(count_rating >= 1000) %>% 
   top_n(-10, mean_rating) %>% 
   arrange(mean_rating) %>% 
   mutate(title = reorder(title, -mean_rating)) %>% 
   ggplot(aes(title, mean_rating)) + 
   geom_bar(color = "black", stat = "identity") +
   labs(title = "Top 10 most unpopular movies \n (with >= 1000 ratings)", 
        x = "movie", y = "mean rating") + 
   coord_flip() +
   theme(axis.text.y = element_text(size = 8)) +
   ylim(0,5)

# MovieId histogram
edx %>% 
   ggplot(aes(movieId)) + 
   geom_histogram(binwidth =  1, color = "black")  + 
   labs(title = "movieId")  

# UserId histogram
edx %>% 
   ggplot(aes(userId)) + 
   geom_histogram(binwidth =  1, color = "black")  + 
   labs(title = "userId") 

# Number of ratings per week
edx %>% 
   mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week")) %>% 
   group_by(week) %>%
   summarize(rating = mean(rating)) %>%
   ggplot(aes(week, rating)) +
   geom_point() +
   geom_smooth() +
   labs(title = "Number of ratings per week", x = "week", y = "rating")

# Correlation analysis
d <- data.frame(userId = edx$userId,
                movieId = edx$movieId,
                rating = edx$rating,
                timestamp = edx$timestamp,
                movie_year = edx$movie_year,
                rating_year = edx$rating_year,
                rating_month = edx$rating_month,
                rating_day = edx$rating_day)

corrplot(cor(d), method = "number")

# Remove data frame, save space
rm(d)

#Stop timer
mytimer$stop("Preprocess, explore, visualize data")

#################################################
#### Findings ####
#################################################

#Start timer
mytimer$start("Findings")

# Ratings over movieId
edx %>% 
   group_by(movieId) %>%
   summarize(n = n(), mean = mean(rating), se= sd(rating)) %>%
   ggplot(aes(movieId, mean, ymin = mean - se, ymax = mean + se)) +
   geom_point() + 
   labs(title="movieId", y="rating") +
   ylim(0,5) 

# Ratings over userId
edx %>% 
   group_by(userId) %>%
   summarize(n = n(), mean = mean(rating), se= sd(rating)) %>%
   ggplot(aes(userId, mean, ymin = mean - se, ymax = mean + se)) +
   geom_point() + 
   labs(title="userId", y="rating") +
   ylim(0,5) 

# Ratings over genres
edx_separated %>% 
   group_by(genres) %>%
   summarize(n = n(), mean = mean(rating), se= sd(rating)) %>%
   ggplot(aes(genres, mean, ymin = mean - se, ymax = mean + se)) +
   geom_point() + 
   geom_errorbar(aes(alpha=0.3), show.legend = FALSE) +
   labs(title="genres", y="rating") +
   ylim(0,5) + 
   theme(axis.text.x = element_text(angle = 90,hjust = 1))

# Remove object edx_separated since not needed anymore, save space
rm(edx_separated)

# Ratings over movie_year
edx %>% 
   group_by(movie_year) %>%
   summarize(n = n(), mean = mean(rating), se= sd(rating)) %>%
   ggplot(aes(movie_year, mean, ymin = mean - se, ymax = mean + se)) +
   geom_point() + 
   geom_errorbar(aes(alpha=0.3), show.legend = FALSE) +
   geom_smooth() +
   labs(title="movie_year", y="rating") +
   ylim(0,5)

# Ratings over rating_year 
edx %>% 
   group_by(rating_year) %>%
   summarize(n = n(), mean = mean(rating), se = sd(rating)) %>%
   ggplot(aes(rating_year, mean, ymin = mean - se, ymax = mean + se)) +
   geom_point() +
   geom_errorbar(aes(alpha=0.3), show.legend = FALSE) + 
   geom_smooth() +
   labs(title = "rating_year", y="rating") +
   ylim(0,5)

# Ratings over rating_month 
edx %>% 
   group_by(rating_month )%>%
   summarize(n = n(), mean = mean(rating), se = sd(rating)) %>%
   ggplot(aes(rating_month, mean, ymin = mean - se, ymax = mean + se)) +
   geom_point() +
   geom_errorbar(aes(alpha=0.3), show.legend = FALSE) + 
   geom_smooth() +
   labs(title = "rating_month", y="rating") +
   ylim(0,5)

# Ratings over rating_day
edx %>% 
   group_by(rating_day )%>%
   summarize(n = n(), mean = mean(rating), se = sd(rating)) %>%
   ggplot(aes(rating_day, mean, ymin = mean - se, ymax = mean + se)) +
   geom_point() +
   geom_errorbar(aes(alpha=0.3), show.legend = FALSE) + 
   geom_smooth() +
   labs(title = "rating_day", y="rating") +
   ylim(0,5)

#Stop timer
mytimer$stop("Findings")

#################################################
#### Modeling approach ####
#################################################

#Start timer
mytimer$start("Modeling approach")

#################################################
## Reduce the data set by unnecessary variables
# Remove variable timestamp, rating_month, and ratinng_day to reduce memory space
edx <- edx %>% select(-timestamp, -rating_month, -rating_day)
validation <- validation %>% select(-timestamp, -rating_month, -rating_day)

#################################################
# Create train and test set
set.seed(1, sample.kind="Rounding")  
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]

# Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- edx_test %>%
   semi_join(edx_train, by = "movieId") %>%
   semi_join(edx_train, by = "userId")

#################################################
## Define the loss function RMSE
# Define RMSE (i.e. the loss function)
RMSE <- function(true_ratings, predicted_ratings) {
   sqrt(mean((true_ratings - predicted_ratings)^2))}

# Define data frame for results
rmse_results <- data.frame(modelid = integer(), 
                           method = character(), 
                           RMSE = numeric(),
                           delta_model_target = numeric())

# Set the target
target_value <- 0.86490

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 0,
                                     method = "target",
                                     RMSE = target_value,
                                     delta_model_target = 0))

rmse_results %>% knitr::kable()

# Define mean edx_train
mu <- mean(edx_train$rating)

#################################################
## Create the result data frame and target value
# Define data frame for results
rmse_results <- data.frame(modelid = integer(), 
                           method = character(), 
                           RMSE = numeric(),
                           delta_model_target = numeric())

# Set the target
target_value <- 0.86490

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 0,
                                     method = "target",
                                     RMSE = target_value,
                                     delta_model_target = 0))

rmse_results %>% knitr::kable()

#Stop timer
mytimer$stop("Modeling approach")

#################################################
#### Train and compare different models ####
#################################################

#Start timer
mytimer$start("Train and compare different models")

#################################################
# Model 1 - Average
rmse_model_1 <- RMSE(edx_test$rating, mu)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 1,
                                     method = "average",
                                     RMSE = rmse_model_1,
                                     delta_model_target = rmse_model_1 - target_value))

rmse_results %>% knitr::kable()

#################################################
# Model 2 - Linear model with movieId
set.seed(1, sample.kind="Rounding")  

movie_avgs  <- edx_train %>% 
   group_by(movieId) %>% 
   summarise(b_i = mean(rating - mu))

predicted_ratings <- mu + edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   pull(b_i)

rmse_model_2 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 2,
                                     method = "linear model (movieId)",
                                     RMSE = rmse_model_2,
                                     delta_model_target = rmse_model_2 - target_value))

rmse_results %>% knitr::kable()

#################################################
# Model 3 - Linear model with userId}
set.seed(1, sample.kind="Rounding")  

user_avgs <- edx_train %>% 
   group_by(userId) %>% 
   summarise(b_u = mean(rating - mu))

predicted_ratings <- mu + edx_test %>% 
   left_join(user_avgs, by = "userId") %>% 
   pull(b_u)

rmse_model_3 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 3,
                                     method = "linear model (userId)",
                                     RMSE = rmse_model_3,
                                     delta_model_target = rmse_model_3 - target_value))

rmse_results %>% knitr::kable()

#################################################
# Model 4 - Linear model with movieId + userId
set.seed(1, sample.kind="Rounding") 

user_avgs  <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(userId) %>% 
   summarise(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   mutate(pred = mu + b_i + b_u) %>% 
   pull(pred)

rmse_model_4 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 4,
                                     method = "linear model (movieId + userId)",
                                     RMSE = rmse_model_4,
                                     delta_model_target = rmse_model_4 - target_value))                                     

rmse_results %>% knitr::kable()

#################################################
# Model 5 - Linear model with movieId + rating_year
set.seed(1, sample.kind="Rounding")  

rating_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(rating_year) %>% 
   summarise(b_rty = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(rating_year_avgs, by = "rating_year") %>% 
   mutate(pred = mu + b_i + b_rty) %>% 
   pull(pred)

rmse_model_5 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 5,
                                     method = "linear model (movieId + rating_year)",
                                     RMSE = rmse_model_5,
                                     delta_model_target = rmse_model_5 - target_value))                                     

#################################################
# Model 6 - Linear model with movieID + movie_year
set.seed(1, sample.kind="Rounding")  

movie_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(movie_year) %>% 
   summarise(b_my = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(movie_year_avgs, by = "movie_year") %>% 
   mutate(pred = mu + b_i + b_my) %>% 
   pull(pred)

rmse_model_6 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 6,
                                     method = "linear model (movieId + movie_year)",
                                     RMSE = rmse_model_6,
                                     delta_model_target = rmse_model_6 - target_value))                                     

#################################################
# Model 7 - Linear model with movieID + genres
set.seed(1, sample.kind="Rounding")  

genres_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(genres) %>% 
   summarise(b_g = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   mutate(pred = mu + b_i + b_g) %>% 
   pull(pred)

rmse_model_7 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 7,
                                     method = "linear model (movieId + genres)",
                                     RMSE = rmse_model_7,
                                     delta_model_target = rmse_model_7 - target_value))                                     

rmse_results %>% knitr::kable()

#################################################
# Model 8 - Linear model with userId + rating_year
set.seed(1, sample.kind="Rounding")  

user_avgs <- edx_train %>% 
   group_by(userId) %>% 
   summarise(b_u = mean(rating - mu))

rating_year_avgs <- edx_train %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(rating_year) %>% 
   summarise(b_rty = mean(rating - mu - b_u))

predicted_ratings <- edx_test %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(rating_year_avgs, by = "rating_year") %>% 
   mutate(pred = mu + b_u + b_rty) %>% 
   pull(pred)

rmse_model_8 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 8,
                                     method = "linear model (userId + rating_year)",
                                     RMSE = rmse_model_8,
                                     delta_model_target = rmse_model_8 - target_value))                                     

#################################################
# Model 9 - Linear model with userID + movie_year
movie_year_avgs <- edx_train %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(movie_year) %>% 
   summarise(b_my = mean(rating - mu - b_u))

predicted_ratings <- edx_test %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(movie_year_avgs, by = "movie_year") %>% 
   mutate(pred = mu + b_u + b_my) %>% 
   pull(pred)

rmse_model_9 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 9,
                                     method = "linear model (userId + movie_year)",
                                     RMSE = rmse_model_9,
                                     delta_model_target = rmse_model_9 - target_value))                                     

#################################################
# Model 10 - Linear model with userID + genres
genres_avgs <- edx_train %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(genres) %>% 
   summarise(b_g = mean(rating - mu - b_u))

predicted_ratings <- edx_test %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   mutate(pred = mu + b_u + b_g) %>% 
   pull(pred)

rmse_model_10 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 10,
                                     method = "linear model (userId + genres)",
                                     RMSE = rmse_model_10,
                                     delta_model_target = rmse_model_10 - target_value))                                     

rmse_results %>% knitr::kable()

#################################################
# Model 11 - Linear model with movieId, userId, and rating_year
set.seed(1, sample.kind="Rounding")  

user_avgs  <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(userId) %>% 
   summarise(b_u = mean(rating - mu - b_i))

rating_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(rating_year) %>% 
   summarise(b_rty = mean(rating - mu - b_i - b_u))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(rating_year_avgs, by = "rating_year") %>% 
   mutate(pred = mu + b_i + b_u + b_rty) %>% 
   pull(pred)

rmse_model_11 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 11,
                                     method = "linear model (movieId + userId + rating_year)",
                                     RMSE = rmse_model_11,
                                     delta_model_target = rmse_model_11 - target_value))                                     

#################################################
# Model 12 - Linear model with movieId, userId, and movie_year
set.seed(1, sample.kind="Rounding")  

movie_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(movie_year) %>% 
   summarise(b_my = mean(rating - mu - b_i - b_u))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(movie_year_avgs, by = "movie_year") %>% 
   mutate(pred = mu + b_i + b_u + b_my) %>% 
   pull(pred)

rmse_model_12 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 12,
                                     method = "linear model (movieId + userId + movie_year)",
                                     RMSE = rmse_model_12,
                                     delta_model_target = rmse_model_12 - target_value))                                     


#################################################
# Model 13 - Linear model with movieId, userId, and genres
set.seed(1, sample.kind="Rounding")  

genres_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(genres) %>% 
   summarise(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   mutate(pred = mu + b_i + b_u + b_g) %>% 
   pull(pred)

rmse_model_13 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 13,
                                     method = "linear model (movieId + userId + genres)",
                                     RMSE = rmse_model_13,
                                     delta_model_target = rmse_model_13 - target_value))                                     

rmse_results %>% knitr::kable()

#Best model index so far
best_model_index <- which.min(rmse_results$delta_model_target) 
best_modelid <- rmse_results$modelid[best_model_index]
best_method <- rmse_results$method[best_model_index]
best_delta_model_target <- rmse_results$delta_model_target[best_model_index]

#################################################
# Model 14 - Linear model with movieId, userId, genres, and rating_year
set.seed(1, sample.kind="Rounding")

rating_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   group_by(rating_year) %>% 
   summarise(b_rty = mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   left_join(rating_year_avgs, by = "rating_year") %>% 
   mutate(pred = mu + b_i + b_u + b_g + b_rty) %>% 
   pull(pred)

rmse_model_14 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 14,
                                     method = "linear model (movieId + userId + genres + rating_year)",
                                     RMSE = rmse_model_14,
                                     delta_model_target = rmse_model_14 - target_value))                                     

#################################################
# Model 15 - Linear model with movieId, userId, genres, and movie_year
set.seed(1, sample.kind="Rounding")  

movie_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   group_by(movie_year) %>% 
   summarise(b_my = mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   left_join(movie_year_avgs, by = "movie_year") %>% 
   mutate(pred = mu + b_i + b_u + b_g  + b_my) %>% 
   pull(pred)

rmse_model_15 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 15,
                                     method = "linear model (movieId + userId + genres + movie_year)",
                                     RMSE = rmse_model_15,
                                     delta_model_target = rmse_model_15 - target_value))                                     

rmse_results %>% knitr::kable()

#Best model index so far
best_model_index <- which.min(rmse_results$delta_model_target) 
best_modelid <- rmse_results$modelid[best_model_index]
best_method <- rmse_results$method[best_model_index]
best_rmse <- rmse_results$RMSE[best_model_index]
best_delta_model_target <- rmse_results$delta_model_target[best_model_index]

#################################################
# Model 16 - Linear model with movieId, userId, genres, movie_year, and rating_year
set.seed(1, sample.kind="Rounding")  

rating_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   left_join(movie_year_avgs, by = "movie_year") %>% 
   group_by(rating_year) %>% 
   summarise(b_rty = mean(rating - mu - b_i - b_u - b_g - b_my))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   left_join(genres_avgs, by = "genres") %>% 
   left_join(movie_year_avgs, by = "movie_year") %>% 
   left_join(rating_year_avgs, by = "rating_year") %>% 
   mutate(pred = mu + b_i + b_u + b_g  + b_my + b_rty) %>% 
   pull(pred)

rmse_model_16 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 16,
                                     method = "linear model (movieId + userId + genres + movie_year + rating_year)",
                                     RMSE = rmse_model_16,
                                     delta_model_target = rmse_model_16 - target_value))                                     

rmse_results %>% knitr::kable()

#Best model index so far
best_model_index <- which.min(rmse_results$delta_model_target) 
best_modelid <- rmse_results$modelid[best_model_index]
best_rmse <- rmse_results$RMSE[best_model_index]
best_delta_model_target <- rmse_results$delta_model_target[best_model_index]

#################################################
# Model 17 - Regularized linear model  with movieId, userId, genres, rating_year, and movie_year 

set.seed(1, sample.kind = "Rounding")

# Lambda is the tuning parameter
lambdas <- seq(1, 6, 0.25)

reg_rmses <- sapply(lambdas, function(lambda){
   mu <- mean(edx_train$rating)
   
   b_i <- edx_train %>% #penalize movieId with few n()
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+lambda))
   
   b_u <- edx_train %>% #penalize userId with few n()
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))
   
   b_g <- edx_train %>% ###penalize genres with few n()
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%
      group_by(genres)%>%
      summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda))
   
   b_my <- edx_train %>% ###penalize movie_years with few n()
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      left_join(b_g, by='genres')%>%
      group_by(movie_year) %>%
      summarize(b_my = mean(rating - mu - b_i - b_u - b_g)/(n()+lambda))
   
   b_rty <- edx_train %>% ###penalize rating_vears with few n()
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      left_join(b_g, by='genres')%>%
      left_join(b_my, by='movie_year')%>%
      group_by(rating_year) %>%
      summarize(b_rty = mean(rating - mu - b_i - b_u - b_g - b_my)/(n()+lambda))
   
   predicted_ratings <- edx_test %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      left_join(b_g, by = "genres") %>%
      left_join(b_my, by = "movie_year")%>%
      left_join(b_rty, by = "rating_year")%>%
      mutate(pred = mu + b_i + b_u + b_g + b_my + b_rty) %>%
      pull(pred)
   
   return(RMSE(edx_test$rating, predicted_ratings))
})

# Plot regularized rmses vs lambdas to select the optimal lambda                                                        
qplot(lambdas, reg_rmses)  

# The optimal lambda                                                             
lambda <- lambdas[which.min(reg_rmses)]

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 17,
                                     method = "Regularized linear model 16 (movieId + userId + genres + movie_year + rating_year)",
                                     RMSE = min(reg_rmses),
                                     delta_model_target = min(reg_rmses) - target_value))                                     

rmse_results %>% knitr::kable()

#################################################
# Model 18 - generalized linear model (GLM)
train_glm <- train(rating ~ movieId + userId, method = "glm", data = edx_train)

model18_predict <- predict(train_glm, edx_test)

rmse_model_18 <- RMSE(edx_test$rating, model18_predict)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 18,
                                     method = "generalized linear model (movieId + userId)",
                                     RMSE = rmse_model_18,
                                     delta_model_target = rmse_model_18 - target_value))                                     

rmse_results %>% knitr::kable()

#################################################
# Model 19 - local weighted regression (LOESS)
train_loess <- train(rating ~ movieId + userId, method = "gamLoess", data = edx_train)

model19_predict <- predict(train_loess, edx_test)

rmse_model_19 <- RMSE(edx_test$rating, model19_predict)

rmse_results <- bind_rows(rmse_results,
                          data.frame(modelid = 19,
                                     method = "gamLOESS",
                                     RMSE = rmse_model_19,
                                     delta_model_target = rmse_model_19 - target_value))                                     

rmse_results %>% knitr::kable()

#Best model index finally
best_model_index <- which.min(rmse_results$delta_model_target) 
best_modelid <- rmse_results$modelid[best_model_index]
best_method <- rmse_results$method[best_model_index]

#Stop timer
mytimer$stop("Train and compare different models")

#################################################
#### Results
#################################################

#Start timer
mytimer$start("Results")

#################################################
# Calculate with best lambda based on cross-validation
lambda <- 4.75

b_i <- edx_train %>% #penalize movieId with few n()
   group_by(movieId) %>%
   summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx_train %>% #penalize userId with few n()
   left_join(b_i, by="movieId") %>%
   group_by(userId) %>%
   summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_g <- edx_train %>% ###penalize genres with few n()
   left_join(b_i, by="movieId") %>%
   left_join(b_u, by="userId") %>%
   group_by(genres)%>%
   summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda))

b_my <- edx_train %>% ###penalize rel_years with few n()
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   left_join(b_g, by='genres')%>%
   group_by(movie_year) %>%
   summarize(b_my = mean(rating - mu - b_i - b_u - b_g)/(n()+lambda))

b_rty <- edx_train %>% ###penalize rel_years with few n()
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   left_join(b_g, by='genres')%>%
   left_join(b_my, by='movie_year')%>%
   group_by(rating_year) %>%
   summarize(b_rty = mean(rating - mu - b_i - b_u - b_g - b_my)/(n()+lambda))

# Validation of the best model based on the validation set ####
final_predict <- 
   validation %>% 
   left_join(b_i, by = "movieId") %>%
   left_join(b_u, by = "userId") %>%
   left_join(b_g, by = "genres") %>%
   left_join(b_my, by ="movie_year")%>%
   left_join(b_rty, by ="rating_year")%>%
   mutate(pred = mu + b_i + b_u + b_g + b_my + b_rty) %>%
   pull(pred)

#Check if there are NA's in the predictions
summary(final_predict) 

# Get titles of movies with no rating
validation %>% filter(is.na(final_predict)) %>% pull(title, rating) %>% knitr::kable()

# Replace NA's with average rating
final_predict <- replace(final_predict, is.na(final_predict), mu) 

# Calculate the RMSE for the final model based on validation data set
final_model_rmse <- RMSE(validation$rating, final_predict)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 20, #"final",
                                     method = "validation (model 17, regularized - movieId + userId + genres + movie_year + rating_year",
                                     RMSE = final_model_rmse, 
                                     delta_model_target = final_model_rmse - target_value))                                     

rmse_results %>% knitr::kable()

#Stop timer
mytimer$stop("Results")

#Get timer summary
getTimer(mytimer) %>%  
   mutate(timeElapsed_min = round(as.numeric(timeElapsed/60),2)) %>%
   select(event, start, end, timeElapsed_min) %>% 
   knitr::kable()