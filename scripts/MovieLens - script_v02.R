#################################################
## Dieter Reinwald
## MovieLens Project - Code
## HarvardX: PH125.9x - Capstone Project
## https://github.com/DieterReinwald
#################################################

#################################################
#### 1. Introduction ####
#################################################

#################################################
### Data load ###

# Load required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(rpart)

# Get data
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                  col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

#################################################
### Preparation of training, test and validation set ###

# Create Movies data frame
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))
 
movielens <- left_join(ratings, movies, by = "movieId")
 
# Add rows removed from validation set back into edx set
 removed <- anti_join(temp, validation)
 edx <- rbind(edx, removed)

# Clean up of not required objects
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#################################################
##### WICHTIG SPÄTER WIEDER ENTFERNEN - nur für Durchlauf
#################################################

#################################################
#### 2. Exploratory data analysis ####
#################################################

#################################################
### Basic analysis
str(edx)
head(edx)

summary(edx)

# Number of distinct users and movies
edx %>% 
   summarise(n_movies = n_distinct(movieId), n_users = n_distinct(userId))

# Number of movies ranked only once
edx %>% 
   group_by(title) %>% 
   summarise(count = n()) %>% 
   filter(count == 1) %>% 
   nrow()

#################################################
### Preprocessing ###

# Extract movie year from title
# Extract rating year, month, and day from column timestamp
edx <- edx %>% 
   mutate(movie_year = str_extract(title, "\\(\\d{4}\\)"), # detect parenthesis and year pattern
          movie_year = as.integer(str_replace_all(movie_year,"\\(|\\)", "")), # remove parenthesis
          rating_year = as.integer(year(as_datetime(timestamp))), 
          rating_month = as.integer(month(as_datetime(timestamp))),
          rating_day = as.integer(wday(as_datetime(timestamp))),
          movieId = as.integer(movieId)) 

validation <- validation %>% 
   mutate(movie_year = str_extract(title, "\\(\\d{4}\\)"), # detect parenthesis and year pattern
          movie_year = as.integer(str_replace_all(movie_year,"\\(|\\)", "")), # remove parenthesis
          rating_year = as.integer(year(as_datetime(timestamp))), 
          rating_month = as.integer(month(as_datetime(timestamp))),
          rating_day = as.integer(wday(as_datetime(timestamp))),
          movieId = as.integer(movieId)) 

# Correlation analysis
d <- data.frame(userId = edx$userId,
                movieId = edx$movieId,
                rating = edx$rating,
                movie_year = edx$movie_year,
                rating_year = edx$rating_year,
                rating_month = edx$rating_month,
                rating_day = edx$rating_day)


# Plot the correlations
corrplot(cor(d), method = "number")


#################################################
### Data visualization ###

# distribution of movies released over time
edx %>% ggplot(aes(movie_year))+
   geom_histogram(binwidth = 1, color = "black")+
   labs(title="Released movies per year")

# distribution of users ratings over time
edx %>% ggplot(aes(rating_year))+
   geom_histogram(binwidth = 1, color = "black")+
   labs(title="Ratings per year")

# distribution of the movie ratings, including the rating mean
rating_mean <- mean(edx$rating)

edx %>% 
   ggplot(aes(rating, y = ..prop..)) +
   geom_bar(color = "black") + 
   labs(title = "Distribution of ratings", x = "rating", y = "relative frequency") + 
   scale_x_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
   geom_vline(xintercept = rating_mean, col = "red", lty = 2)  #show rating mean in histogram

# distribution of movie ratings per genre
# separate genres for each movie first
edx_separated <- edx %>% separate_rows(genres, sep = "\\|")

edx_separated %>% 
   ggplot(aes(rating)) +
   geom_bar(color = "black") + 
   labs(title = "Distribution of ratings per genre", x = "rating", y = "count") + 
   scale_x_continuous(breaks = c(seq(0.5, 5, 0.5))) +
   scale_y_continuous(breaks = c(seq(0, 1250000, 300000))) +
   facet_wrap(genres ~ .)

# distribution of ratings per movie
edx %>% count(movieId) %>% 
   ggplot(aes(n)) +
   geom_histogram(bins = 30, color = "black") +
   scale_x_log10() +
   labs(title = "Number of ratings per movie", x = "number of ratings", y = "number of movies")

# top 20 most ranked movies due to number of rankings
edx %>% 
   group_by(title) %>% 
   summarise(count = n()) %>% 
   arrange(desc(count)) %>% 
   top_n(20, count) %>% 
   ggplot(aes(count, reorder(title, count))) +
   geom_bar(color = "black", stat = "identity") +
   labs(title = "Top 20 most ranked movies", y = "movie")

# total movie ratings per genre
edx_separated %>% 
   group_by(genres) %>% 
   summarise(count = n()) %>% 
   mutate(genres = reorder(genres, count)) %>% 
   ggplot(aes(genres, count)) +
   geom_bar(stat = "identity") +
   labs(title ="Total movie ratings per genre", y = "count", x = "genre") +
   coord_flip() +
   theme(axis.text.y = element_text(size = 8)) +
   scale_y_continuous(breaks = c(seq(0, 4000000, 500000)))

# top 10 most popular movies (best rated, with at least 1000 ratings)
edx %>% 
   group_by(title) %>% 
   summarise(mean_rating = mean(rating), count_rating = n()) %>% 
   filter(count_rating > 1000) %>% 
   top_n(10, mean_rating) %>% 
   arrange(desc(mean_rating)) %>% 
   mutate(title = reorder(title, mean_rating)) %>% 
   ggplot(aes(title, mean_rating)) + 
   geom_bar(stat = "identity") +
   labs(title = "Top 10 most popular movies (with >= 1000 ratings)", 
        x = "movie", y = "mean rating") + 
   coord_flip() +
   theme(axis.text.y = element_text(size = 8))

# top 10 most unpopular movies (worst rated, with at least 1000 ratings)
edx %>% 
   group_by(title) %>% 
   summarise(mean_rating = mean(rating), count_rating = n()) %>% 
   filter(count_rating > 1000) %>% 
   top_n(-10, mean_rating) %>% 
   arrange(mean_rating) %>% 
   mutate(title = reorder(title, -mean_rating)) %>% 
   ggplot(aes(title, mean_rating)) + 
   geom_bar(stat = "identity") +
   labs(title = "Top 10 most unpopular movies (with >= 1000 ratings)", 
        x = "movie", y = "mean rating") + 
   coord_flip() +
   theme(axis.text.y = element_text(size = 8))

# distribution of ratings versus users 
edx %>%
   count(userId) %>%
   ggplot(aes(n)) +
   geom_histogram(bins = 30, color = "black") +
   labs(title = "Distribution of ratings versus users ", x = "movie ratings", y = "users") +
   scale_x_log10() 

# mean ratings given by users (with number of ratings >= 100)
edx %>%
   group_by(userId) %>%
   filter(n() >= 100) %>%
   summarize(mean_rating_per_user = mean(rating)) %>%
   ggplot(aes(mean_rating_per_user)) +
   geom_histogram(bins = 30, color = "black") +
   labs(title = "Mean ratings given by users (>= 100 ratings per user)", 
        x = "Mean rating", y = "Number of users") +
   scale_x_discrete(limits = c(seq(0.5,5,0.5))) 

# number of rankings per week
edx %>% 
   mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week")) %>% 
   group_by(week) %>%
   summarize(rating = mean(rating)) %>%
   ggplot(aes(week, rating)) +
   geom_point() +
   geom_smooth() +
   labs(title = "Number of rankings per week", x = "time", y = "rating")



#################################################
#### 3. Modeling ####
#################################################

#################################################
### create train and test set from edx to keep validation set for final step

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]

# Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- edx_test %>%
   semi_join(edx_train, by = "movieId") %>%
   semi_join(edx_train, by = "userId")


# Define RMSE (i.e. the loss) function #
RMSE <- function(true_ratings, predicted_ratings) {
   sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Define mu as mean of the training ratings
mu <- mean(edx_train$rating)
mu

# Define data frame for results
rmse_results <- data.frame(modelid = character(), 
                           method = character(), 
                           rmse = numeric())

#################################################
### Model approach 1 - average

# Calculate the RMSE value
avg_rmse <- RMSE(edx_test$rating, mu)
avg_rmse

# Fill result data frame
rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 1,
                                     method = "average",
                                     rmse = avg_rmse))

rmse_results %>% knitr::kable()

#################################################
### Model approach 2 - LM with movieId

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

movie_avgs  <- edx_train %>% 
   group_by(movieId) %>% 
   summarise(b_i = mean(rating - mu))

predicted_ratings <- mu + edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   pull(b_i)

rmse_model2 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 2,
                                     method = "linear model (movieId)",
                                     rmse = rmse_model2))

rmse_results %>% knitr::kable()

#################################################
### Model approach 3 - LM with userId

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

user_avgs <- edx_train %>% 
   group_by(userId) %>% 
   summarise(b_u = mean(rating - mu))

predicted_ratings <- mu + edx_test %>% 
   left_join(user_avgs, by = "userId") %>% 
   pull(b_u)

rmse_model3 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 3,
                                     method = "linear model (userId)",
                                     rmse = rmse_model3))

rmse_results %>% knitr::kable()

#################################################
### Model approach 4 - LM with movieId + userId

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

user_avgs  <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(userId) %>% 
   summarise(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   mutate(pred = mu + b_i + b_u) %>% 
   pull(pred)

rmse_model4 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 4,
                                     method = "linear model (movieId + userId)",
                                     rmse = rmse_model4))

rmse_results %>% knitr::kable()

#################################################
### Model approach 5 - LM with userId + genres

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

genres_avgs  <- edx_train %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(genres) %>% 
   summarise(b_g = mean(rating - mu - b_u))

predicted_ratings <- edx_test %>% 
   left_join(genres_avgs, by = "genres") %>% 
   left_join(user_avgs, by = "userId") %>% 
   mutate(pred = mu + b_u + b_g) %>% 
   pull(pred)

rmse_model5 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 5,
                                     method = "linear model (userId +  genres)",
                                     rmse = rmse_model5))

rmse_results %>% knitr::kable()

#################################################
### Model approach 6 - LM with movieId + userId + genres

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

genres_avgs  <- edx_train %>% 
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

rmse_model6 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 6,
                                     method = "linear model (movieId + userId + genres)",
                                     rmse = rmse_model6))

rmse_results %>% knitr::kable()

#################################################
### Model approach 7 - LM with movieId + rating_year

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

movie_avgs <- edx_train %>% 
   group_by(movieId) %>% 
   summarise(b_i = mean(rating - mu))

rating_year_avgs <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(rating_year) %>% 
   summarise(b_rty = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(rating_year_avgs, by = "rating_year") %>% 
   mutate(pred = mu + b_i + b_rty) %>% 
   pull(pred)

rmse_model7 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 7,
                                     method = "linear model (movieId + rating_year)",
                                     rmse = rmse_model7))

rmse_results %>% knitr::kable()

#################################################
### Model approach 8 - LM with movieId + userId + rating_year

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

movie_avgs <- edx_train %>% 
   group_by(movieId) %>% 
   summarise(b_i = mean(rating - mu))

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
   left_join(rating_year_avgs, by = "rating_year") %>% 
   mutate(pred = mu + b_i + b_u + b_rty) %>% 
   pull(pred)

rmse_model8 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 8,
                                     method = "linear model (movieId + userId + rating_year)",
                                     rmse = rmse_model8))

rmse_results %>% knitr::kable()

#################################################
### Model approach 9 - LM with movieId + userId + genres + rating_year

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

movie_avgs <- edx_train %>% 
   group_by(movieId) %>% 
   summarise(b_i = mean(rating - mu))

user_avgs  <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   group_by(userId) %>% 
   summarise(b_u = mean(rating - mu - b_i))

genres_avgs  <- edx_train %>% 
   left_join(movie_avgs, by = "movieId") %>% 
   left_join(user_avgs, by = "userId") %>% 
   group_by(genres) %>% 
   summarise(b_g = mean(rating - mu - b_i - b_u))

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

rmse_model9 <- RMSE(edx_test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 9,
                                     method = "linear model (movieId + userId + genres + rating_year)",
                                     rmse = rmse_model9))

rmse_results %>% knitr::kable()

#################################################
### Model approach 10 - Regularized LM 
# Cross validation to get the best lambda for model 9 "movieId + userId + genres + rating_year" (best model so far)

set.seed(1, sample.kind = "Rounding")

# Lambda is the tuning parameter
lambdas <- seq(1, 8, 0.25)

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
   
   b_rty <- edx_train %>% ###penalize rating_vears with few n()
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      left_join(b_g, by='genres')%>%
      group_by(rating_year) %>%
      summarize(b_rty = mean(rating - mu - b_i - b_u - b_g)/(n()+lambda))
   
   predicted_ratings <- edx_test %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      left_join(b_g, by = "genres") %>%
      left_join(b_rty, by = "rating_year")%>%
      mutate(pred = mu + b_i + b_u + b_g + b_rty) %>%
      pull(pred)
   
   return(RMSE(edx_test$rating, predicted_ratings))
   
})

# Plot regularized rmses vs lambdas to select the optimal lambda                                                        
qplot(lambdas, reg_rmses)  

# The optimal lambda                                                             
lambda <- lambdas[which.min(reg_rmses)]
lambda

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 10,
                                     method = "Regularized linear model 9 (movieId + userId + genres + rating_year)",
                                     rmse = min(reg_rmses)))

rmse_results %>% knitr::kable()

#################################################
### Model approach 11 - GLM

train_glm <- train(rating ~ userId + movieId, method = "glm", data = edx_train)

#ACHTUNG FEHLER mit TYPE
model11_predict <- predict(train_glm, edx_test, type="response")

rmse_model11 <- RMSE(edx_test$rating, model11_predict)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 11,
                                     method = "GLM",
                                     rmse = rmse_model11))

rmse_results %>% knitr::kable()

#################################################
### Model approach 12 - LOESS

########ACHTUNG TESTEN TODO
train_loess <- train(rating ~ movieId, method = "gamLoess", data = edx_train)

model12_predict <- predict(train_loess, edx_test)

rmse_model12 <- RMSE(edx_test$rating, model12_predict)

rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 12,
                                     method = "LOESS",
                                     rmse = rmse_model12))

rmse_results %>% knitr::kable()

#################################################
#### Select the best model ####

rmse_results %>% knitr::kable()

# Best model was model 10 "Regularized linear model 9" with lambda = 4.75
# Calculate with best lambda  based on cross-validation
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

b_rty <- edx_train %>% ###penalize rel_years with few n()
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   left_join(b_g, by='genres')%>%
   group_by(rating_year) %>%
   summarize(b_rty = mean(rating - mu - b_i - b_u - b_g)/(n()+lambda))

#################################################
#### Validation of the best model ####

final_predict <- 
   validation %>% 
   left_join(b_i, by = "movieId") %>%
   left_join(b_u, by = "userId") %>%
   left_join(b_g, by = "genres") %>%
   left_join(b_rty, by ="rating_year")%>%
   mutate(pred = mu + b_i + b_u + b_g + b_rty) %>%
   pull(pred)

# Look for NA´s in the validation data set
summary(final_predict) 

# Get titles of movies with no rating
validation %>% filter(is.na(final_predict)) %>% pull(title)

# Replace NAs with average rating
final_predict <- replace(final_predict, is.na(final_predict), mu)

# Calculate the RMSE for the final model based on validation data set
final_model_rmse <- RMSE(validation$rating, final_predict)

# Get final overview
rmse_results <- bind_rows(rmse_results, 
                          data.frame(modelid = 13, #"final",
                                     method = "validation (model 10, regularized - movieId + userId + genres + rating_year",
                                     rmse = final_model_rmse))

rmse_results %>% knitr::kable()
