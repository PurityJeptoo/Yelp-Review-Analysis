## Importing the necessary packages
library(dplyr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(magrittr)
## Loading the CSV file
yelp_reviews <- read.csv("yelp_reviews.csv")

## view the first 10 and last 6 rows of my data
head(yelp_reviews,10) ; tail(yelp_reviews)

##checking structure of Data frame and names of the columns
str(yelp_reviews)
names(yelp_reviews)

## Data Cleaning
# Converting to appropriate datatypes
yelp_reviews$date <- as.Date(yelp_reviews$date)
yelp_reviews$stars <- as.factor(yelp_reviews$stars)
str(yelp_reviews)

# Checking for missing values
is.null(yelp_reviews)
any_null_rows <- apply(yelp_reviews, 1, function(row) any(is.null(row)))
sum(any_null_rows)

## making a copy of my data before dropping duplicate rows
yelp_reviews_copy <- yelp_reviews


# Find the duplicated rows
duplicated_rows <- duplicated(yelp_reviews)

# Drop the duplicated rows from the original dataframe
yelp_reviews <- yelp_reviews[!duplicated_rows, ]
num_rows_after <- nrow(yelp_reviews);


### Question 2
## Performing a summary statistics on the following variables
summary_stat=summary(yelp_reviews[c("stars", "review_length", "pos_words", "neg_words", "net_sentiment")])
summary_stat

## Performing Data Visualizations 
## Plotting a bar plot for variable stars
ggplot(yelp_reviews, aes(x = stars)) +
  geom_bar() +
  labs(title = "Distribution of Stars", x = "Stars", y = "Frequency")

## Plotting histogram for review length,positive words,negative words and net sentiment.
par(mfrow = c(2, 2))
options(repr.plot.width = 10, repr.plot.height = 8)  # Adjusting the size as needed

hist(yelp_reviews$review_length, main = "Review Length", cex.axis = 0.8, cex.lab = 1.5)
hist(yelp_reviews$pos_words, main = "Positive Words", cex.axis = 0.8, cex.lab = 1.5)
hist(yelp_reviews$neg_words, main = "Negative Words", cex.axis = 0.8, cex.lab = 1.5)
hist(yelp_reviews$net_sentiment, main = "Net Sentiment", cex.axis = 0.8, cex.lab = 1.5)

## QUESTION 3
## Creating tables for positive and negative words
poswords_counts <- table(yelp_reviews$pos_words);poswords_counts
negwords_counts <- table(yelp_reviews$neg_words);negwords_counts

## plotting the counts of both positive and negative words
plot_word_counts <- function(pos_counts, neg_counts) {
  # Plotting poswords count
  barplot(pos_counts[1:20], main = "Counts of Positive Words per Review [first 20]", xlab = "Number of Positive Words", ylab = "Total Count")
  
  # Plotting negwords counts
  barplot(neg_counts[1:20], main = "Counts of Negative Words per Review [first 20]", xlab = "Number of Negative Words", ylab = " Total Count")
}

##calling our function
plot_word_counts(poswords_counts, negwords_counts)


## question 4
net_counts <- table(yelp_reviews$net_sentiment);net_counts
## barplot for the first 20 entries
net_counts[1:20] %>%
  barplot(main = "Difference between positive and negative words per Review",
  xlab = "Net sentiment value",
  ylab = "TotalCount")

##question 5
hist(yelp_reviews$review_length)## for choice
avglength_per_rating <- tapply(yelp_reviews$review_length, yelp_reviews$stars, median)
avglength_per_rating
## converting it to Data frame
Avglen_rating <- data.frame(Stars = as.factor(names(avglength_per_rating)), Avg_review_Length = avglength_per_rating)
Avglen_rating

## plotting a barplot for average review length per star category
ggplot(Avglen_rating, aes(x = Stars, y = Avg_review_Length)) +
  geom_bar(stat="identity", fill = "grey") +
  labs(x = "Star Category", y = "Average Review Length", title = "Average Review Length per Rating") +
  theme_minimal()

## qUESTION 6
summary(yelp_reviews$votes_useful)

## distribution
histplot <- ggplot(yelp_reviews, aes(x = votes_useful)) +
  geom_histogram(fill = "grey70", color = "grey40", bins = 30) +
  labs(title = "Distribution of Useful Votes", x = "Votes Useful", y = "Frequency")
boxplot <- ggplot(yelp_reviews, aes(x = "", y = votes_useful)) +
  geom_boxplot(fill = "grey70", color = "grey40") +
  labs(title = "Distribution of Useful Votes", x = NULL, y = "Votes Useful")
hist_box <- histplot + boxplot
hist_box

# votesuseful per star category
votesuseful_by_star <- yelp_reviews %>%
  group_by(stars) %>%
  summarise(total_votes_useful = sum(votes_useful));votesuseful_by_star
# Create a box plot
ggplot(data = yelp_reviews, aes(x = stars, y = votes_useful)) +
  geom_boxplot() +
  xlab("Star Rating") +
  ylab("Votes Useful") +
  ggtitle("Relationship between Star Rating and Votes Useful")
## correlation analysis
correlation <- cor(yelp_reviews$review_length,yelp_reviews$votes_useful);correlation
## sampling my data ramdomly
sample_size <- 100000
sampled_data <- yelp_reviews[sample(nrow(yelp_reviews), size = sample_size, replace = FALSE),]

# Plotting the relationship between review length and votes useful
ggplot(data = sampled_data, aes(x = review_length, y = votes_useful)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(x = "Review Length", y = "Votes Useful") +
  ggtitle("Relationship between review length and votes useful")


## reviews per date 
reviewsperday <- yelp_reviews %>%
    group_by(date) %>%
    summarise(review_count = n())

## Line plot showing reviews over time
reviewsperday <- mutate(reviewsperday, date_value = 1:nrow(reviewsperday))
ggplot(reviewsperday, aes(x = date_value, y = review_count)) +
  geom_line() +
  xlab("Date - Assigned Values") +
  ylab("Review Count") +
  ggtitle("Number of Reviews Overtime") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))


## number of reviews for each user id
reviewsperuser <- yelp_reviews %>%
  group_by(user_id) %>%
  summarise(review_count = n())
## total votes for each user(funny,cool,useful)
total_votes <- yelp_reviews %>%
  group_by(user_id) %>%
  summarise(total_votes = sum(votes_total))
## total net sentiment for each user
user_net <- yelp_reviews %>%
  group_by(user_id) %>%
  summarise(net_sentiment = sum(net_sentiment))

## merging the three tables then sorting in descending orders and finally selecting user id with highest Average
best_user <- merge(reviewsperuser, total_votes, by = "user_id")
best_user <- merge(best_user, user_net, by = "user_id")
best_user <- best_user %>% 
  mutate(overall_average = (review_count + total_votes + net_sentiment) / 3) %>%
  arrange(desc(overall_average))
BestUser <- best_user %>% 
  filter(overall_average == max(overall_average)) %>%
  select(user_id);BestUser


## getting useful votes for each business
useful_votes <- yelp_reviews %>%
  group_by(business_id) %>%
  summarise(total_votes = sum(votes_useful))
## getting the total net sentiment for each business
business_sentiment <- yelp_reviews %>%
  group_by(business_id) %>%
  summarise(total_netsentiment = sum(net_sentiment));business_sentiment

## merging the businesses based on useful votes and business sentiment
merged_data <- merge(useful_votes, business_sentiment, by = "business_id")

## the number of times a business received a 5 star rating
higher_rating_count <- yelp_reviews %>%
  filter(stars == 5) %>%
  count(business_id)

## joining best_business and higher rating count then sort in descending order and select the business with the highest overall average.
best_business <- left_join(higher_rating_count, merged_data, by = "business_id") %>%
  mutate(overall_average = (total_votes + total_netsentiment) / 2) %>%
  arrange(desc(overall_average))
BestBusiness <- best_business %>% 
  filter(overall_average == max(overall_average)) %>%
  select(business_id);BestBusiness