#################################
# Data Mining Excercise Project #
#################################

# A restaurant want to know based on their customer's info (weight, height, birth year)
# if they should build a smokers area in their facilities

##### Data Importing #####
setwd("~/Documents/Maestria/2 Cuatrimestre/Analitica de Negocio/Unidad 4/uci-restaurant-consumer-data/smokers-area/")
user_profile <- read.table("userprofile.csv", header = TRUE, sep = ",")

##### Data Preprocessing #####
library(dplyr)

user_profile$smoker[user_profile$smoker == "?"] <- NA

# Only customers from 1950 and 2000 (People actually pays)
users_can_pay <- user_profile %>% 
  # Get all the observations (variables)
  select(c("smoker", "weight", "height", "birth_year")) %>% 
  # Only people are likely to pay
  filter(birth_year >= 1980 & birth_year <= 2000) 

# Data summary
str(users_can_pay)
summary(users_can_pay)

##### Data visualization #####
library(ggplot2)

# 1) Distribution of Height of the original customers
ggplot(data = user_profile, aes(x = height)) + 
  geom_histogram(bins = 40, fill = "lightblue", col = "blue")

# 2) Distribution of Weight of the original customers
ggplot(data = user_profile, aes(x = weight)) + 
  geom_histogram(bins = 40, fill = "lightblue", col = "blue")

# 3) Distribution customers can actually pay based on their age
ggplot(data = users_can_pay, aes(x = birth_year)) + 
  geom_histogram(bins = 10, fill = "lightblue", col = "blue")


# Relation between customer's birth year and weight
ggplot(data = na.omit(user_profile), aes(x = weight, y = birth_year)) +
  geom_point(mapping = aes(color = smoker)) +
  geom_smooth() 

# Relation between customer's birth year and weight by smokers separately
ggplot(data = na.omit(user_profile), aes(x = weight, y = birth_year)) +
  geom_point(mapping = aes(color = smoker)) +
  geom_smooth() +
  facet_wrap(~ smoker, nrow = 1)

# Relation between customer's birth year and height
ggplot(data = na.omit(user_profile), aes(x = height, y = birth_year)) +
  geom_point(mapping = aes(color = smoker)) +
  geom_smooth()

# Relation between customer's birth year and height by smokers separately
ggplot(data = na.omit(user_profile), aes(x = height, y = birth_year)) +
  geom_point(mapping = aes(color = smoker)) +
  geom_smooth() +
  facet_wrap(~ smoker, nrow = 1)


##### Data prediction #####
library(caTools)

# Creating Train and Test data
BirthYear <- c() ; Weight <- c() ; Height <- c()

BirthYear$split <- sample.split(users_can_pay$birth_year, SplitRatio = 0.65)
BirthYear$train <- subset(users_can_pay, BirthYear$split == TRUE)
BirthYear$test  <- subset(users_can_pay, BirthYear$split == FALSE)

Weight$split <- sample.split(users_can_pay$weight, SplitRatio = 0.65)
Weight$train <- subset(users_can_pay, Weight$split == TRUE)
Weight$test  <- subset(users_can_pay, Weight$split == FALSE)

Height$split <- sample.split(users_can_pay$height, SplitRatio = 0.65)
Height$train <- subset(users_can_pay, Height$split == TRUE)
Height$test  <- subset(users_can_pay, Height$split == FALSE)


# Model building for Birth Year
BirthYear$model  <- lm(data = BirthYear$train, birth_year~.)
BirthYear$result <- predict(BirthYear$model, BirthYear$test)
BirthYear$compare <- as.data.frame(cbind(actual = BirthYear$test$birth_year, predicted = as.integer(BirthYear$result)))
BirthYear$error   <- BirthYear$compare$actual - BirthYear$compare$predicted
cbind(BirthYear$compare, BirthYear$error)
BirthYear$rmse <- sqrt(mean(BirthYear$error ^ 2, na.rm = TRUE))
BirthYear$rmse

# Model building for Weight
Weight$model  <- lm(data = Weight$train, weight~.)
Weight$result <- predict(Weight$model, Weight$test)
Weight$compare <- as.data.frame(cbind(actual = Weight$test$weight, predicted = as.integer(Weight$result)))
Weight$error   <- Weight$compare$actual - Weight$compare$predicted
cbind(Weight$compare, Weight$error)
Weight$rmse <- sqrt(mean(Weight$error ^ 2, na.rm = TRUE))
Weight$rmse

# Model building for Height
Height$model  <- lm(data = Height$train, height~.)
Height$result <- predict(Height$model, Height$test)
Height$compare <- as.data.frame(cbind(actual = Height$test$height, predicted = as.integer(Height$result)))
Height$error   <- Height$compare$actual - Height$compare$predicted
cbind(Height$compare, Height$error)
Height$rmse <- sqrt(mean(Height$error ^ 2, na.rm = TRUE))
Height$rmse


### Boxplot relation between customers who are smoker by birth year
library(gridExtra)

# Original Data
BirthYear$boxplot <- ggplot(data = na.omit(users_can_pay), aes(y = birth_year, x = smoker, fill = smoker)) +
  ggtitle("Original") +
  geom_boxplot()

# Test Data
BirthYear$boxplot_test <- ggplot(data = na.omit(BirthYear$test), aes(y = birth_year, x = smoker, fill = smoker)) +
  ggtitle("Test") +
  geom_boxplot()

# Plot both
grid.arrange(BirthYear$boxplot, BirthYear$boxplot_test, nrow = 1)

### Boxplot relation between customers who are smoker by weight

# Original Data
Weight$boxplot <- ggplot(data = na.omit(users_can_pay), aes(y = weight, x = smoker, fill = smoker)) +
  ggtitle("Original") +
  geom_boxplot()

# Test Data
Weight$boxplot_test <- ggplot(data = na.omit(Weight$test), aes(y = weight, x = smoker, fill = smoker)) +
  ggtitle("Test") +
  geom_boxplot()

# Plot both 
grid.arrange(Weight$boxplot, Weight$boxplot_test, nrow = 1)

### Boxplot relation between customers who are smoker by height

# Original Data
Height$boxplot <- ggplot(data = na.omit(users_can_pay), aes(y = height, x = smoker, fill = smoker)) +
  ggtitle("Original") +
  geom_boxplot()

# Test Data
Height$boxplot <- ggplot(data = na.omit(Height$test), aes(y = height, x = smoker, fill = smoker)) +
  ggtitle("Test") +
  geom_boxplot()

# Plot both 
grid.arrange(Weight$boxplot, Weight$boxplot_test, nrow = 1)


