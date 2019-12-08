#################################
# Data Mining Excercise Proyect #
#################################

# A restaurant want to know based on their customer's info (weight, height, birth year)
# if they should build a smokers area in their facilities

##### Data Importing #####
setwd("~/Documents/Maestria/2 Cuatrimestre/Analitica de Negocio/Unidad 4/uci-restaurant-consumer-data/")
user_profile <- read.table("userprofile.csv", header = TRUE, sep = ",")


##### Data Preprocessing #####
library(dplyr)
str(user_profile)
summary(user_profile)

user_profile$smoker[user_profile$smoker == "?"] <- NA
user_profile$drink_level[user_profile$drink_level == "?"] <- NA

# Only customers from 1950 and 2000 (People actually pays)
users_can_pay <- user_profile %>% 
  # Get all the observations (variables)
  select(c("smoker", "weight", "height", "birth_year")) %>% 
  # Only people are likely to pay
  filter(birth_year >= 1980 & birth_year <= 2000) 


##### Data visualization #####
library(ggplot2)

# Distribution of Height and Weight of the original customers
ggplot(data = user_profile, aes(x = height)) + 
  geom_histogram(bins = 40, fill = "lightblue", col = "blue")

ggplot(data = user_profile, aes(x = weight)) + 
  geom_histogram(bins = 40, fill = "lightblue", col = "blue")

# Distribution customers can actually pay
ggplot(data = users_can_pay, aes(x = birth_year)) + 
  geom_histogram(bins = 10, fill = "lightblue", col = "blue")


##### Data prediction #####
library(caTools)

# Creating Train and Test data
BirthYear <- c()
Weight    <- c()
Height    <- c()

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
BirthYear$compare <- as.data.frame(cbind(actual = BirthYear$test$birth_year, predicted = as.integer(BirthYear$result1)))
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

### Boxplot relation between customers who are smoker and their weight, height and birth year

# Original Data
ggplot(data = na.omit(users_can_pay), aes(y = birth_year, x = smoker, fill = smoker)) +
  geom_boxplot()

# Train Data
ggplot(data = na.omit(BirthYear$train), aes(y = birth_year, x = smoker, fill = smoker)) +
  geom_boxplot()

### Boxplot relation between customers who are smoker and their weight

# Original Data
ggplot(data = na.omit(users_can_pay), aes(y = weight, x = smoker, fill = smoker)) +
  geom_boxplot()

# Train Data
ggplot(data = na.omit(Weight$test), aes(y = weight, x = smoker, fill = smoker)) +
  geom_boxplot()


### Boxplot relation between customers who are smoker and their height

# Original Data
ggplot(data = na.omit(users_can_pay), aes(y = height, x = smoker, fill = smoker)) +
  geom_boxplot()

# Train Data
ggplot(data = na.omit(Height$test), aes(y = height, x = smoker, fill = smoker)) +
  geom_boxplot()


