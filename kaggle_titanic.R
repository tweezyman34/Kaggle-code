#Load tidyverse
library(tidyverse)

#Import training data
train <- read.csv("/Users/dylanwilkerson/Downloads/titanic/train.csv")

#replace training dataset NAs with the mean value
train <- train %>%
  mutate(Age = replace_na(Age, mean(Age, na.rm = T)))

#Import test data
test <- read.csv("/Users/dylanwilkerson/Downloads/titanic/test.csv")

#replace test dataset NAs with the mean value
test <- test %>%
  mutate(Age = replace_na(Age, mean(Age, na.rm = T)),
        Fare = replace_na(Fare, mean(Fare, na.rm = T)))

#Create model to predict if a passenger would survive
model <- glm(Survived ~ Pclass + Age + SibSp + Parch + Fare,  family = 'binomial', train,
             na.action = "na.omit")

#Get model summary statistics
summary(model)

#Add survival prediction as "survived"
test <- test %>%
  mutate(survived = round(predict(model, test, type = "response")))

#create the dataset to only have the passengerid and survival prediction
test <- test %>%
  select(PassengerId, survived)

#Get a count of survivors and non-survivors
test %>%
  group_by(survived) %>%
  summarize(count = n())

#Save the dataframe as a CSV
write.csv(test, "/Users/dylanwilkerson/Downloads/titanic\\Titanic Submission.csv", row.names = FALSE)
         