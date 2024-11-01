#Prodigy Infotech DS Task-5


#Analyze traffic accident data to identify patterns related to road conditions,weather and time of day.
#Visualize accident hotspots & contributing factors

#Load the data
accident<-read.csv("C:\\Users\\User\\Documents\\Data science\\Internship\\Prodigy Infotech Internship\\Task-5\\RTA Dataset.csv")

#View the data
View(accident)

#Column names of the dataset
colnames(accident)

#Observing no. of rows & columns of the dataset
dim(accident)

#Checking for missing value
any(is.na(accident))

#Checking for duplicated values
any(duplicated(accident))

#Summary of the dataset
summary(accident)

#Necessay packages
install.packages("sf")
install.packages("leaflet")
install.packages("ggcorrplot")
install.packages("leaflet.extras")

#Load library
library(ggplot2)
library(dplyr)
library(sf) # For spatial data handling
library(leaflet) # For interactive maps
library(ggcorrplot) # For correlation matrix visualization
library(leaflet.extras)

#Count occurrences and create a data frame
accident_severity_counts <- accident %>% 
  count(Accident_severity)

#count dataset
accident_severity_counts

#Plot the distribution of accident severity
ggplot(data = accident, aes(x = Accident_severity)) +
  geom_bar(fill=c("darkblue","lightgreen","brown")) +
  labs(title = "Distribution of Accident severity",
       x = "Categorical Variable",
       y = "Count")

#Removing unnecessary columns
accidents<-accident[,-c(1,10,11,28,29)]

#Showing first six rows of the dataset
accident6<-head(accidents)

#Viewing the dataset
View(accident6)


#Plot accident severity by road surface and weather conditions
ggplot(accidents, aes(x = Road_surface_conditions, fill = Weather_conditions)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Accident_severity) +
  labs(title = "Accident Severity by Road Surface and Weather Conditions", x = "Road Surface Condition", y = "Count") +
  theme(axis.text.x = element_text())

#Plot accident count by time of day and light conditions
ggplot(accidents, aes(x = Light_conditions, fill = Day_of_week)) +
  geom_bar(position = "dodge") +
  labs(title = "Accident Count by Time of Day and Day of the Week", x = "Light Conditions", y = "Count") +
  theme(axis.text.x = element_text())


#Plot accident severity by vehicle movement
ggplot(accidents, aes(x = Vehicle_movement, fill = factor(Accident_severity))) +
  geom_bar(position = "dodge") +
  labs(title = "Accident Severity by Vehicle Movement", x = "Vehicle Movement", y = "Count") +
  theme(axis.text.x = element_text())

#Plot accident by day of week
ggplot(accidents, aes(x = Day_of_week)) +
  geom_bar(fill = c("darkviolet","skyblue","red","yellow","orange","lightgreen","brown")) +
  labs(title = "Accidents by Day of Week",
       x = "Day of Week",
       y = "Count")

#Plot relationship between Age and Driving Experience
ggplot(accidents, aes(x = Driving_experience, y = Age_band_of_driver)) +
  geom_point() +
  labs(title = "Relationship between Age and Driving Experience",
       x = "Driving Experience",
       y = "Age Band")

#Plot Casualty Severity by Vehicle Type
ggplot(accidents, aes(x = Type_of_vehicle, y = Casualty_severity)) +
  geom_boxplot() +
  labs(title = "Casualty Severity by Vehicle Type",
       x = "Vehicle Type",
       y = "Casualty Severity")


#Convert Accident_severity to a binary variable (e.g., 1 for Serious/Fatal, 0 for Slight)
Accident_severity_binary <- ifelse(accidents$Accident_severity=="Slight Injury" , 0, 1)
Accident_severity_binary


#Fit a logistic regression model predicting the accident severity based on factors
logistic_model <- glm(Accident_severity_binary ~ Road_surface_conditions + Weather_conditions + Light_conditions + Vehicle_movement + Day_of_week+Cause_of_accident, 
                      data = accidents, 
                      family = "binomial")


#View the summary of the logistic regression model
summary(logistic_model)

#Make Predictions (probabilities)
predicted_probs <- predict(logistic_model, type = "response")

#Convert probabilities to binary outcomes using a threshold of 0.5
predicted_classes <- ifelse(predicted_probs >= 0.5, 1, 0)

#Calculate the accuracy
accuracy <- mean(predicted_classes == Accident_severity_binary)
accuracy

