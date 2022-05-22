
#----------- Installing required packages -----------
if (!require("janitor")) {
  install.packages("janitor")    
  library(janitor)              
}
if (!require("skimr")) {
  install.packages("skimr")    
  library(skimr)              
}
if (!require("dplyr")) {
  install.packages("dplyr")    
  library(dplyr)              
}
if (!require("caret")) {
  install.packages("caret")    
  library(caret)              
}
#----------- loading and preparing the data set -----------
data_name = "Mobile_Price.csv"
data <- read.csv(data_name, header= TRUE)
#using the janitor package and converting all names to lowercase.
data<- data %>% janitor::clean_names()
#----------- exploring the data -----------
names(data)
summary(data)
str(data)
skim(data)
#----------- dealing with missing values -----------
#droping rows with missing values in the target columns
data<- data[!is.na(data$dual_sim),]
skim(data)
#----------- NORMALIZATION (IF NEEDED!) -----------
normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}
#remove non-numeric or binary column before processing
data_numeric<-data[,7:20]
data_numeric_normal <- as.data.frame(lapply(data_numeric,normalise))
#----------- identifying the outliers -----------
par(mar = c(15, 4, 4, 2) + 0.1)
boxplot(data_numeric, las = 2)
boxplot(data_numeric_normal, las = 2)
#----------- exporting the data set -----------
data[,7:20] <- data_numeric
write.csv(data,"Mobile_Price_Ready.csv", row.names = FALSE)
data[,7:20] <- data_numeric_normal
write.csv(data,"Mobile_Price_Normal.csv", row.names = FALSE)
