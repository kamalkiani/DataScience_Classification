
#----------- Installing required packages -----------
if (!require("skimr")) {
  install.packages("skimr")    
  library(skimr)              
}
if (!require("party")) {
  install.packages("party")    
  library(party)              
}
if (!require("rpart")) {
  install.packages("rpart")    
  library(rpart)              
}
if (!require("rpart.plot")) {
  install.packages("rpart.plot")    
  library(rpart.plot)              
}
if (!require("rattle")) {
  install.packages("rattle")    
  library(rattle)              
}
if (!require("tidyverse")) {
  install.packages("tidyverse")    
  library(tidyverse)              
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")    
  library(RColorBrewer)              
}
#----------- loading the dataset -----------
data_name = "Mobile_Price_Ready.csv"
data <- read.csv(data_name, header= TRUE)
# convert the specified column into a factor column.
data$price_range <- as.factor(data$price_range)
str(data)
skim(data)
#----------- prepare train and test dataset -----------
set.seed(1234) 
temp <- sample(2, nrow(data),replace=TRUE, prob=c(0.8,0.2))
train <- data[temp==1,] 
test <- data[temp==2,] 
dim(train) 
dim(test)
#----------- training the tree (rpart) -----------
mytree <- rpart(
    price_range ~ 
      battery_power + 
      px_height  +
      ram  , 
  data = train, 
  method = "class", 
  maxdepth = 5, 
  minsplit = 2, 
  minbucket = 1
)
rpart.plot(mytree, box.palette="RdBu", cex=.9)
# prediction and accuracy
# prediction on test data
t_pred = predict(mytree,test,type="class")
test_predict <- table(test$price_range,t_pred)
print(test_predict) 
sum(diag(test_predict))/sum(test_predict)       # accuracy
1-sum(diag(test_predict))/sum(test_predict)     # classification error
# importance of variables
print(mytree1$variable.importance)
df <- data.frame(imp = mytree1$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()
#----------- training the tree (ctree) -----------
my_tree <- ctree(price_range ~ 
                   battery_power + 
                   px_height  +
                   ram  
                 ,data = train) 
plot(my_tree)
#prediction and accuracy
# prediction on test data
test_predict <- table(predict(my_tree, newdata= test), test$price_range)
print(test_predict) 
sum(diag(test_predict))/sum(test_predict)       # accuracy
1-sum(diag(test_predict))/sum(test_predict)     # classification error


