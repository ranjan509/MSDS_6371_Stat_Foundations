library(tidyverse)
library(dplyr)
library(car)
library(readr)
library(readxl)
library(knitr)

library(ggplot2)
library(ggpubr)
library(ggcorrplot)

library(stringr)
library(multcomp)
library(pairwiseCI)
library(effectsize)
library(investr)
library(broom)
library(olsrr)

setwd("D:/School/GitHub/MSDS_6371_Stat_Foundations/FinalProject")

FullData = read.csv("Data/train.csv", na.strings = "NA", strip.white = T)

Clean_FullData = FullData

Submission_TestData  = read.csv("Data/test.csv", na.strings = "NA", strip.white = T)

####################################### Data Cleaning

summary(Clean_FullData)

# Test if na.strings worked
# There are 81 NA's in the column BM "GarageCond" according to Excel

sum(is.na(Clean_FullData$GarageCond))

# R counted 81 na's, therefore it worked. 

# This is a loop that will go through the columns and decide 
# whether to delete the column based off of the amount of na's. Then it will 
# go through each row and eleminate the row if it has more than 1 na. 

yCol = 0
yRow = 0

for (i in 1:length(Clean_FullData)){
  x = sum(is.na(Clean_FullData[,i])) / length(Clean_FullData[,i])
  if (x > 0.3150685){ # 1460 - (.3150685 * 1460) = 1000
    yCol = append(yCol,i)
  }
}
yCol
length(yCol)
Clean_FullData = subset(Clean_FullData, select = -yCol)


for (i in 1:nrow(Clean_FullData)){
  x = as.numeric(rowSums(is.na(Clean_FullData[i,])))
  if (x > 0){
    yRow = append(yRow,i)
  }
}
yRow
length(yRow)
Clean_FullData = Clean_FullData[-yRow,]

# Now that the dataset is cleaned

rm(i,x,yCol,yRow)

summary(Clean_FullData)
dim(Clean_FullData)

# We can see that there are 76 variables, and 1094 rows, where each row has
# complete information on a house. This sample is more than plenty to configure
# a model. 

####################################### Data Selection // Analysis Question 1

pattern = c("NAmes","Edwards","BrkSide")

AQ1_Data = Clean_FullData %>% dplyr::select(Neighborhood,GrLivArea,SalePrice) %>% filter(grepl(paste(pattern, collapse="|"), Neighborhood))

AQ1_Data$Neighborhood = as.factor(AQ1_Data$Neighborhood)

summary(AQ1_Data)






























































































































































































































































































































































































































