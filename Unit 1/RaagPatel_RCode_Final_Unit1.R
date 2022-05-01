setwd("D:/School/DS6371/Week 1")

library(readr)
library(dplyr)
library(tidyverse)

## LSA

creativity <- read_csv("D:/School/GitHub/MSDS_6371_Stat_Foundations/Unit 1/Creativity.csv")

xbars = creativity %>% group_by(Treatment_S) %>% summarize(mean = mean(Score))
xbarNminusT = xbars[2,2] - xbars[1,2] # observed difference xbarIntrinsic - xbarExtrinsic = 4.14
xbarNminusT

## Homework

SMU = c(34, 1200, 23, 50, 60, 50, 0, 0, 30, 89, 0, 300, 400, 20, 10, 0)
Seattle = c(20, 10, 5, 0, 30, 50, 0, 100, 110, 0, 40, 10, 3, 0)
hist(SMU)
hist(Seattle)

amt = c(34, 1200, 23, 50, 60, 50, 0, 0, 30, 89, 0, 300, 400, 20, 10, 0, 20, 10, 
        5, 0, 30, 50, 0, 100, 110, 0, 40, 10, 3, 0)
univ = c()

univ[1:length(SMU)] = "smu"
univ[(length(SMU)+1):(length(Seattle)+length(SMU))] = "seau"

amtUniv = data.frame(cbind(amt,univ))
summary(amtUniv)

amtUniv$amt = as.numeric(amtUniv$amt)
amtUniv$univ = as.factor(amtUniv$univ)
summary(amtUniv)

#Difference in means
original <- diff(tapply(amt, univ, mean))
mean(amt[univ=="smu"])-mean(amt[univ=="seau"])

#Permutation test
permutation.test <- function(univ, amt, n){
  distribution=c()
  result=0
  for(i in 1:n){
    distribution[i]=diff(by(amt, sample(univ, length(univ), FALSE), mean))
  }
  result=sum(abs(distribution) >= abs(original))/(n)
  return(list(result, distribution))
}

test1 <- permutation.test(univ, amt, 100000)
hist(test1[[2]], breaks=50, col='grey', main="Permutation Distribution", las=1, xlab='')
abline(v=original, lwd=3, col="red")

test1[[1]]


#Compare to t-test
t.test(amt~univ)
