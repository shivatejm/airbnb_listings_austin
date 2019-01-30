---
title: "Austin Airbnb Listings"
author: "Shiva Teja Medichelmala"
date: "December 8, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r installing packages and reading data}
install.packages("tidyverse")
install.packages("ISLR")
install.packages("ggplot2")
install.packages("MASS")
install.packages("class")
install.packages("randomForest")
install.packages("dplyr")
install.packages('leaps')
library(leaps)
library(tidyverse)
library(ISLR)
library(ggplot2)
library(MASS)
library(class)
library(rmarkdown)
library(randomForest)
library(dplyr)
```

```{r fig.width=8, fig.height=8}

## Importing dataset
```{r}

Listings_exp <- read.csv("Listings_exp.csv")

View(Listings_exp)

summary(Listings_exp)

# Coverting to the relevant data types

Listings_exp$id <- as.factor(Listings_exp$id)
table(Listings_exp$id)

Listings_exp$host_id <- as.factor(Listings_exp$host_id)
table(Listings_exp$host_id)

Listings_exp$neighbourhood <- as.factor(Listings_exp$neighbourhood)
table(Listings_exp$neighbourhood)

Listings_exp$room_type <- as.factor(Listings_exp$room_type)

Listings_exp$number_of_reviews <- as.factor(Listings_exp$number_of_reviews)

View(Listings_exp)

```

#### Data Exploration

```{r}
summary(Listings_exp)

Listings_exp %>% filter(price == 10900.0) %>% select(host_name, neighbourhood, room_type, price, availability_365, reviews_per_month, host_since)

# Exploring Prices: 
summary(Listings_exp$price)      # Average price for the rooms is 270.6

sum(Listings_exp$price > 270.6)
(3153/12037)*100                 # 26.19 % listings have above average prices 


plot(Listings_exp$id, Listings_exp$price)

# How many hosts have above average house prices?  

n_distinct(Listings_exp$host_id)  # There are a total of 8769 hosts. 

Listings_Host_Price <- Listings_exp %>% filter(price > 270.6)
View(Listings_Host_Price)
n_distinct(Listings_Host_Price$host_id)     # There are 2633 hosts with above average prices

(2633/8769)*100     # 30 % hosts have above average prices. 

## Checking for missing values
colnames(Listings_exp)[colSums(is.na(Listings_exp))>0]
dim(Listings_exp)
Listings_exp=na.omit(Listings_exp)
dim(Listings_exp)


```

# Correlation 

```{r}

colnames(Listings_exp)
Listings_exp %>% select(price, minimum_nights, number_of_reviews, availability_365, review_scores_rating, reviews_per_month, calculated_host_listings_count) %>% cor

# There is no strong correlation between variables, except for calculated_host_listings_count and availability_365
plot(Listings_exp$availability_365, Listings_exp$calculated_host_listings_count)

plot(Listings_exp$availability_365, Listings_exp$price)

```

#Best subset selection
```{r}

Listings.full=regsubsets(price~ neighbourhood +minimum_nights+ number_of_reviews+ room_type+ availability_365+ review_scores_rating+ reviews_per_month+calculated_host_listings_count, data=Listings_exp, method="forward")
Listings_summary = summary(Listings.full)
Listings_summary
Listings_summary$adjr2

par(mfrow=c(2,2))
plot(Listings_summary$rss, xlab="Number of variables", ylab="RSS", type='l')
plot(Listings_summary$adjr2,xlab="Number of variables", ylab="Adjusted Rsq", type='l')

#Annotate the one with highest value
which.max(Listings_summary$adjr2)

## This shows that we need to consider 8th model with 7 coefficients, excluding calculated_host_listings_count
```


## Linear Regression with significant variables
```{r}

model_lm <- lm(price ~ neighbourhood + minimum_nights+ number_of_reviews+ availability_365+ review_scores_rating+ reviews_per_month + room_type, data = Listings_exp) 
summary(model_lm)

## minimum_nights, availability_365, reviews_score_rating have a positive impact on the prices. But our main focus is of the neighborhoods which determine prices. 
```


## Logistic Regression with full significant numerical values
```{r}


Listings_log <- Listings_exp %>% 
  select(price, minimum_nights, number_of_reviews, availability_365, reviews_per_month, review_scores_rating,room_type)
Listings_log=na.omit(Listings_log)
summary(Listings_log)
dim(Listings_log)
## Splitting prices into expensive (price > 100) and economical (price<100)
Listings_log$PriceRange[Listings_log$price>=100]=1
Listings_log$PriceRange[Listings_log$price<100]=0

## creating training and testing sets.

set.seed(3)
idx = sample(dim(Listings_log)[1] , 0.75*dim(Listings_log)[1] , replace = F)
Listings_train = Listings_log[idx , ]
Listings_test = Listings_log[-idx , ]

log.train=glm(PriceRange ~ minimum_nights+number_of_reviews+availability_365+reviews_per_month+review_scores_rating + room_type, family = binomial, data = Listings_train)
summary(log.train)

Listings_test$prob=predict(log.train,Listings_test,type = "response")
View(Listings_test)

Listings_test$predDir=0
Listings_test$predDir[Listings_test$prob>0.5]=1
with(Listings_test,
     table(predDir,PriceRange))
with(Listings_test,
     mean(predDir==PriceRange))

## The % of right perdiction is 77.47% without making a Type 1 or 2 errors. That means we were able to perdict the correct price range in 77.47 of 100 cases. 
```

#2)LDA

```{r}

lda.pred=lda(PriceRange~.,data=Listings_train)
lda.pred
summary(lda.pred)

ldatest = predict(lda.pred,Listings_test)
names(ldatest)

classes<-ldatest$class[1:200]

posteriorprobability<-ldatest$posterior[1:200,1]

table(ldatest$class,Listings_test$PriceRange)

mean(ldatest$class==Listings_test$PriceRange)

##The Accuracy using LDA is 77.47%, which is similar to Logistic
```

#3)QDA

```{r}

qda.pred=qda(PriceRange~.,data=Listings_train)
qda.pred
summary(qda.pred)

qdatest = predict(qda.pred,Listings_test)
names(qdatest)

classes<-qdatest$class[1:200]

posteriorprobability<-qdatest$posterior[1:200,1]

table(qdatest$class,Listings_test$PriceRange)

mean(qdatest$class==Listings_test$PriceRange)

##The Accuracy using QDA is 85.17%, higher than LDA and Logistic Regression 
```


#4)KNN
```{r}

KListings_train = as.matrix(Listings_train %>%
                              dplyr::select(price, minimum_nights, number_of_reviews, availability_365, reviews_per_month, review_scores_rating, PriceRange))
#KListings_train=na.omit(KListings_train)
dim(KListings_train)

KListings_test = as.matrix(Listings_test %>%
                             dplyr::select(price, minimum_nights, number_of_reviews, availability_365, reviews_per_month, review_scores_rating, PriceRange))
##KListings_test=na.omit(KListings_test)
dim(KListings_test)

klabel=as.matrix(Listings_train$PriceRange)
dim(klabel)
cl = klabel[,1]
knnaccuracy.fn=function(k,cl){
  
  knn.pred=knn(KListings_train,KListings_test,klabel,k)
  table(knn.pred, KListings_test[,c(7)])
  A=mean(knn.pred == KListings_test[,c(7)])
  
  return(A)
}


#looping it for k=1 to 100 and finding the highest value of accuracy A 

P=1:100 #initializing
for(i in seq(from = 1, to = 100, by = 1)){
  print("This is iteration number ")
  print(i)
  P[i]=knnaccuracy.fn(i,cl)
}


optimumK <- which.max(P)
optimumAccuracy <- max(P) 
optimumK
optimumAccuracy

##Maximum accuracy occured at K=35 as 97.63%
```

## Best Model with high Accuracy
```{r}
# After running Logistic, LDA, QDA and KNN, we achieved highest accuracy with KNN model at K=35, followed by QDA, LDA ~ Logistic.

```
