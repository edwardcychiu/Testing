df_org<-read.csv('bank-market_final.csv')
df_work<-df_org

## Question 1 ====
sum(duplicated(df_work))
df_work<-df_work[!duplicated(df_work),]
sum(duplicated(df_work))

df_work<-df_work[,-1] # remove CustomerID
summary(df_work) # check data

# remove row age < 18
sum(df_work$age<18)
df_work<-df_work[df_work$age>=18,]
sum(df_work$age<18)
dim(df_work)
# remove row pdays = -2
sum(df_work$pdays< (-1))
df_work<-df_work[!df_work$pdays< (-1),]
sum(df_work$pdays< (-1))
dim(df_work)
# remove NA
sum(is.na(df_work))
df_work<-df_work[complete.cases(df_work),]
dim(df_work)
# current as factor
df_work$current<-as.factor(df_work$current)
df<-df_work
dim(df)

write.csv(df, "df_clean.csv", row.names = FALSE)
## Question 2 ====
# a
library(cluster)
library(tidyverse)
gd1<-daisy(df, metric = "gower")
# b
set.seed(123)
km1<-kmeans(gd1, 2)
# c
df2<-df %>% select(-current)
gd2<-daisy(df2, metric = "gower")
# d
set.seed(123)
km2<-kmeans(gd2, 2)
# e
table(km1$cluster, km2$cluster) 
# Both of them have the same clustering result.
# "current" column only have one identical value -> meaningless on clustering

## Question 3 ====
# a
df_num<-df %>% select(age, balance, duration, campaign, previous, pdays)
gd_num<-daisy(df_num, metric = "gower")
# b
set.seed(123)
km_num<-kmeans(gd_num, 2)
# c
df_scale<-scale(df_num)
gd_scale<-daisy(df_scale, metric = "gower")
# d
set.seed(123)
km_scale<-kmeans(gd_scale, 2)
# e
table(km_num$cluster, km_scale$cluster)
# Both of them have the same clustering result.
# As in the Gower Distance calculation, when the targeted variable is numerical, the distance calculation has the scaling function.

## Question 4 ====
# a
eu<-dist(df_num, method = "euclidean")
eu_scale<-dist(df_scale, method = "euclidean")
# b
set.seed(123)
km_eu<-kmeans(eu, 2)
set.seed(123)
km_euscale<-kmeans(eu_scale, 2)
# c
table(km_eu$cluster, km_euscale$cluster)
# They have different result. 
# Standardization will affect the clustering result if we use euclidean distance, as the euclidean distance is scale sensitive.