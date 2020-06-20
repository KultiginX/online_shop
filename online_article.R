library(tidyverse)
library(stringr)
library(dplyr)
#install.packages("ggpubr")
library(ggpubr)
rm(list = ls())

getwd()
setwd("/Users/kultiginbozdemir/Documents/GitHub/online_shop")
rstudioapi::getSourceEditorContext()$path

df<-read.delim2("iw_article.txt")
str(df)
summary(df)
head(df)
tail(df)
names(df)
summary(df$owner)
summary(df$IWAN)
summary(df$article_No)
summary(df$productGroup)
summary(df$colorDescription)
summary(df$size)


#let's drop unneccessary columns
drops <- c("owner","IWAN","article_No", "deftime", "modtime",
           "colorCode")
#df<-df[ , !(names(df) %in% drops)]



# 8th column is not consistent. It has to be consistent of "0" or "1". 
#Further rows of df has been stored in this columnn. 
#First element of this long string tells us if it is "0" or "1"
# We can recover those rows,but no time for that.
table(df$articleOnline)
barplot(prop.table(table(df$articleOnline)))


df2 = df[FALSE,]
str(df2)
head(df2)


counnt=0
for (i in seq_along(df$articleOnline)){
  if (df$articleOnline[i]=="1" | df$articleOnline[i]=="0" ) {
    next}
  #print(".............i starts.........")
  print(i)
  n=toString(df$articleOnline[i])
  text=strsplit(n, "     ")
  #print(text)
  #print("+++++++ n 1. part   +++++++")
  text1=strsplit(n, "     ")[[1]][1]
  print(text1)
  df[i,13]<-text1[[1]]
  counnt=counnt+1
  
  #print("========n 2.part =======")
  text2=strsplit(n, "     ")[[1]][2]
  
  for (line in strsplit(text2, "\n")){
    #print(line[line>0])
    #print("----------------------------start of newrow")
    newrow<- (strsplit(line[line>0], "\t"))
    #print(as.list(newrow[1]))
    dfad <- data.frame(matrix(unlist(newrow), ncol = 13, byrow=T))
    df2<-rbind(df2, dfad)
    #print("end of row")
    #print(paste("length of the newrow", length(newrow)))
  }
  
}
counnt  # shows the total nuber of rows which has been corrected.
#Remember the df
dim(df)
#Check created new dataframe from the corrpted column of articleOnline.
dim(df2)
head(df2)
#tail(df2)
#Let's merge both data frames.
colnames(df2)<-names(df)
df<-rbind(df,df2)
# Dimennsions after merge
cat("dimennsions after merge:" , dim(df))

#Let's check second issue; factors of articleOnline column if they are consisted of either 0 or 1.
print(df$articleOnline[ df$articleOnline!=0 &df$articleOnline!=1])

#to check if there is still untiddy values in df$articleOnline
for (i in seq_along(df$articleOnline)){
  if (df$articleOnline[i]=="0" |df$articleOnline[i]=="1" ){next}
  print(i)
  print(df$articleOnline[i])
}
#Let's clear unused factors.
str(df$articleOnline)

df$articleOnline<-factor(df$articleOnline)
str(df$articleOnline)

#Let's plot it.
table(df$articleOnline)
barplot(prop.table(table(df$articleOnline)))


#unit price
df$unitPrice<-as.integer(df$unitPrice)
df$description[max((df$unitPrice))]
df$description[min((df$unitPrice))]

# Product Group
names(df)
barplot(prop.table(table(df$productGroup)))
# See most popular product groups
sort(table(df$productGroup))
max(table(df$productGroup))

# plot product group against price
gg<-ggplot(df, aes(x=productGroup, y=unitPrice, color=articleOnline)) + 
  geom_point() + labs(title="product group~ unnit price Scatterplot", x="Product group", y="unit price") 
gg+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#aggregate then plot it.
agg = aggregate(df$unitPrice,
                by = list(df$productGroup),
                FUN = mean)
colnames(agg)<-(c("product_group","mean_price"))

gg<-ggplot(agg, aes(x=product_group, y=mean_price)) + 
  geom_point() + labs(title="product group~ mean price Scatterplot", x="Product group", y="mean price") 
gg+theme(axis.text.x = element_text(angle = 90, hjust = 1))
# It seems that there is no a correlation between price and product group.
# But it seems that there can ve clusters. 



#Size
# is there any correlation between size and price. 
# extract only letter sizes ; XS, S, ....XXL
ls=sort(levels(df$size))
ls
size_with_letter=ls[336:348]
sort(table(df$size))

# let is aggregate and find the meann of sizes of XS,.... XXL
prices=list()
for (i in size_with_letter){
  p=mean(as.integer(df$unitPrice[df$size==i]))
  prices<-c(prices, p)
  cat("size:",i, "=", p)
  print("......")
}
#put the values in a df
price_size_df= data.frame(prices=as.integer(prices), size=size_with_letter)
#plot
gg<-ggplot(price_size_df, aes(x=size, y=prices)) + 
  geom_point() + labs(title="size~ price Scatterplot", x="size", y="price") 
gg+theme(axis.text.x = element_text(angle = 90, hjust = 1))
 # it seems there is no clear correlation. it is clear one size is very expensive.







