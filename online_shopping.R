


getwd()
setwd("/Users/kultiginbozdemir/GitHub/online_shop")
df<-read.delim2("iw_customer.txt")
head(df)
summary(df)
head(df)
tail(df)
bo(df$birthdate)
min(as.Date(df$birthdate))
summary(df$birthdate)
df<- data.frame(df)
df$birthdate<-as.Date(df$birthdate)
str(df$birthdate)
min(df$birthdate)
df$age<- as.integer((Sys.Date()-df$birthdate)/365)
head(df$age)
