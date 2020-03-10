


getwd()
#setwd("/Users/kultiginbozdemir/GitHub/online_shop")
rstudioapi::getSourceEditorContext()$path

df<-read.delim2("iw_customer.txt")
str(df)
summary(df)
head(df)
tail(df)

min(as.Date(df$birthdate))
summary(df$birthdate)
df$birthdate<-as.Date(df$birthdate)
str(df$birthdate)
min(df$birthdate)
# we add a new column of age
df$age<- as.integer((Sys.Date()-df$birthdate)/365) # not very accurate, it works for now
head(df$age)
#let's drop unneccessary columns
drops <- c("riskID","eMail", "newsletter",  "owner",
           "creditLimit", "street","firstname","surname","birthdate")
df<-df[ , !(names(df) %in% drops)]
head(df)
#Create a new column of gender
summary(df$salutation)
df$gender<-ifelse ( df$salutation=="Frau", "Female", "Male" )
#df[,c("salutation","gender")]
df$gender<-as.factor(df$gender)
#Chech if we created the new gender column correctly.
summary(df$gender)
summary(df$salutation)
head(df)
drops <- c("salutation")
df<-df[ , !(names(df) %in% drops)]
head(df)
summary(df)
boxplot(df[,2:4])
