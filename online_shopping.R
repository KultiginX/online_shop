library(tidyverse)


getwd()
setwd("/Users/kultiginbozdemir/GitHub/online_shop")
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

install.packages("ggpubr")
library(ggpubr)
corr<-cor(df$age,df$credit)
print(corr)
#Plot
gg<-ggplot(df, aes(x=city, y=age, color=gender)) + geom_point() + 
        labs(title="Scatterplot", x="City", y="age") 
gg+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Berlin
berlin<-subset(df, df$city=="Berlin")
berlin
summary(berlin)

gg<-ggplot(berlin, aes(x=postcode, y=age, color=gender)) + 
        geom_point() + labs(title="Berlin Scatterplot", x="Postcode", y="age") 
gg+theme(axis.text.x = element_text(angle = 90, hjust = 1))
summary(df$city)

#Herford
herford<-subset(df, df$city=="Herford")
gg<-ggplot(herford, aes(x=postcode, y=age, color=gender)) +
        geom_point() + labs(title="Herford Scatterplot", x="Postcode", y="age") 
gg+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Credit
gg<-ggplot(herford, aes(x=postcode, y=age, alpha=1, color=credit)) +
        geom_point() + labs(title="Herford Scatterplot", x="Postcode", y="age") 
gg+theme(axis.text.x = element_text(angle = 90, hjust = 1))
summary(herford)
as.character(df$postcode)[1]
