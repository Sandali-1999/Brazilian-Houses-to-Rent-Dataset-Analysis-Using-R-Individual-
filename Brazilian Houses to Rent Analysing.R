attach(Brazilian_Houses_to_Rent)
#%%%%%%%%%%%%%%%%%%%%  CLEAR DATA SET  %%%%%%%%%%%%%%%%%%%%
#install.packages("tidyverse")
library(tidyverse)
#view(Brazilian_Houses_to_Rent)
glimpse(Brazilian_Houses_to_Rent)
#__________________________ Check Missing Values in Data Set ____________________________
sum(is.na(Brazilian_Houses_to_Rent))

#__________________________ Check Duplicated Rows in Data Set ___________________________
sum(duplicated(Brazilian_Houses_to_Rent))

#__________________________ Remove Duplicated Rows in Data Set __________________________
Brazilian_Houses_to_Rent=unique(Brazilian_Houses_to_Rent)
sum(duplicated(Brazilian_Houses_to_Rent))

#__________________________ Summary of Data Set __________________________________
summary(Brazilian_Houses_to_Rent)

#__________________________ Check Unique in variables _________________________________
unique(Brazilian_Houses_to_Rent$floor)
Brazilian_Houses_to_Rent$floor[Brazilian_Houses_to_Rent$floor == '-']=0
unique(Brazilian_Houses_to_Rent$floor)
New_DataSet = subset(Brazilian_Houses_to_Rent, Brazilian_Houses_to_Rent$floor != "301") 
New_DataSet
unique(New_DataSet$floor)

unique(city)
unique(rooms)
unique(bathroom)
unique(`parking spaces`)
unique(`keeping animal`)
unique(furniture)

#_________________ Objective1 [Does the total rent amount significantly affect the floor number?]___________

#Remove Outline Data in Total Rent 
plot(New_DataSet$floor,New_DataSet$`total rent`)
boxplot(New_DataSet$`total rent`) 
summary(`total rent`)
Outlier_Data_total_rent=filter(New_DataSet,New_DataSet$`total rent`>30000)
Outlier_Data_total_rent
Total_Rent_Filtering_DataSet=filter(New_DataSet,New_DataSet$`total rent`<=30000)
Total_Rent_Filtering_DataSet
boxplot(Total_Rent_Filtering_DataSet$`total rent`)

#Check for normality Total rent
qqnorm(Total_Rent_Filtering_DataSet$`total rent`)
qqline(Total_Rent_Filtering_DataSet$`total rent`)
ks.test(Total_Rent_Filtering_DataSet$`total rent`,"pnorm")
unique(New_DataSet$floor)

#Non Parametric Test (Kruskal Wallis Test)
kruskal.test(Total_Rent_Filtering_DataSet$`total rent`,Total_Rent_Filtering_DataSet$floor)

#Check median in the sample data set
aggregate(Total_Rent_Filtering_DataSet$`total rent`, list(Total_Rent_Filtering_DataSet$floor), FUN=median)

#Draw box plot 
boxplot(Total_Rent_Filtering_DataSet$`total rent`~Total_Rent_Filtering_DataSet$floor, main="Floor Vs Total Rent", col="orange", xlab="Floor", ylab = "Total Rent")

#_________________Check Objective2 [Does the house rental amount significantly affect the keeping of a pet or not?]___________

#Remove Outline Data in the Rent amount
boxplot(New_DataSet$`rent amount`) 
summary(New_DataSet$`rent amount`)
Outlier_Data_rent_amount=filter(New_DataSet,New_DataSet$`rent amount`>15000)
Outlier_Data_rent_amount
Rent_Filtering_DataSet=filter(New_DataSet,New_DataSet$`rent amount`<=15000)
Rent_Filtering_DataSet
boxplot(Rent_Filtering_DataSet$`rent amount`)

#Checking for normality
qqnorm(Rent_Filtering_DataSet$`rent amount`)
qqline(Rent_Filtering_DataSet$`rent amount`)
ks.test(Rent_Filtering_DataSet$`rent amount`,"pnorm")

#Non Parametric Test(Wilcoxon's Rank Sum Test)
wilcox.test(Rent_Filtering_DataSet$`rent amount`~Rent_Filtering_DataSet$`keeping animal`,mu=0,alternative="two.sided",conf.int=T,Conf.level=0.95,paired=F,exact=F,correct=T)
wilcox.test(Rent_Filtering_DataSet$`rent amount`~Rent_Filtering_DataSet$`keeping animal`,mu=0,alternative="greater",conf.int=T,Conf.level=0.95,paired=F,exact=F,correct=T)

#Check median in the sample data set
aggregate(Rent_Filtering_DataSet$`rent amount`, list(Rent_Filtering_DataSet$`keeping animal`), FUN=median)

#Draw box plot 
boxplot(Rent_Filtering_DataSet$`rent amount`~Rent_Filtering_DataSet$`keeping animal`,main="Kepping Animal Vs Rent Amount", col="pink", xlab = "Kepping Animal", ylab = "Rent Amount")

#__________________________ Objective 3 [Does Fire insurance significantly affect whether the house has furniture or not?] ____________________

#Remove Outline Data in the fire insurance
summary(New_DataSet$`fire insurance`)
boxplot(New_DataSet$`fire insurance`)

Outlier_Data_fire_insurance=filter(New_DataSet,New_DataSet$`fire insurance`>250)
Outlier_Data_fire_insurance
Fire_Insurance_Filtering_DataSet=filter(New_DataSet,New_DataSet$`fire insurance`<=250)
Fire_Insurance_Filtering_DataSet
boxplot(Fire_Insurance_Filtering_DataSet$`fire insurance`)

#Check for normality Fire insurance
qqnorm(Fire_Insurance_Filtering_DataSet$`fire insurance`)
qqline(Fire_Insurance_Filtering_DataSet$`fire insurance`)
ks.test(Fire_Insurance_Filtering_DataSet$`fire insurance`,"pnorm")

#Non Parametric Test (Wilcoxon's Rank Sum Test)
wilcox.test(Fire_Insurance_Filtering_DataSet$`fire insurance`~Fire_Insurance_Filtering_DataSet$furniture,mu=0,alternative="two.sided",conf.int=T,Conf.level=0.95,paired=F,exact=F,correct=T)
wilcox.test(Fire_Insurance_Filtering_DataSet$`fire insurance`~Fire_Insurance_Filtering_DataSet$furniture,mu=0,alternative="greater",conf.int=T,Conf.level=0.95,paired=F,exact=F,correct=T)

#Check median in the sample data set
aggregate(Fire_Insurance_Filtering_DataSet$`fire insurance`, list(Fire_Insurance_Filtering_DataSet$furniture), FUN=median)

#Draw box plot 
boxplot(Fire_Insurance_Filtering_DataSet$`fire insurance`~Fire_Insurance_Filtering_DataSet$furniture, main="Furnished Vs Fire Insurance", col="sky blue", xlab = "Furnished", ylab="Fire Insurance")

#_____________________ Objective 4 [Does the city in which the property is significantly affect the total rent amount?] ______________________________

#Checking for normality Total rent
qqnorm(Total_Rent_Filtering_DataSet$`total rent`)
qqline(Total_Rent_Filtering_DataSet$`total rent`)
ks.test(Total_Rent_Filtering_DataSet$`total rent`)


#Non Parametric Test (Kruskal Wallis Test)
kruskal.test(Total_Rent_Filtering_DataSet$`total rent`,Total_Rent_Filtering_DataSet$city)

#Check median in the sample data set
aggregate(Total_Rent_Filtering_DataSet$`total rent`, list(Total_Rent_Filtering_DataSet$city), FUN=median)

#Draw box plot
boxplot(Total_Rent_Filtering_DataSet$`total rent`~Total_Rent_Filtering_DataSet$city, col="green", main="City Vs Total Rent", xlab = "City", ylab = "Total Rent")

#_____________________ Objective 5 [Does property tax relate to the area of the property?] ___________________
#Remove Outline Data in Property tax
summary(New_DataSet$`property tax`)
boxplot(New_DataSet$`property tax`)

Outlier_Data_Property_Tax=filter(New_DataSet,New_DataSet$`property tax`>4000)
Outlier_Data_Property_Tax
Property_Tax_Filtering_DataSet=filter(New_DataSet,New_DataSet$`property tax`<=4000)
Property_Tax_Filtering_DataSet
boxplot(Property_Tax_Filtering_DataSet$`property tax`)

#Check normality in Property tax
qqnorm(Property_Tax_Filtering_DataSet$`property tax`)
qqline(Property_Tax_Filtering_DataSet$`property tax`)
ks.test(Property_Tax_Filtering_DataSet$`property tax`,"pnorm")

#Remove Outline data in Area
summary(New_DataSet$area)
boxplot(New_DataSet$area)

Outlier_Data_Area=filter(New_DataSet,New_DataSet$area>800)
Outlier_Data_Area
Area_Filtering_DataSet=filter(New_DataSet,New_DataSet$area<=800)
Area_Filtering_DataSet
boxplot(Area_Filtering_DataSet$area)

#Check normality in Area
qqnorm(Area_Filtering_DataSet$area)
qqline(Area_Filtering_DataSet$area)
ks.test(Area_Filtering_DataSet$area,"pnorm")

#Filer Outline data of Property tax in the Area Filtering data set
Area_Property_Tax_Filtering_DataSet=filter(Area_Filtering_DataSet,Area_Filtering_DataSet$`property tax`<=4000)
Area_Property_Tax_Filtering_DataSet

#Non Parametric Test (Spearman Correlation Test)
cor.test(Area_Property_Tax_Filtering_DataSet$area,Area_Property_Tax_Filtering_DataSet$`property tax`,method="spearman",exact = FALSE,alternative = "two.sided")
cor.test(Area_Property_Tax_Filtering_DataSet$area,Area_Property_Tax_Filtering_DataSet$`property tax`,method="spearman",exact = FALSE,alternative = "greater")


#Draw Scatter Plot
library(ggplot2)
ggplot(Area_Property_Tax_Filtering_DataSet, aes(x = Area_Property_Tax_Filtering_DataSet$area, y = Area_Property_Tax_Filtering_DataSet$`property tax`)) + 
  geom_point(color = "blue", size = 0.5) + 
  ggtitle("Area Vs Property Tax") + 
  xlab("Area") + ylab("Property Tax")+
  geom_smooth(method=lm, color='#2C3E50')
