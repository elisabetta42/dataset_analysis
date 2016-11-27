titanic1 <- read.csv("C:/Users/Elisabetta/Desktop/EM/titanic3.csv",header= TRUE) #load the csv table containing data

# 1. FIRST OVERVIEW OF THE DATA - Basic Features
# AGE
titanic1$age[is.na(titanic1$age)] <- median(titanic1$age, na.rm=T) #mean value of all the available ages to replace NA
age= titanic1$age
qqnorm(age)
qqline(age,col="green")  #testing the normality 
mean(titanic1$age,na.rm=TRUE) #calculate the mean
median(titanic1$age,na.rm=TRUE) #calculate the median
sd(titanic1$age,na.rm=TRUE) #calculate the standard deviation
var(titanic1$age,na.rm=TRUE) #calculate the variance

utils::str(hist(titanic1$age, main="Distribution of age", 
                xlab="age", col = "gray", border="blue"))#age distribution


myhist <- hist(titanic1$age, main="Distribution of age", 
               xlab="age", col = "gray", border="blue")
multiplier <- myhist$counts / myhist$density
mydensity <- density(titanic1$age,na.rm=TRUE)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity) # plot age distribution 

#SEX
summary(titanic1$sex) #count the number of female and male

#SURVIVAL
table(titanic1$survived)
table(titanic1$survived)-> survived
prop.table(survived)*100 ->psurvived #calculate percentage of survived
round(psurvived, digits=1)->psurvived
barplot(psurvived,  main="Percentage of survived",
        xlab="survived", ylab="percentage",col = "gray", border="blue") #plotting the percentage of survived

#CLASS
table(titanic1$pclass) #number of passengers for each class
table(titanic1$pclass) -> pclass
prop.table(pclass)*100 -> class #percentage of passengers for each class
round(class, digits=1) ->class
class

#SUMMARY OF sex, age and class
table (titanic1[,c("survived","sex","pclass")])

# 2. CHECK CORRELATION BETWEEN CLASS AND SURVIVAL
mosaicplot(titanic1$pclass ~ titanic1$survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")
table(titanic1[,c("survived","pclass")]) #comparison between class and survival using table
barplot(table(titanic1[,c("survived","pclass")]),legend.text =c("dead","survived")) #plotting the previous table using barchar

# 3. CHECK CORRELATION BETWEEN AGE AND SURVIVAL
boxplot(titanic1$age ~ titanic1$survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

# 3.1 ANALYSIS TAKING INTO ACCOUNT ONLY CHILDREN
titanic1.children <- titanic1[which (titanic1$age < 13),] #select all the children
table(titanic1.children[,c("survived","pclass")]) #number of children survived
barplot(table(titanic1.children[,c("survived","pclass")]),legend.text =c("dead","survived"))

# 3.2 DIVIDE DATA INTO CATEGORIES
# C = "children", A= "adults" and S = "senior"
test1 <- titanic1
test1$age[test1$age<=13] <- "C"
test1$age[test1$age>13 & test1$age<=30 & test1$age!="C"] <- "A"
test1$age[test1$age>30 & test1$age!="C"  & test1$age!="A"] <- "S"
table(test1[,c("survived","age")])
barplot(table(test1[,c("survived","age")]),legend.text =c("dead","survived"))

# 4. CHECK CORRELATION BETWEEN SEX AND SURVIVAL
table(titanic1[,c("survived","sex")]) #comparison between  and survival sex using table
barplot(table(titanic1[,c("survived","sex")]),legend.text =c("dead","survived"))




