#Part A
#Q1 Import Dataset through Import Dataset button

#Q2
head(population_malaysia, 5)

#Q3
dim(population_malaysia)

#Q6
summary(population_malaysia)
install.packages("dplyr")
library(dplyr)
count(population_malaysia, date)
count(population_malaysia, sex)
count(population_malaysia, age)
count(population_malaysia, ethnicity)


#Part B
#Q1
install.packages("lessR")
library(lessR)
#BarChart(population, data=population_malaysia, by1=date)
BarChart(date, data=population_malaysia)

#Q2
PieChart(sex, data=population_malaysia)

#Q3
Chart(age, data=population_malaysia)



#Q4

library(lessR)
d <- population_malaysia
Plot(ethnicity, data=population_malaysia)

?Chart
?X
?XY

#Part C
#Q1.1
summary(population_malaysia)

sexCount <- table(population_malaysia$sex)
sexCountNum <- as.numeric(sexCount)
meanSex <- mean(sexCountNum)
medianSex <- median(sexCountNum)
modeSex <- max(sexCountNum)
meanSex
medianSex
modeSex

ageCount <- table(population_malaysia$age)
ageCountNum <- as.numeric(ageCount)
meanAge <- mean(ageCountNum)
meanAge
medianAge <- median(ageCountNum)
modeAge <- max(ageCountNum)
medianAge
modeAge






popuCount <- table(population_malaysia$population)
popuCountNum <- as.numeric(popuCount)
meanPopu <- mean(popuCountNum)
meanPopu


ECount <- table(population_malaysia$ethnicity)
ECountNum <- as.numeric(ECount)
meanE <- mean(ECountNum)
medianE <- median(ECountNum)
modeE <- max(ECountNum)
meanE
medianE
modeE


#Q2
summary(population_malaysia)

RangePopu <- range(population_malaysia$population)
Range <- diff(RangePopu)
Range

vPopu <- var(population_malaysia$population)
vPopu

sdPopu <- sd(population_malaysia$population)
sdPopu

iqrPopu <- IQR(population_malaysia$population)
iqrPopu



#Part D
#Q1.2


Male2 <- population_malaysia[population_malaysia$sex == "male", ]
MeanMale <- mean(Male2$population)
MeanMale


Female <- population_malaysia[population_malaysia$sex == "female", ]
MeanFemale <- mean(Female$population)
MeanFemale




#Q1.4
library(lessR)

popuCount <- table(population_malaysia$population)
popuCountNum <- as.numeric(popuCount)
meanPopu <- mean(popuCountNum)
meanPopu

Male2 <- population_malaysia[population_malaysia$sex == "male", ]
MeanMale <- mean(Male2$population)
MeanMale


Female <- population_malaysia[population_malaysia$sex == "female", ]
MeanFemale <- mean(Female$population)
MeanFemale

MaleFemaleMean <- data.frame(
  sex = c("Male", "Female"),
  Mean = c(MeanMale, MeanFemale)
)


Chart(Mean, by=sex, data=MaleFemaleMean)







