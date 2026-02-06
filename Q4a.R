#Q4
#ai
hist(Babies_Weight_2$Weight)

#aii

femaleB <- Babies_Weight_2[Babies_Weight_2$Gender == "1", ]
hist(femaleB$Weight)

maleB <- Babies_Weight_2[Babies_Weight_2$Gender == "0", ]
hist(maleB$Weight)

#aiii
mean(maleB$Weight)
sd(maleB$Weight)


mean(femaleB$Weight)
sd(femaleB$Weight)

#aiv
install.packages("dplyr")
library(dplyr)

CountFemaleB <- count(femaleB)

FHigherEqual <- Babies_Weight_2[Babies_Weight_2$Gender == "1" & Babies_Weight_2$Weight >= 132, ]

CountFHigherEqual <- count(FHigherEqual)


ProbabF = CountFHigherEqual / CountFemaleB
ProbabF

#av

library(dplyr)

CountFemaleB <- count(femaleB)

FBetween <- Babies_Weight_2[Babies_Weight_2$Gender == "1" & Babies_Weight_2$Weight >= 120 & Babies_Weight_2 <= 130, ]

CountFBetween <- count(FBetween)


ProbabFB = CountFBetween / CountFemaleB
ProbabFB

#avi
library(dplyr)

CountFemaleB <- count(femaleB)

FBetween2 <- Babies_Weight_2[Babies_Weight_2$Gender == "1" & Babies_Weight_2$Weight >= 130 & Babies_Weight_2 <= 140, ]

CountFBetween2 <- count(FBetween2)


ProbabFB2 = CountFBetween2 / CountFemaleB
ProbabFB2


#aviii

install.packages("dplyr")
library(dplyr)

CountMaleB <- count(maleB)

MLower <- Babies_Weight_2[Babies_Weight_2$Gender == "0" & Babies_Weight_2$Weight < 128, ]

CountMLower <- count(MLower)


ProbabM = CountMLower / CountMaleB
ProbabM



#aix


library(dplyr)

CountMaleB <- count(maleB)

MBetween <- Babies_Weight_2[Babies_Weight_2$Gender == "0" & Babies_Weight_2$Weight >= 130 & Babies_Weight_2$Weight <= 140, ]

CountMBetween <- count(MBetween)


ProbabMBetween = CountMBetween / CountMaleB
ProbabMBetween

#axi
qnorm(0.85, mean=mean(femaleB$Weight), sd=sd(femaleB$Weight))

#axii
qnorm(0.80, mean=mean(femaleB$Weight), sd=sd(femaleB$Weight))
qnorm(0.20, mean=mean(femaleB$Weight), sd=sd(femaleB$Weight))




#axiii
qnorm(0.20, mean=mean(maleB$Weight), sd=sd(maleB$Weight))

#axiv
qnorm(0.20, mean=mean(maleB$Weight), sd=sd(maleB$Weight))
qnorm(0.80, mean=mean(maleB$Weight), sd=sd(maleB$Weight))












