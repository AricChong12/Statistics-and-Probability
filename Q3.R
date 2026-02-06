#Q3
#ai
library(lessR)

#Server X
MeanX <- mean(ServerResponseTimes$Server.X)
print(MeanX)
MedianX <- median(ServerResponseTimes$Server.X)
print(MedianX)
sdX <- sd(ServerResponseTimes$Server.X)
print(sdX)


#Server Y
MeanY <- mean(ServerResponseTimes$Server.Y)
print(MeanY)
MedianY <- median(ServerResponseTimes$Server.Y)
print(MedianY)
sdY <- sd(ServerResponseTimes$Server.Y)
print(sdY)


#aii


XandY <- data.frame(
  server = c("X", "Y"),
  Mean = c(MeanX, MeanY)
)

Chart(Mean, by=server, data=XandY)


X <- ServerResponseTimes$Server.X
Y <- ServerResponseTimes$Server.Y


BoxData <- data.frame(
  #server = c("X", "Y")
  data = c(X, Y)
)





#BoxPlot
BoxPlot(
  BoxData,
  data=ServerResponseTimes
)


#ci
ttest(Server.X, Server.Y, data=ServerResponseTimes)










