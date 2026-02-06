#Part B
#ai,aii,aiii

unique(electricity_consumption$date)
unique(electricity_consumption$sector)

#b
mean(electricity_consumption$consumption)


#c
mean(electricity_consumption$consumption)



#d

n <- length(electricity_consumption$consumption)
s <- sd(electricity_consumption$consumption)
xBar <- mean(electricity_consumption$consumption)

value <- 1 - (0.05 / 2)
criticalRegion <- qnorm(value)

error <- s / sqrt(n)
marginError <- criticalRegion * error

lowerBounds <- xBar - marginError
upperBounds <- xBar + marginError

lowerBounds
upperBounds


#e
MarginOfError <- criticalRegion * (s / sqrt(n))
MarginOfError

y <- MarginOfError / mean(electricity_consumption$consumption)
y * 100

