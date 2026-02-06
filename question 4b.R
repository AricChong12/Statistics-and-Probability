#Question 4b

#Parameters
mu <- 50 
sigma <- 2

#i.Simulate
sim_data <- rnorm(300,mean=mu,sd=sigma)

#Display first 10 simulated lengths
print("First 10 simulated lengths:")
print(head(sim_data,10))

#Compute sample mean & standard deviation
sample_mean <-mean(sim_data)
sample_sd <-sd(sim_data)

print(paste("Theoretical Mean:50 | Sample Mean:",round(sample_mean,4)))
print(paste("Theoretical SD: 2 | Sample SD:",round(sample_sd,4)))


#ii.Visualization
#Plot Histogram
hist(sim_data,probability=TRUE,col="lightblue",
     main="Simulated Metal Rod Lengths vs Theoretical Normal Curve",
     xlab="Length(mm)",breaks=15,ylim=c(0,0.25))

#Add density line
lines(density(sim_data),col="blue",lwd=2,lty=2)

#Add Theoretical normal curve
curve(dnorm(x,mean=mu,sd=sigma),add=TRUE,col="red",lwd=2)

#Add legend 
legend("topright",legend=c("Sample Density","Theoretical Curve"),
        col=c("blue","red"),lty=c(2,1),lwd=2)


#iii.Calculate Probabilities
# 1.P(X<48)
prob_less_48 <-pnorm(48,mean=mu,sd=sigma)
print(paste("1.P(X<48):",prob_less_48))

# 2.P(X>52)
prob_more_52 <-pnorm(52,mean=mu,sd=sigma,lower.tail=FALSE)
print(paste("2.P(X>52):",prob_more_52))

# 3.P(49<X<51)
prob_between_49_51 <-pnorm(51,mean=mu,sd=sigma)-pnorm(49,mean=mu,sd=sigma)
print(paste("3.P(49<X<51):",prob_between_49_51))


#iv.Determine Quantiles
# 1. Length below which 10% of rods fall
q_10_percent <-qnorm(0.10,mean=mu,sd=sigma)
print(paste("Length below which 10% fall:",q_10_percent))

# 2. Length above which 5% of rods fall
q_top_5_percent <-qnorm(0.95,mean=mu,sd=sigma)
print(paste("Length above which 5% fall:",q_top_5_percent))

# 3. Middle 95% range
lower_bound <-qnorm(0.025,mean=mu,sd=sigma)
upper_bound <-qnorm(0.975,mean=mu,sd=sigma)
print(paste("Middle 95% range is between", round(lower_bound,2),"and",round(upper_bound,2)))

#v. Standard Normal Transformation & Reflection
#1. Convert X=53 to Z-score
x_val <-53
z_score <-(x_val-mu)/sigma
print(paste("Z-score for X=53:",z_score))

#Find P(Z<z_score)
prob_z <-pnorm(z_score,mean=0,sd=1)
print(paste("P(Z<1.5) using standard normal:",prob_z))

#2. Compare with direct calculation P(X<53)
prob_x_direct <-pnorm(53,mean=mu,sd=sigma)
print(paste("P(X<53) using direct normal:",prob_x_direct))
