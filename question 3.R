#Question 3a
#Parameters
n_orders <- 6
p_success <- 0.90

#i. Probability of exactly 4 orders on time
prob_exact_4 <- dbinom(4,size=n_orders,prob=p_success)
print(paste("Probability of exactly 4: ", prob_exact_4))

#ii. Probability of at least 5 orders
prob_at_least_5 <- pbinom(4,size=n_orders,prob=p_success,lower.tail=FALSE)
print(paste("Probability of at least 5:",prob_at_least_5))

#iii. Probability distribution function
x_vals <- 0:6
probs <-dbinom(x_vals,size=n_orders,prob=p_success)

barplot(probs,names.arg = x_vals,
        main="Binomial Distribution of On-Time Deliveries(n=6,p=0.9)",
        xlab="Number of On-Time Orders", ylab="Probability",col="lightblue"


#Question 3b
#Parameters
#m = number of dinner orders
#n = number of non-dinner orders
#k = sample size
m_dinner <- 12
n_non_dinner <-20 - 12 
k_sample <- 6

#i. Probability that exactly 4 are dinner orders
prob_hyp_exact_4 <- dhyper(x=4,m=m_dinner,n=n_non_dinner,k=k_sample)
print(paste("Probability of exactly 4 dinner orders:",prob_hyp_exact_4))

#ii. Probability that at least 3 are dinner orders
prob_hyp_at_least_3 <-phyper(q=2,m=m_dinner,n=n_non_dinner,k=k_sample)
print(paste("Probability of at least 3 dinner orders:", prob_hyp_at_least_3))


#Question 3c
#Parameter
#lambda = Average rate of orders per hour
lambda_val <-15

#i. Calculate the probability of receiving exactly 10 orders
prob_pois_exact_10 <- dpois(10,lambda=lambda_val)
print(paste("Probability of exactly 10 orders:",prob_pois_exact_10))


#ii. Probability of more than 18 orders
prob_pois_more_18 <- ppois(18,lambda=lambda_val,lower.tail=FALSE)
print(paste("Probability more than 18 orders:",prob_pois_more_18))

#iii. Plot the probability distribution function
x_vals_pois <-0:40
y_vals_pois <- dpois(x_vals_pois,lambda=lambda_val)

plot(x_vals_pois, y_vals_pois, type = "h", lwd = 2, col = "darkgreen",
     main = "Poisson Distribution of Orders (lambda=15)",
     xlab = "Number of Orders per Hour", ylab = "Probability")
# Add points on top of the lines for better visibility
points(x_vals_pois, y_vals_pois, pch = 16, col = "darkgreen")
