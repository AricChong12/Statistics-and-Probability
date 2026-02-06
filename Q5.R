# Question 5
p <- 0.90
n <- 100
se <- sqrt(p * (1 - p) / n)

# Answer (a)
ans_a <- 1 - (pnorm(0.92, p, se) - pnorm(0.88, p, se))
print(paste("Part A:", round(ans_a, 4)))

# Answer (b)
ans_b <- pnorm(0.84, p, se)
print(paste("Part B:", round(ans_b, 4)))

# Answer (c)
ans_c <- pnorm(0.95, p, se, lower.tail = FALSE)
print(paste("Part C:", round(ans_c, 4)))