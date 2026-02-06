
df_trt <- 5
ss_trt <- 70
df_err <- 23
ss_total <- 300

df_total <- df_trt + df_err
ss_err <- ss_total - ss_trt

ms_trt <- ss_trt/df_trt
ms_err <- ss_err/df_err
Fstat  <- ms_trt/ms_err

anova_table <- data.frame(
  Source = c("Treatments","Error","Total"),
  df = c(df_trt, df_err, df_total),
  SS = c(ss_trt, ss_err, ss_total),
  MS = c(ms_trt, ms_err, NA),
  F  = c(Fstat, NA, NA)
)
F_005 <- qf(0.95, df1 = df_trt, df2 = df_err)  # alpha = 0.05
F_001 <- qf(0.99, df1 = df_trt, df2 = df_err)  # alpha = 0.01

F_005
F_001

anova_table

s1 <- c(48,56,46,45,50)
s2 <- c(60,56,53,60,51)
s3 <- c(57,55,52,50,51)

y <- c(s1,s2,s3)
system <- factor(rep(c("System1","System2","System3"), each=5))
df_c <- data.frame(y, system)


boxplot(y ~ system, data=df_c,
        main="Figure 5(c): Efficiency by Fuel Injection System",
        xlab="System", ylab="Efficiency")

fit_c <- aov(y ~ system, data=df_c)
summary(fit_c)


tapply(df_c$y, df_c$system, mean)

online <- c(72,84,77,80,81)
hybrid <- c(83,73,84,81)
f2f    <- c(80,78,84,81,86,79,82)

score <- c(online, hybrid, f2f)
type <- factor(c(rep("Online",5), rep("Hybrid",4), rep("Face-to-Face",7)))
df_d <- data.frame(score, type)

# Plot
boxplot(score ~ type, data=df_d,
        main="Figure 5(d):",
        xlab="Delivery Type", ylab="Final Score")

# ANOVA
fit_d <- aov(score ~ type, data=df_d)

summary(fit_d)

tapply(df_d$score, df_d$type, mean)

heavy  <- c(5.1, 3.1, 4.7, 5.3)
medium <- c(4.0, 3.5, 4.5, 6.1)
light  <- c(3.1, 3.3, 2.1, 1.9)

dist <- c(heavy, medium, light)
paper <- factor(rep(c("Heavy","Medium","Light"), each=4))
df_e <- data.frame(dist, paper)

# (Plot for report)
boxplot(dist ~ paper, data=df_e,
        main="Figure 5(e1): Airplane Distance by Paper Weight",
        xlab="Paper Weight", ylab="Distance (m)")

# Descriptive stats (means + SD)
tapply(df_e$dist, df_e$paper, mean)
tapply(df_e$dist, df_e$paper, sd)

# One-way ANOVA
fit_e <- aov(dist ~ paper, data=df_e)
summary(fit_e)

# Values they ask you to fill (manual-style but done in R)
means <- tapply(df_e$dist, df_e$paper, mean)
vars  <- tapply(df_e$dist, df_e$paper, var)

var_group_means  <- var(means)    
n <- 4
MS_between <- n * var_group_means  
MS_within  <- mean(vars)           

Fstat <- MS_between / MS_within
pval  <- pf(Fstat, df1=2, df2=9, lower.tail=FALSE)

var_group_means
MS_between
MS_within
Fstat
pval


Fcrit_001 <- qf(0.99, df1=2, df2=9)
Fcrit_001

curve(df(x, 2, 9), from=0, to=20,
      main="Figure 5(e2): F Distribution (df1=2, df2=9) with p-value shaded",
      xlab="F", ylab="Density")
abline(v=Fstat, lwd=2)

x <- seq(Fstat, 20, length=500)
y <- df(x, 2, 9)
polygon(c(Fstat, x, 20), c(0, y, 0), density=20, angle=45, border=NA)
