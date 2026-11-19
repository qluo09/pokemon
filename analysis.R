# STAT 344 Project - Pokemon Weight

# Read data
pokemon <- read.csv("Pokeman.csv")

head(pokemon)
table(pokemon$generation)
summary(pokemon$weight_kg)


# Population sizes and weights
generation <- c("I", "II", "III", "IV", "V",
                "VI", "VII", "VIII", "IX")

N_g <- c(151, 100, 135, 107, 156, 72, 88, 96, 120)

W_g <- N_g / sum(N_g)


# Calculate sample statistics for each generation
n_g <- numeric(9)
ybar_g <- numeric(9)
s2_g <- numeric(9)

for (g in 1:9) {
  
  y <- pokemon$weight_kg[pokemon$generation == generation[g]]
  
  n_g[g] <- length(y)
  ybar_g[g] <- mean(y)
  s2_g[g] <- var(y)
  
}

summary_table <- data.frame(
  Generation = generation,
  N_g = N_g,
  n_g = n_g,
  W_g = round(W_g, 4),
  Mean = round(ybar_g, 2),
  Variance = round(s2_g, 2),
  SD = round(sqrt(s2_g), 2)
)

print(summary_table)


# Histogram of Pokemon weights
hist(pokemon$weight_kg,
     main = "Pokemon Weight Distribution",
     xlab = "Weight (kg)",
     ylab = "Frequency",
     col = "yellow",
     breaks = 15)


# Boxplot by generation
pokemon$generation <- factor(pokemon$generation,
                             levels = generation)

boxplot(weight_kg ~ generation,
        data = pokemon,
        main = "Pokemon Weight by Generation",
        xlab = "Generation",
        ylab = "Weight (kg)",
        col = "#6495ED")


# Stratified mean estimate
ybar_st <- sum(W_g * ybar_g)


# Estimated variance and standard error
v2_st <- sum(W_g^2 * (1/n_g - 1/N_g) * s2_g)

SE_st <- sqrt(v2_st)


# 95% confidence interval
lower <- ybar_st - 1.96 * SE_st
upper <- ybar_st + 1.96 * SE_st


# Results
results <- data.frame(
  Estimate = round(ybar_st, 2),
  Variance = round(v2_st, 2),
  SE = round(SE_st, 2),
  CI_Lower = round(lower, 2),
  CI_Upper = round(upper, 2)
)

print(results)
