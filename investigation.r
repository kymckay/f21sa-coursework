# Part 5 requires VGAM package, `anaconda install r-vgam`
library(VGAM)

# Wind data is stored as CSV, x column contains wind speeds
wind_data = read.csv("wind.csv")[["x"]]

# Histogram to summarise data
pdf("DailyMean.pdf", width=6, height=4)
par(mar=c(4,4,1,1)+0.1)
hist(wind_data, main=NULL, xlab="Daily average wind speed (mph)")
dev.off()

# Summary statistics
cat("\nWind Speed Summary Statistics\n")
cat("Mean:", mean(wind_data), "mph\n")
cat("Standard deviation:", sd(wind_data), "mph\n")
cat("Median:", median(wind_data), "mph\n")
cat("Quantiles:", quantile(wind_data), "(all mph)\n\n")

# MLE
# Derivated in report

sigma_hat = sqrt(sum(wind_data ^ 2) / (2 * length(wind_data)))
cat("MLE:", sigma_hat, "mph\n")

ese = sqrt(sigma_hat^2 / (4 * length(wind_data)))
cat("ese:", ese, "mph\n")

# From NCST Table 5
z025 = 1.96

# 95% confidence interval for sigma_hat
I095 = c(sigma_hat - z025 * ese, sigma_hat + z025 * ese)
cat("Confidence Interval: [", I095[1], ",", I095[2], "] mph\n\n")

# Find the sample mean may times to produce a distribution
Y_prime = rep(0, 10000) # Preallocate for efficiency
n = 1000 # Sample size
for (i in 1:10000) {
    # Part 5, random samples of the assumed Rayleigh distribution
    X_prime = rrayleigh(n, scale = sigma_hat)

    Y_prime[i] = mean(X_prime)
}

pdf("PredictionMean.pdf", width=6, height=4)
par(mar=c(4,4,1,1)+0.1)
hist(Y_prime, main=NULL,
xlab="Predicted mean wind speed in next 1000 days (mph)")
dev.off()

cat("\nPredicted Mean Summary Statistics\n")
cat("Mean:", mean(Y_prime), "mph\n")
cat("Standard deviation:", sd(Y_prime), "mph\n")
cat("Variance:", var(Y_prime), "mph\n")
cat("Median:", median(Y_prime), "mph\n")
cat("Quantiles:", quantile(Y_prime), "(all mph)\n\n")

# CLT allows finding the population SD as follows
sigma = sqrt(var(Y_prime) * n)
cat("Sigma:", sigma, "\n")

# Pivotal value which follows chi-squared distribution
# Value when S = sd(x)
pv = (n - 1) * var(wind_data) / sigma^2
cat("Pivotal value:", pv, "\n")

# Desired probability that S > sd(x), so tail
prob = pchisq(pv, df = n-1, lower.tail = FALSE)
cat("Probability that S > sd(x):", prob, "\n\n")
