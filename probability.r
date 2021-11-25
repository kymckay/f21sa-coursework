# Finds P(Sample SD > x) using sample mean distribution
prob_S_from_means <- function(x, sample_size, means_sample) {
    # CLT allows finding the population SD as follows
    sigma = sqrt(var(means_sample) * sample_size)
    cat("Sigma:", sigma, "\n")

    # Pivotal value which follows chi-squared distribution
    # Value when S = sd(x)
    pv = (sample_size - 1) * x^2 / sigma^2
    cat("Pivotal value:", pv, "\n")

    # Desired probability that S > sd(x), so tail
    prob = pchisq(pv, df = sample_size - 1, lower.tail = FALSE)
    cat("Probability that S > sd(x):", prob, "\n\n")

    return(prob)
}
