# Part 5 requires VGAM package, `conda install r-vgam`
library(VGAM)

simulate <- function(scale, sample_size, iterations = 10000) {
    # Find the sample mean may times to produce a distribution
    Y_prime = rep(0, iterations) # Preallocate for efficiency

    for (i in 1:iterations) {
        # Part 5, random samples of the assumed Rayleigh distribution
        X_prime = rrayleigh(sample_size, scale = scale)

        Y_prime[i] = mean(X_prime)
    }

    return(Y_prime)
}
