## ---- nh_more_than_200
t = 59
k = pi / 182.5
m <- 2*t + (1/k) * sin(k*t)
print(m)

ppois(200, lambda=m, lower=FALSE)

## Part 1: Simulating number of Poisson events during the first two months of 2017
observations = 1000  # Number of simulated observations

# Drawing samples from Poisson distribution
V <- rpois(observations, lambda=m)

# Calculating percentage of samples larger than 200
sum(V > 200) / observations * 100