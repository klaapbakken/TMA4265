## ---- more_than_200
# Poisson mean
lambda <- 3
# Calculating the number of days elapsed, which gives 59
t <- as.numeric(as.Date("2017-03-01") - as.Date("2017-01-01"), units="days")
ppois(200, lambda=lambda*t, lower=FALSE)

## ---- first_two_months
## Part 1: Simulating number of Poisson events during the first two months of 2017
observations = 1000  # Number of simulated observations

# Drawing samples from Poisson distribution
V <- rpois(observations, lambda=lambda*t)

# Calculating percentage of samples larger than 200
sum(V > 200) / observations * 100

## ---- tenth_arrival_time
## Part 2: Simulating arrival times of the tenth Poisson event
n <- 10 # Tenth event
# Drawing samples from the Gamma distribution
W <- rgamma(observations, n, lambda)
# Calculating the mean arrival time
mean(W)

## ---- simulations
## Part 3: Plotting 100 Poisson processes as a function of time

# Function for generating one poisson process with Poisson
# parameter lambda, up to time t_max
pp_realizations <- function(lambda=1, t_max) {
  # N(t=0) = 0, the first axiom of Poisson processes
  t <- c(0)
  repeat{
    # Generate the next event's arrival time as the
    # previous arrival time plus an exponentially
    # distributed interarrival time
    new_t = t[length(t)] + rexp(n=1, rate=lambda)
    if(new_t <= t_max){
      t <- c(t, new_t)
    }
    else{
      # t_max has been reached,
      # and this process simulation is finished
      return(c(t, new_t))
    }
  }
}

# Looking at 100 Poisson processes the first 30 days
processes = 100
t_max = 35 # Calculating a bit past the t cutoff point of the plotting area

# Specifying data types and which data to plot
require(ggplot2)
df <- data.frame(t = numeric(), N = numeric())
g <- ggplot(df, aes(t, N))

# Generating the 100 independent Posson processes
for(b in 1:100){
  t = pp_realizations(lambda = lambda, t_max = t_max)
  N <- 0:(length(t)-1)
  new_row = data.frame(t, N)
  g <- g + geom_step(data=new_row, color="darkgrey")
}
g <- g + coord_cartesian(xlim = c(0, t_max-5)) 
print(g)