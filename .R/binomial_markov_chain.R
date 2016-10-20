## ---- binomial_markov_chain_function
binomial_markov_chain <- function(n, p, chain_length) {
  
  Q <- matrix(0, n+1, n+1)
  Q[1, 1] <- Q[1, 2] <- Q[n+1, n+1] <- Q[n+1, n] <- 1/2
  for (i in 2:n) {
    Q[i, i-1] <- Q[i, i+1] <- 1/2
  }

  alpha <- matrix(0, n+1, n+1)
  alpha[1, 1] <- alpha[n+1, n+1] <- 1
  alpha[1, 2] <- min(n*p/(1-p), 1)
  alpha[n+1, n] <- min(n*(1-p)/p, 1)
  for (i in 2:n-1) {
    alpha[i, i-1] <- min(i/(n-i+1) * (1-p)/p, 1)
    alpha[i, i+1] <- min((n-i)/(i+1) * p/(1-p), 1)
  }
  alpha[n, n-1] <- min(n * (1-p)/p, 1)
  alpha[n, n+1] <- min(1/(n+1) * p/(1-p), 1)
  
  X <- vector(length=chain_length+1)
  X[0] <- 6
  X_previous <- 0
  states <- c(0:n)
  choice <- c(0,1)
  accepted <- 0
  for (i in 1:chain_length) {
    X_proposed <- sample(states, 1, prob = Q[X_previous+1,])
    accept <- rbinom(1, 1, alpha[X_previous+1, X_proposed+1])
    if (accept == 1) {
      accepted <- accepted + 1
      X[i] <- X_proposed
    } 
    else {
      X[i] <- X_previous
    }
    X_previous <- X[i]
  }
  
  accepted_proportion <- accepted/chain_length
  print(accepted_proportion)
  
  return(X)
  
  # P <- matrix(0, n+1, n+1) 
  # P[1, 2] <- 1/2 * min(n*p/(1-p), 1)
  # P[n+1, n] <- 1/2 * min(n*(1-p)/p, 1)
  # P[1, 1] <- 1 - P[1, 2]
  # P[n+1, n+1] <- 1 - P[n+1, n]
  # for (i in 2:(n-1)) {
  #   P[i, i-1] <- 1/2 * min(i/(n-i+1) * (1-p)/p, 1)
  #   P[i, i+1] <- 1/2 * min((n-i)/(i+1) * p/(1-p), 1)
  #   P[i, i] <- 1 - P[i, i-1] - P[i, i+1]
  # }
  # P[n, n-1] <- 1/2 * min(n * (1-p)/p, 1)
  # P[n, n+1] <- 1/2 * min(1/(n+1) * p/(1-p), 1)
  # P[n, n] <- 1 - P[n, n-1] - P[n, n+1]
  
  # P_exp <- P
  # for (i in chain_length) {
  #   P_exp <- P_exp%*%P
  # }
  
  # return(P_exp)
  
}