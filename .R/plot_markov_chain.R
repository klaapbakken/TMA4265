#source("binomial_markov_chain.R")
## ---- plot_markov_chain 
n <- 20
p <- 0.3
chain_length <- 5000
X <- binomial_markov_chain(n, p, chain_length)

qplot(c(0:199), X[1:200], geom="line", xlab = expression('X'[n]), ylab = "State")

## ---- plot_markov_chain_histogram
states <- c(0:n)
ggplot() +
  geom_bar(data = data.frame(X), aes(x = X, y = (..count..)/sum(..count..))) +
  geom_point(data = data.frame(states), aes(x = states, y = dbinom(states, n, prob=p))) +
  labs(x = "State", y = "Relative frequency") #+

## ---- old
# theme_bw()
# ggplot(data.frame(X), aes(x=X)) + theme_bw() + 
#   geom_bar(aes(y = (..count..)/sum(..count..))) + labs(x = "State", y = "Relative frequency") #+
  # stat_bin(aes(y = ..density..)) +
  # stat_function(fun = dbinom(c(0:n), n, p))
  # geom_histogram(aes(y = ..density..))
  # geom_point(aes(x = c(0:n), y = dbinom(x, n, prob=p)))