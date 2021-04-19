
########## PRACTICE PROBLEMS ##########

#9E1. Which of the following is a requirement of the simple Metropolis algorithm?
# (1) The parameters must be discrete
# (2) The likelihood function must be gaussian
# (3) The proposal distribution must be symmetric  (X)

# 9E2.  Gibbs sampling is more efficient than the Metropolis algorithm. How does it achieve 
#   this extra efficiency?  Are there any limitations to the Gibbs sampling strategy?
# A. using conjugate pair, you gain efficiency.  
# A. Gibbs has limitations when you don't want to use conjugate pair- with multilvel models
#   that need priors for entire covariance matrices, the conjugate priors are pathological in shape
#   (more in chap 14)
#   With high dim of params (thousands), both metropolis and Gibbs become very inefficient

# 9E3.  Which sort of parameters can Hamiltonian Monte Carlo not handle? Can you explain why?
# A.  parameters must be continuous. The HMC algorithm looks for the next value as it computes 
#   gradients and stopped at any point.  Hence the space must be continuous.

# 9E4.  Explain the difference between the effective number of samples, n_eff as calculated by
#   Stan, and the actual number of samples.
#   A.  n_eff is number of effective samples that are uncorrelated.  HMC adaptive sampler that Stan uses can
#   produce sequential samples that are better than the samples.

# 9E5.  Which value should Rhat approach, when a chain is sampling the posterior distribution correctly?
#  A. 1 (computes variation within the chain to variation between chains)

#9E6. Sketch a good trace plot for a Markov chain, one that is effectively sampling from posterior
# distribution. What is good about its shape?  Then sketch a trace plot for a 
# malfunctioning Markov chain. What about its shape indicates malfunction?
# good: stationary, well mixing, similar across different chains
# bad:  spikes etc

# 9E7.  Repeat the problem above, but now for a trace rank plot
# good: max or min of chains are somewhat overlapping across different chains

# 9M1. Re-estimate the terrain ruggedness model from the chapter, but now using a uniform 
# prior for the standard deviation, sigma.  The uniform prior should be unif(0,1).
# Use ulam to estimate the posterior.  Does the different prior have any detectible 
# influence on the posterior distribution of sigma? why or why not?

# A. No noticeable/detectable influence on the posterior. the posterior distributions of all params look similar
#    Perhaps it's due to flat/weakly informative prior for sigma

library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]
dd$log_gdp_std <- dd$log_gdp/mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa==1, 1, 2)

dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer(dd$cid)
)

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid]+ b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dunif(0,1)
  ), data= dat_slim, chains=1
)

m9m1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid]+ b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dunif(0,1)
  ), data= dat_slim, chains=1
)

precis(m9.1, depth=2)
pairs(m9.1)

precis(m9m1, depth=2)
pairs(m9m1)

# 9M2.  Modify the terrain ruggedness model again.  This time, change the prior for
#  b[cid] to dexp(0.3).  What does this do to the posterior distribution? Can you explain it?

# A. The shape of the posterior for b is skewed as expected from the prior

m9m2 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid]+ b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dexp(0.3),
    sigma ~ dexp(1)
  ), data= dat_slim, chains=1
)
precis(m9m2, depth=2)
pairs(m9m2)
traceplot(m9m2)

# 9M3.  Re-estimate one of the Stan models from the chapter, but at different numbers of warmup iterations.
#  Be sure to use the same number of sampling itermations in each case.  Compare the n_eff values.  
#  How much warmup is enough?

# A. For the ruggedness model, I tried 300, 100, 50 and 10 warm ups, and at 50 it was showing acceptable results
#    Not sure if there is a rule of tumb for sufficient proportion of warmup.

m9m3 <- ulam(
  warmup = 50,
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid]+ b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dexp(0.3),
    sigma ~ dexp(1)
  ), data= dat_slim, chains=1
)
precis(m9m3, depth=2)
show(m9m3)
# warmup: 300
#       mean   sd 5.5% 94.5% n_eff Rhat4
# a[1]  0.89 0.02 0.86  0.91   332     1
# a[2]  1.05 0.01 1.03  1.06   502     1
# b[1]  0.14 0.07 0.02  0.26   226     1
# b[2]  0.02 0.02 0.00  0.05  1094     1
# sigma 0.11 0.01 0.10  0.12   336     1

#warmup: 100
#       mean   sd 5.5% 94.5% n_eff Rhat4
# a[1]  0.89 0.02 0.86  0.91  1035     1
# a[2]  1.05 0.01 1.03  1.06  1008     1
# b[1]  0.15 0.08 0.03  0.27   155     1
# b[2]  0.02 0.02 0.00  0.06   170     1
# sigma 0.11 0.01 0.10  0.12   716     1

#warmup: 10 (got divergent error)
# mean   sd 5.5% 94.5% n_eff Rhat4
# a[1]  0.89 0.02 0.86  0.92   473  1.00
# a[2]  1.05 0.01 1.03  1.06   558  1.00
# b[1]  0.22 0.10 0.15  0.33    20  1.00
# b[2]  0.03 0.08 0.01  0.07    25  1.05
# sigma 0.12 0.01 0.10  0.13   111  1.00

# warmup: 50
# mean   sd 5.5% 94.5% n_eff Rhat4
# a[1]  0.89 0.01 0.87  0.91   793     1
# a[2]  1.05 0.01 1.03  1.07   968     1
# b[1]  0.15 0.08 0.04  0.29   204     1
# b[2]  0.02 0.02 0.00  0.07   138     1
# sigma 0.11 0.01 0.10  0.12   790     1


#9H1.  Run the model below and then inspect the posterior distribution and explain what it is accomplishing.

# A. It's estimating normal (a) and cauchy (b) distributions 
library(rethinking)
mp <- ulam(
  alist(
    a~ dnorm(0,1),
    b ~ dcauchy(0,1)
  ), data=list(y=1), chains=1
)
precis(mp) 
traceplot(mp)


#  Compare the samples for the parameters a and b.  Can you explain the different trace plots?
# If you are unfamliar with the Cauchy distribution, you should look it up.  The key feature to attend to is that
#  it has no expected value.  Can you connect this fact to the trace plot?
# the trace plot of a shows more stationary, b is all over the place; it makes sense as you would expect
# caughy to be unreliable distribution

# A. yes, stan ulam throws an unreliable posterior mean error, and the traceplot of b shows the values
#   going all over the place (unreliable)


#9H2.  Recall the divorce rate example from Chap 5.  Repeat that analysis, using ulam this time fitting models m5.1, m5.2, and m5.3.
#  Use compare to compare the models on the basis of WAIC or PSIS.  To use WAIC or PSIS with ulam, you need add the argument
#  log_lik=TRUE.  Explain the model comparison results.

#  A. The result of ulam is similar to that of quad.  

# going back to chap 5
data("WaffleDivorce")
d <- WaffleDivorce

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)


# define posterior and fit the model
m5.1 <- quap(
  alist(
    D~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

m5.2 <- quap(
  alist(
    D~dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = d
)

m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
  mu <- a + bM*M + bA*A,
  a ~ dnorm(0, 0.2),
  bM ~ dnorm(0, 0.5),
  bA ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
  ), data= d
)
precis(m5.3)
compare(m5.1, m5.2, m5.3, func = WAIC)
compare(m5.1, m5.2, m5.3, func = PSIS)

# m5.1 and m5.3 have similar WAIC value/SE 

#       WAIC    SE dWAIC   dSE pWAIC weight
# m5.1 126.7 13.93   0.0    NA   4.2   0.78
# m5.3 129.3 14.66   2.6  1.17   5.8   0.21
# m5.2 140.0 10.15  13.3 10.69   3.0   0.00


dat9h2_slim <- list(
  d_std = d$D,
  m_std = d$M,
  a_std = d$A
)
str(dat9h2_slim)

m9h2.1 <- ulam(
  alist(
    d_std ~ dnorm(mu, sigma),
    mu <- a + bA * a_std,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dat9h2_slim, chains=1, log_lik=TRUE
)

# got unreliable posterior error
m9h2.2 <- ulam(
  alist(
    d_std ~dnorm(mu, sigma),
    mu <- a + bM * m_std,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = dat9h2_slim, chains=1, log_lik=TRUE
)

m9h2.3 <- ulam(
  alist(
    d_std ~ dnorm(mu, sigma),
    mu <- a + bM*m_std + bA*a_std,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data= dat9h2_slim, chains=1, log_lik=TRUE
)

compare(m5.1, m5.2, m5.3, func=PSIS)
compare(m5.1, m5.2, m5.3, func=WAIC)


compare(m9h2.1, m9h2.2, m9h2.3, func=PSIS )
compare(m9h2.1, m9h2.2, m9h2.3, func=WAIC )


# 9H3.  Sometimes changing a prior for one parameter has unanticipated effects on other parameters.  This is because
# when a parameter is highly correlated with another parameters in the posterior, the prior influences both
# parameters.  Here's an example to work and think through.
#  Go back to the leg length example in Chap 6 and use the code tehre to simulate height and leg lengths for 100 imagined individuals.
# Below is the model you fit before, resulting in a highly correlated posterior for the two beta parameters.
# This time, fit the model using ulam:

N <- 100
set.seed(909)
height <-rnorm(N, 10, 2)
leg_prop <- runif(N,0.4, 0.5)
leg_left <- leg_prop* height + rnorm(N, 0, 0.02)
leg_right <- leg_prop*height + rnorm(N, 0, 0.02)

d <- data.frame(height, leg_left, leg_right)


# Compare the posterior distribution produced by the code above to the posterior distribution produced
# when you change the prior for br so that it is strictly positive:
library(rethinking)
m5.8s <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2,10),
    sigma ~ dexp(1)
  ), data=d, chains =4,log_lik=TRUE,
  start=list(a=10, bl=0, br=0.1, sigma=1)
)

pairs(m5.8s)

m5.8s2 <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2,10),
    sigma ~ dexp(1)
  ), data=d, chains =4, log_lik=TRUE,
  constraints = list(br="lower=0"),
  start=list(a=10, bl=0, br=0.1, sigma=1)
)

precis(m5.8s2)
pairs(m5.8s2)
# > precis(m5.8s2)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a      0.97 0.29  0.50  1.42   927  1.00
# b1    -0.67 1.86 -3.82  1.74   370  1.01
# br     2.67 1.86  0.28  5.89   372  1.01
# sigma  0.63 0.04  0.56  0.70   526  1.00

# > precis(m5.8s)
#       mean   sd  5.5% 94.5% n_eff Rhat4
# a     0.99 0.29  0.54  1.46   820  1.00
# b1    0.35 2.56 -3.77  4.28   603  1.01
# br    1.65 2.56 -2.31  5.76   600  1.01 
# sigma 0.63 0.05  0.56  0.71   920  1.00

# Note the constraints list.  What this does is constrain the prior distribution of br so that it has
# positive probablity only above zero.  In other words, that prior ensures that the posterior disrbituion for br will have
# no probability mass below zero  Compare the two posterior distrubtions for m5.8s and m5.8s2.  What has changed in the posteior
# disgribution of both beta parameters?  Can you explain the change induced by the change in prior?

# A.  both not significant where as br came out to be significant.

traceplot(m5.8s2)
pairs(m5.8s2)

# 9H4.  For the two models fit in the previous problem, use WAIC or PSIS to compare the effective numbers of parameters for each model.
# You will need to use log_lik=TRUE to instruct ulam to compute the terms that both WAIC and PSIS need. Which model has more 
# effective parameters? Why?

compare(m5.8s, m5.8s2, func=PSIS)


# 9H5.Modify the Metropolis algorithm code from the chapter to handle the case that the island populations have a different
# distribution than the island labels.  This means the island's number will not be the same as its population.
num_weeks <- 10
positions <- rep(0, num_weeks)
populations <- sample(c(1:10), size=10)

current <- 10
for(i in 1:num_weeks){
  ## record current position
  positions[i] <- current

  ## flip coin to generate proposal
  proposal <- current + sample(c(-1,1), size = 1)
  
  ## now make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal >10) proposal <- 1

  ## move?
  prob_move <- populations[proposal]/populations[current]
  print(paste("prob_move: ", prob_move))
  current <- ifelse(runif(1) < prob_move, proposal, current)
  
}

#9H6.  Modify the Metropolis algorithm code from the chapter to write your own simple MCMC estimator for globe tossing data
# and model from Chapter 2.

# Proportion of Water
n_samples <- 500
p <- rep(NA, n_samples)
p_current <- 0.3

W <- 6
L <- 3

for(i in 1:n_samples) {
  
  # current value of p
  p[i] <- p_current
  
  # generate new proposal
  p_proposed <- runif(1,0,1)
    
  # posterior of current
  prob_current <- dbinom(W, W+L, p_current) * dunif(p_current, 0,1)
  
  # posterior of proposal
  prob_proposed <- dbinom(W, W+L, p_proposed) * dunif(p_proposed,0,1)
  
  # ratio to compare 
  r <- prob_proposed/prob_current
  
  # update 
  p_current <- ifelse(runif(1) < r, p_proposed, p_current)
}

plot(p, type = "l")

#9H7.  Can you write your own Hamiltonian Monte Carlo algorithm for the globe tossing data, using the R code in the chapter?
# You will have to write your own functions for the likelihood and gradient, but 
# you can use the HMC2 function.

# Negative log of f(w,n|p)*f(p)
U <- function(q, a=0, b=1){
  pr <- q
  U <- sum(dbinom(W, N, pr, log=TRUE)) + dunif(pr, a, b, log=TRUE)
  return(-U)
}

# gradient function
U_gradient <- function(q){
   pr <- q
   G <- sum(W/pr - (N-W)/(1-pr))
   return (-G)
}

library(rethinking)
W <- 3
N <- 9

Q <- list()
Q$q <- 0.5
step <- 0.03
n_samples <- 100
L <- 11 # steps

plot(NULL, xlab="p", xlim=c(0,1), ylim=c(0,n_samples/1.5))
points(Q$q, 1, pch=20, col="black")


for (i in 1:n_samples) {
  Q <- HMC2(U, U_gradient, step, L, Q$q)
  points(Q$q, i, pch=20, col="black")
}


