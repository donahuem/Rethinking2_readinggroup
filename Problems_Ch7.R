# Statistical Rethinking Chapter 7 exercises ----------------------
# load library
library(rethinking)

# Easy -----------------------------------------------------------
# 7E1. State the three motivating criteria that define information entropy. Try to express each in your
# own words.

# Answer: information entropy is associated with measuring the average level of uncertainty in a
# distribution. The three motivating criteria are (p209-210):
# 1. Uncertainty measures should be continuous, otherwise small changes in probability = large changes
# in uncertainty
# 2. Uncertainty increases as the number of possible events increase. If there are more possible outcomes
# we should be less certain about any one outcome.
# 3. Uncertainty measures should be additive. You should be able to sum up uncertainty for all 
# combinations of events (ex is rain/hot, rain/cold, sun/hot, sun/cold). 


# 7E2. Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads
# 70% of the time. What is the entropy of this coin?

# Answer, following code on p 210:
# Probability of heads = 0.7, tails = 0.3
p <- c( 0.7 , 0.3 )
# calculate information entropy
-sum( p*log(p) )
# 0.6108643

# 7E3. Suppose a four-sided die is loaded such that, when tossed onto a table, it shows “1” 20%, “2”
# 25%, ”3” 25%, and ”4” 30% of the time. What is the entropy of this die?

# Answer, following code on p 210 and above:
p2 <- c(0.2, 0.25, 0.25, 0.3)
# calculate information entropy
-sum( p2*log(p2) )
# 1.376227

# 7E4. Suppose another four-sided die is loaded such that it never shows “4”. The other three sides
# show equally often. What is the entropy of this die?
# Answer, following code on p 210 and above below. There is no need to include a category that 
# doesn't occur, so simply use the values for the three other sides:
p3 <- c(1/3, 1/3, 1/3) 
# calculate information entropy
-sum( p3*log(p3) )
# 1.015035



# Medium ----------------------------------------------------------
# 7M1. Write down and compare the definitions of AIC and WAIC. Which of these criteria is most
# general? Which assumptions are required to transform the more general criterion into a less general
# one?

# Answer: AIC and WAIC estimate average out-of-sample deviance.  
# AIC = Dtrain + 2p = −2lppd + 2p (p 223) and has three important assumptions:
# (1) The priors are flat or overwhelmed by the likelihood.
# (2) The posterior distribution is approximately multivariate Gaussian.
# (3) The sample size N is much greater113 than the number of parameters k.
# AIC indicates the expected bias is proportion to the number of free parameters (p)
# The fitting term is based on Dtrain
# WAIC = −2(lppd − pWAIC)
# WAIC has a penatly proportional to the variance of the posterior distribution, summing
# the variance in log probabilities across each observation i
# This make WAIC more flexible, which does not require the assumptions listed above are met

# 7M2. Explain the difference between model selection and model comparison. What information is
# lost under model selection?

# Answer: model selection refers to choosing the model with the lowest criterion value and 
# discarding the rest, while model comparison a more general approach that uses multiple 
# models to understand both how variables influence predictions

# 7M3. When comparing models with an information criterion, why must all models be fit to exactly
# the same observations? What would happen to the information criterion values, if the models were
# fit to different numbers of observations? Perform some experiments, if you are not sure.

# The models must be fit to the exact same observations because the observations are used as targets.
# If the number of observations changed than the information criteria values would not be comparable.
# For example, if there are fewer observations to predict than there should be less prediction erros
# and then the deviance should be smaller
# Example using data from p 226
data(cars)

m <- quap(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) , data=cars )

set.seed(94)
post <- extract.samples(m,n=1000)

n_samples <- 1000
logprob <- sapply( 1:n_samples ,
                   function(s) {
                     mu <- post$a[s] + post$b[s]*cars$speed
                     dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
                   } )

n_cases <- nrow(cars)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(logprob[i,]) - log(n_samples) )

pWAIC <- sapply( 1:n_cases , function(i) var(logprob[i,]) )
m_WAIC <- -2*( sum(lppd) - sum(pWAIC) )
# 423.3127

# Repeat above with fewer data observations
cars2 <- cars[-1,] # remove the first row of data and repeat the process above

m2 <- quap(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) , data=cars2 )

set.seed(94)
post <- extract.samples(m2,n=1000)

n_samples <- 1000
logprob <- sapply( 1:n_samples ,
                   function(s) {
                     mu <- post$a[s] + post$b[s]*cars$speed
                     dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
                   } )

n_cases <- nrow(cars2)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(logprob[i,]) - log(n_samples) )

pWAIC <- sapply( 1:n_cases , function(i) var(logprob[i,]) )
m2_WAIC <- -2*( sum(lppd) - sum(pWAIC) )
# 416.1146

# 7M4. What happens to the effective number of parameters, as measured by PSIS or WAIC, as a prior
# becomes more concentrated? Why? Perform some experiments, if you are not sure.

# Answer: As a prior becomes more concentrated, the PSIS or WAIC should be less flexible and influenced
# by the sample, and less prone to overfitting. This is shown in Fig. 7.8 on p 220.
# Example:
y <- rnorm(20)

A7m4a <- map(
  alist(
    y ~ dnorm(mu,1),
    mu ~ dnorm(0,sigma)
  ),
  data=list(y=y,sigma=10) )

WAIC(A7m4a)

#   WAIC      lppd   penalty std_err
#   58.05474 -28.09714 0.9302268 7.21479

# repeat above with more concentrated prior
A7m4b <- map(
  alist(
    y ~ dnorm(mu,0.3),
    mu ~ dnorm(0,sigma)
  ),
  data=list(y=y,sigma=10) )

WAIC(A7m4b)
#   WAIC      lppd  penalty  std_err
#   215.5498 -97.70102 10.07387 80.33784

# 7M5. Provide an informal explanation of why informative priors reduce overfitting.

# Answer: informative priors reduce overfitting because they reduce the sensitivity of 
# the model to the data sample. 

# 7M6. Provide an informal explanation of why overly informative priors result in underfitting.
# Overly informative priors limit the "learning" the model can do from the data by indicating
# limited plausibility for data where data actually occurs.


# Hard -------------------------------------------------------------
# 7H1. In 2007, The Wall Street Journal published an editorial (“We’re Number
# One, Alas”) with a graph of corporate tax rates in 29 countries plotted
# against tax revenue. A badly fit curve was drawn in (reconstructed
# at right), seemingly by hand, to make the argument that the relationship
# between tax rate and tax revenue increases and then declines, such that
# higher tax rates can actually produce less tax revenue. I want you to actually
# fit a curve to these data, found in data(Laffer). Consider models
# that use tax rate to predict tax revenue. Compare, using WAIC or PSIS, a
# straight-line model to any curved models you like. What do you conclude
# about the relationship between tax rate and tax revenue?

# Answer, this is like the exercise starting on p 200
data(Laffer)

Laffer$tax_rate_std <- scale(Laffer$tax_rate)
Laffer$tax_revenue_std <- scale(Laffer$tax_revenue)

linear_mod <- quap(
  alist(
    tax_revenue_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b*tax_rate_std,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=Laffer )

curved_mod <- quap(
  alist(
    tax_revenue_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b*tax_rate_std^2,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=Laffer )
  
compare(linear_mod, curved_mod)

#             WAIC  SE    dWAIC  dSE pWAIC weight
# curved_mod 85.7 20.98   0.0   NA   5.7    0.8
# linear_mod 88.5 20.23   2.8 2.37   5.7    0.2

# Although the curved model has a lower (and therefore better WAIC), the SE is so high high
# that the model fits overlap and therefore we cannot say there is a real difference between
# the two models

# plot
par(mfrow=c(1,2))
post <- extract.samples(linear_mod)
mass_seq <- seq( from=min(Laffer$tax_rate_std) , to=max(Laffer$tax_rate_std) , length.out=100 )
l <- link( linear_mod , data=list( tax_rate_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( tax_revenue_std ~ tax_rate_std , data=Laffer, pch = 16, xlab = "Standardized tax rate", ylab = "Standardized tax revenue")
lines( mass_seq , mu )
shade( ci , mass_seq )

post <- extract.samples(curved_mod)
l <- link( curved_mod , data=list( tax_rate_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( tax_revenue_std ~ tax_rate_std , data=Laffer, pch = 16, xlab = "Standardized tax rate", ylab = "Standardized tax revenue")
lines( mass_seq , mu )
shade( ci , mass_seq )


# 7H2. In the Laffer data, there is one country with a high tax revenue that is an outlier. Use PSIS
# and WAIC to measure the importance of this outlier in the models you fit in the previous problem.
# Then use robust regression with a Student’s t distribution to revisit the curve fitting problem. How
# much does a curved relationship depend upon the outlier point?

# Answer, following code on p 236
PSIS_linear <- PSIS(linear_mod, pointwise=TRUE)
WAIC_linear <- WAIC(linear_mod, pointwise=TRUE)
plot( PSIS_linear$k , WAIC_linear$penalty , xlab="PSIS Pareto k" ,
      ylab="WAIC penalty" , col=rangi2 , lwd=2, main = "linear model" )

PSIS_curved <- PSIS(curved_mod, pointwise=TRUE)
WAIC_curved <- WAIC(curved_mod, pointwise=TRUE)
plot( PSIS_curved$k , WAIC_curved$penalty , xlab="PSIS Pareto k" ,
      ylab="WAIC penalty" , col=rangi2 , lwd=2, main = "curved model" )

# refit curved model with student's t distribtuion
curved_mod_student <- quap(
  alist(
    tax_revenue_std ~ dstudent(2, mu , exp(log_sigma) ),
    mu <- a + b*tax_rate_std^2,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=Laffer )

PSIS_curved_student <- PSIS(curved_mod_student, pointwise=TRUE)
WAIC_curved_student <- WAIC(curved_mod_student, pointwise=TRUE)
# the outlier becomes much less important in the curved model with the student's t distribution
# You can see this by looking at row 12 in the above data. This is expected because this 
# distribution indicates more extreme values are less unexpected than a guassian distribution.

# 7H3. Consider three fictional Polynesian islands. On each there is a Royal Ornithologist charged by
# the king with surveying the bird population. They have each found the following proportions of 5
# important bird species:
#   Species A Species B Species C Species D Species E
# Island 1 0.2 0.2 0.2 0.2 0.2
# Island 2 0.8 0.1 0.05 0.025 0.025
# Island 3 0.05 0.15 0.7 0.05 0.05
# Notice that each row sums to 1, all the birds. This problem has two parts. It is not computationally
# complicated. But it is conceptually tricky. First, compute the entropy of each island’s bird distribution.
# Interpret these entropy values. Second, use each island’s bird distribution to predict the other two.
# This means to compute the K-L Divergence of each island from the others, treating each island as if
# it were a statistical model of the other islands. You should end up with 6 different K-L Divergence
# values. Which island predicts the others best? Why?

# Answer, part 1, calculate entropy of each island's bird distribution:
Island1 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
Island2 <- c(0.8, 0.1, 0.05, 0.025, 0.025)
Island3 <- c(0.05, 0.15, 0.7, 0.05, 0.05)

# calculate information entropy
entropy_fun <- function(p){-sum( p*log(p) )}
entropy_fun(Island1) # 1.609438
entropy_fun(Island2) # 0.7430039
entropy_fun(Island3) # 0.9836003
# Island 2 has the lowest entropy value and therefore the least uncertainty. This is probably 
# because Island 2 is dominated by Species A. 

# Answer, part 2, calculate the K-L Divergenence values for each island from the others.
# Divergence is calculated as the average difference in log probability between target (p) and model (q),
# so first we'll need to calculate average proportions.

# Average the proportions of Island 2 and 3
KLDivergence <- function(p, q){sum(p * log(p/q))}

Is1_from_Is2 <- KLDivergence(Island1, Island2) # 0.9704061
Is1_from_Is3 <- KLDivergence(Island1, Island3) # 0.6387604

Is2_from_Is1 <- KLDivergence(Island2, Island1) # 0.866434
Is2_from_Is3 <- KLDivergence(Island2, Island3) # 2.010914

Is3_from_Is1 <- KLDivergence(Island3, Island1) # 0.6258376
Is3_from_Is2 <- KLDivergence(Island3, Island2) # 1.838845

# Island 3, which has the highest entropy (see above) makes the best predictions (lowest divergence).
# Similarly, Island 2, which has the lowest entropy (see above), makes the worst predictions (highest
# divergence). This shows that more island entropy (uncertainty) leads to less certain out-of-sample
# predictions, and more dissimilar observations are less unexpected.

# 7H4. Recall the marriage, age, and happiness collider bias example from Chapter 6. Run models
# m6.9 and m6.10 again. Compare these two models using WAIC (or LOO, they will produce identical
# results). Which model is expected to make better predictions? Which model provides the correct
# causal inference about the influence of age on happiness? Can you explain why the answers to these
# two questions disagree?

# Answer, from chapter 6:
d <- sim_happiness( seed=1977 , N_years=1000 )
d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )
d2$mid <- d2$married + 1 

m6.9 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )

m6.10 <- quap( 
               alist(
                 happiness ~ dnorm( mu , sigma ),
                 mu <- a + bA*A,
                 a ~ dnorm( 0 , 1 ),
                 bA ~ dnorm( 0 , 2 ),
                 sigma ~ dexp(1)
               ) , data=d2 )

compare(m6.9, m6.10)

# WAIC indicates that model 6.9 is "better", however, we know from the last chapter that m6.9
# includes a collider, which leads to a spurious negative relationship between age and happiness.
# Therefore, if the goal is model inference, than the WAIC values should not be valued more than
# our causal model (m6.10), which does not condition on the collider.

# 7H5. Revisit the urban fox data, data(foxes), from the previous chapter’s practice problems. Use
# WAIC or PSIS based model comparison on five different models, each using weight as the outcome,
# and containing these sets of predictor variables:
# (1) avgfood + groupsize + area
# (2) avgfood + groupsize
# (3) groupsize + area
# (4) avgfood
# (5) area
# Can you explain the relative differences in WAIC scores, using the fox DAG from last week’s homework?
# Be sure to pay attention to the standard error of the score differences (dSE).

data(foxes)
d <- foxes
d$area_std <- scale(d$area)
d$avgfood_std <- scale(d$avgfood)
d$weight_std <- scale(d$weight)
d$groupsize_std <- scale(d$groupsize)

# (1) avgfood + groupsize + area
b7h5_1 <- quap(
  alist(
    weight_std ~ dnorm(mu, sigma),
    mu <- a + bFood * avgfood_std + bGroup * groupsize_std + bArea * area_std,
    a ~ dnorm(0, .2),
    c(bFood, bGroup, bArea) ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (2) avgfood + groupsize
b7h5_2 <- quap(
  alist(
    weight_std ~ dnorm(mu, sigma),
    mu <- a + bFood * avgfood_std + bGroup * groupsize_std,
    a ~ dnorm(0, .2),
    c(bFood, bGroup) ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (3) groupsize + area
b7h5_3 <- quap(
  alist(
    weight_std ~ dnorm(mu, sigma),
    mu <- a + bGroup * groupsize_std + bArea * area_std,
    a ~ dnorm(0, .2),
    c(bGroup, bArea) ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (4) avgfood
b7h5_4 <- quap(
  alist(
    weight_std ~ dnorm(mu, sigma),
    mu <- a + bFood * avgfood_std,
    a ~ dnorm(0, .2),
    bFood ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = d
)

# (5) area
b7h5_5 <- quap(
  alist(
    weight_std ~ dnorm(mu, sigma),
    mu <- a + bArea * area_std,
    a ~ dnorm(0, .2),
    bArea ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = d
)

# Comparison and plot results
compare(b7h5_1, b7h5_2, b7h5_3, b7h5_4, b7h5_5)
#         WAIC  SE    dWAIC  dSE pWAIC weight
# b7h5_1 323.3 16.89   0.0   NA   5.2   0.41
# b7h5_3 324.0 16.02   0.6 4.17   3.9   0.30
# b7h5_2 324.0 16.82   0.7 3.88   4.1   0.29
# b7h5_4 333.4 13.84  10.1 8.68   2.4   0.00
# b7h5_5 333.8 13.79  10.5 8.73   2.7   0.00
dev.off() # reset plotting device
plot(compare(b7h5_1, b7h5_2, b7h5_3, b7h5_4, b7h5_5))

# b7h5_1, b7h5_2, and b7h5_3 are almost the same in terms of out-of-sample deviance (open circles).
# These three models all use groupsize, which create pipes with area and/or avgfood, so as long as
# they include group size they give similar predictions (i.e, the effect of area while adjusting for groupsize
# is the same as the effect of avgfood while adjusting for groupsize).