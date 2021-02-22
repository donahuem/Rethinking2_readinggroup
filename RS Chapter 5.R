##########
#CHAPTER 5#
##########

library (rethinking)

#Easy
#see pdf

#Medium
#5M1
#example spurious correlation between the number hours training and goals during season that vanishes when player income is included
#Use simulation from page 144
N <- 100 # number of cases
income <- rnorm(n = 100, mean = 0, sd = 1) # x_real as Gaussian with mean 0 and stddev 1
traininghours <- rnorm(n = N, mean = income, sd = 2) # x_spur as Gaussian with mean=x_real
goals <- rnorm(n = N, mean = income, sd = 1) # y as Gaussian with mean=x_real
d <- data.frame(goals, traininghours, income)# bind all together in data frame
pairs(d)
#show that there is a (spurious) association between number of hours training and goals scored.
m <- quap(
  alist(
    goals ~ dnorm(mu, sigma),
    mu <- a + bo * traininghours,
    a ~ dnorm(0, 5),
    bo ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)
#show that this association vanishes when player income is added to the model
m <- quap(
  alist(
    goals ~ dnorm(mu, sigma),
    mu <- a + bo * traininghours + bi * income,
    a ~ dnorm(0, 5),
    bo ~ dnorm(0, 5),
    bi ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

#5M2
#example of a masked relationship involving the prediction of happiness ratings from the amount of waffles one eats and the amount of time feeling sick due to overeating.
#Use simulation from page 156
N <- 100
rho <- 0.8
waffles <- rnorm(n = N, mean = 0, sd = 1)
sick <- rnorm(n = N, mean = rho * waffles, sd = sqrt(1 - rho^2))
happiness <- rnorm(n = N, mean = waffles - sick, sd = 1)
d <- data.frame(happiness, waffles, sick)
pairs(d)

# Due to the masking relationship, the bivariate associations should be weak/widely variable.
m <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a + ba * waffles,
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

m <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a + bi * sick,
    a ~ dnorm(0, 5),
    bi ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

#In the multivariate regression, the masking relationship should be resolved.
m <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a + ba * waffles + bi * sick,
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 5),
    bi ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

#5M4
#Add new column in WaffleDivorce data frame with percent Mormons per State
data(WaffleDivorce)
d <- WaffleDivorce
d$LDS <- c(0.0077, 0.0453, 0.0610, 0.0104, 0.0194, 0.0270, 0.0044, 0.0057, 0.0041, 0.0075, 0.0082, 0.0520, 0.2623, 0.0045, 0.0067, 0.0090, 0.0130, 0.0079, 0.0064, 0.0082, 0.0072, 0.0040, 0.0045, 0.0059, 0.0073, 0.0116, 0.0480, 0.0130, 0.0065, 0.0037, 0.0333, 0.0041, 0.0084, 0.0149, 0.0053, 0.0122, 0.0372, 0.0040, 0.0039, 0.0081, 0.0122, 0.0076, 0.0125, 0.6739, 0.0074, 0.0113, 0.0390, 0.0093, 0.0046, 0.1161)
d$logLDS <- log(d$LDS)
d$logLDS.s <- (d$logLDS - mean(d$logLDS)) / sd(d$logLDS)
simplehist(d$LDS)

#log transform these data
simplehist(d$logLDS)

#standardize
simplehist(d$logLDS.s)

#multiple regression model.
m <- quap(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bm * Marriage + ba * MedianAgeMarriage + bl * logLDS.s,
    a ~ dnorm(10, 20),
    bm ~ dnorm(0, 10),
    ba ~ dnorm(0, 10),
    bl ~ dnorm(0, 10),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

#plot posterior distributions
plot( coeftab(m), par=c("ba","bm", "bl") )

#Hard
data("foxes")
#5H1
#OPTION 1: body weight as a linear function of territory size
#use model on p.139
d <- foxes
ma <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area,
    a ~ dnorm(5, 5),
    ba ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(ma)

#plot regression + MAP regression line + the 95% interval of the mean
area.seq <- seq(from = min(d$area), to = max(d$area), length.out = 30)
mu <- link(ma, data = data.frame(area = area.seq))

mu.PI <- apply(mu, 2, PI, prob = 0.95)
plot(weight ~ area, data = d, col = rangi2)
abline(ma)
shade(mu.PI, area.seq)

#OPTION 2: body weight as a linear function of group size
mg <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bg * groupsize,
    a ~ dnorm(5, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(mg)

#plot regression + MAP regression line + the 95% interval of the mean
groupsize.seq <- seq(from = min(d$groupsize), to = max(d$groupsize), length.out = 30)
mu <- link(mg, data = data.frame(groupsize = groupsize.seq))
mu.PI <- apply(mu, 2, PI, prob = 0.95)
plot(weight ~ groupsize, data = d, col = rangi2)
abline(mg)
shade(mu.PI, groupsize.seq)

#5H2
#regression model
mag <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area + bg * groupsize,
    a ~ dnorm(5, 5),
    ba ~ dnorm(0, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(mag)

#counterfactual plot for territory areamodel (p.145)
G.avg <- mean(d$groupsize)
A.seq <- seq(from = 0, to = 6, length.out = 30)
pred.data <- data.frame(
  groupsize = G.avg,
  area = A.seq
)
mu <- link(mag, data = pred.data)

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)
A.sim <- sim(mag, data = pred.data, n = 1e4)

A.PI <- apply(A.sim, 2, PI)
plot(weight ~ area, data = d, type = "n")
mtext("groupsize = 4.345")
lines(A.seq, mu.mean)
shade(mu.PI, A.seq)
shade(A.PI, A.seq)

#counterfactual plot for group size
A.avg <- mean(d$area)
G.seq <- seq(from = 1, to = 10, length.out = 30)
pred.data <- data.frame(
  groupsize = G.seq,
  area = A.avg
)
mu <- link(mag, data = pred.data)

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)
G.sim <- sim(mag, data = pred.data, n = 1e4)

G.PI <- apply(G.sim, 2, PI)
plot(weight ~ groupsize, data = d, type = "n")
mtext("area = 3.169")
lines(G.seq, mu.mean)
shade(mu.PI, G.seq)
shade(G.PI, G.seq)

#5H3
#MODEL 1: body weight as an additive function of avgfood and groupsize 
m <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bf * avgfood + bg * groupsize,
    a ~ dnorm(5, 5),
    bf ~ dnorm(0, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

#MODEL 2: body weight as an additive function of all three variables (avgfood, groupsize and area)
m <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bf * avgfood + bg * groupsize + ba * area,
    a ~ dnorm(5, 5),
    bf ~ dnorm(0, 5),
    bg ~ dnorm(0, 5),
    ba ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

#Question a
#calculate standard slope estimate for average food
d$avgfood.s <- (d$avgfood - mean(d$avgfood)) / sd(d$avgfood)
m <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bf * avgfood.s + bg * groupsize,
    a ~ dnorm(5, 5),
    bf ~ dnorm(0, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)

#calculate standard slope estimate for territory area
d$area.s <- (d$area - mean(d$area)) / sd(d$area)
m <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area.s + bg * groupsize,
    a ~ dnorm(5, 5),
    ba ~ dnorm(0, 5),
    bg ~ dnorm(0, 5),
    sigma ~ dunif(0, 5)
  ),
  data = d
)
precis(m)


