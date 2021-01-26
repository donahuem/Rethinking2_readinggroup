#Chapter 2 Practice Problems

#Easy
#1  the probability of rain on Monday:  P(rain|Monday) = P(rain,Monday)/P(Monday)
#2  Pr(Monday|rain)  The probability that it is Monday, given it is raining
#3  the probability that it is Monday, given that it is raining
#     Pr(Monday|Rain) = Pr(rain|Monday)Pr(Monday)/Pr(rain)
#4  "the probability of water is 0.7"  really, this should be the probabiliy
#    of observing water on a toss is 0.7.  There is a true (certain) value of the proportion of water.

#Medium
#2 - grid approximation
L <- 20  #grid step size
clr <- c("red","yellow","blue")
obs <- list(c(3,0),c(3,1),c(5,2))

#define grid for p between 0 and 1 in L steps
p_grid <- seq(from=0,to=1,length=L)

#define uniform prior as equal at all prior values of p
prior <- list(prior1 = rep(1,L), 
              prior2 = c(rep(0,L/2),rep(1,L/2)))

#compute likelihood at each grid value
for (j in 1:length(prior)){
  for (i in 1:length(obs)) {
    #binomial prob of the observed value of 6 W of 9 observations given each value of p
    like <- dbinom(obs[[i]][1],sum(obs[[i]]),prob=p_grid)
    #compute posterior so it sums to 1
    post <- like*prior[[j]]/sum(like*prior[[j]])
    if(i==1) {
      plot(p_grid,post,type="b",col=clr[i],
           xlab="prob of water",ylab="posterior prob")
          title = paste0("prior ", j)
      } else {
      lines(p_grid,post,type="b", col=clr[i])
      }
  }
}

#2M3
# P(land|earth) = 0.3
# P(land|mars) = 1.0
# P(earth) = 0.5
# P(mars) = 0.5
# posterior prob of earth given land
# P(earth|land) = P(land|earth)*P(earth)/P(land)
P_earth_given_land <- 0.3 * 0.5 / (0.3*0.5 + 1.0 * 0.5)

#2H1
#P(twins|Panda1) = 0.1
#P(twin|Panda2) = 0.2
#Obs of twins, P(next birth is twins)

