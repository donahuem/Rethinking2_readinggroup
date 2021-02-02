#Statistical Rethinking
#Chapter 3 Practice Problems
#blame Lillian for errors/inaccuracies/etc.!

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100) #important if you want your answers to match mine exactly!
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

#"EASY"
#1 How much posterior probability lies below p = 0.2?
sum( samples < 0.2 ) / 1e4 #0.0004 = 0.04%

#2 How much posterior probability lies above p = 0.8?
sum( samples > 0.8 ) / 1e4 #0.1116 = 11.16%

#3 How much posterior probability lies between p = 0.2 and p = 0.8?
sum( samples > 0.2 & samples < 0.8 ) / 1e4 #0.888
#which is approximately equal to ...
sum(posterior[p_grid > 0.2 & p_grid < 0.8])
#increasing size = x in sample function above will get you closer to a match

#4 20% of the posterior probability lies below which value of p?
quantile( samples , 0.2 ) #0.5185185

#5 20% of the posterior probability lies above which value of p?
#1 - 0.2 = 0.8
quantile( samples , 0.8 ) #0.7557558

#6 Which values of p contain the narrowest interval equal to 66% of the 
#posterior probability?
#Answer: the HPDI function provides the narrowest interval containing the 
#specified probability mass
library(rethinking) #load library to access custom HPDI() and PI() functions
HPDI( samples , prob=0.66 ) #0.5085085, 0.7737738

#7 Which values of p contain 66% of the posterior probability, assuming equal 
#posterior probability both below and above the interval?
#Answer: the PI function provides the CENTRAL x% probability, so it is 
#appropriate here:
PI( samples , prob=0.66 ) #0.5025025, 0.7697698 (17, 83%)


#"MEDIUM"
#1 Suppose the globe tossing data had turned out to be 8 water in 15 tosses. 
#Construct the posterior distribution, using grid approximation. Use the same 
#flat prior as before.
p_grid <- seq( from=0 , to=1 , length.out=1000 ) #same
prior <- rep( 1 , 1000 ) #same
likelihood_new <- dbinom( 8 , size=15 , prob=p_grid ) #NEW
posterior_new <- likelihood_new * prior
posterior_new <- posterior_new / sum(posterior_new)
plot( posterior ~ p_grid , type="l" )

#2 Draw 10,000 samples from the grid approximation from above. Then use the 
#samples to calculate the 90% HPDI for p.10000
set.seed(100)
samples_new <- sample( p_grid , prob=posterior_new , size=1e4 , replace=TRUE )
HPDI( samples_new , prob=0.9 ) #0.3343343, 0.7217217

#3 Construct a posterior predictive check for this model and data. This means 
#simulate the distribution of samples, averaging over the posterior uncertainty 
#in p. What is the probability of observing 8 water in 15 tosses?
w <- rbinom( 1e4 , size=15 , prob=samples_new ) #object of 10000 simulations of 
#15 tosses each
sum( w==8 ) / 1e4 #0.1499; numerator is number of simulations in which exactly 8 
#of 15 tosses were water, denominator is total number of simulations
simplehist(w) #8 of 15 tosses being water is most common result in simulation

#4 Using the posterior distribution constructed from the new (8/15) data, now 
#calculate the probability of observing 6 water in 9 tosses.
w2 <- rbinom( 1e4 , size=9 , prob=samples_new )
sum( w2==6 ) / 1e4 #0.1842
simplehist(w2)

#5 Start over at 3M1, but now use a prior that is zero below p=0.5 and a 
#constant above p=0.5. This corresponds to prior information that a majority of 
#the Earth’s surface is water. Repeat each problem above and compare the 
#inferences. What difference does the better prior make? If it helps, compare 
#inferences (using both priors) to the true value p = 0.7.
p_grid <- seq( from=0 , to=1 , length.out=1000 ) #same
prior_new <- ifelse( p_grid < 0.5 , 0 , 1 ) #NEW
likelihood_new <- dbinom( 8 , size=15 , prob=p_grid ) #assuming 8/15 data
posterior_new2 <- likelihood_new * prior_new
posterior_new2 <- posterior_new2 / sum(posterior_new2)
plot( posterior_new2 ~ p_grid , type="l" ) #looks right -- exactly zero 
#posterior probability below p_grid = 0.5, due to new prior parameterization

set.seed(100)
samples_new2 <- sample( p_grid, prob=posterior_new2, size=1e4, replace=TRUE )
HPDI( samples_new2 , prob=0.9 ) #0.5005005, 0.7097097
#compare to 0.3343343, 0.7217217 -- shifts prob dist'n to right (above 0.5) and
#narrows the range

w3 <- rbinom( 1e4 , size=15 , prob=samples_new2 )
sum( w3==8 ) / 1e4 #0.163
simplehist(w3) # compare to simplehist(w) -- shifts prob dist'n to the right and
#narrows the range

w4 <- rbinom( 1e4 , size=9 , prob=samples_new2 )
sum( w4==6 ) / 1e4 #0.2353

#6 Suppose you want to estimate the Earth’s proportion of water very precisely. 
#Specifically, you want the 99% percentile interval of the posterior 
#distribution of p to be only 0.05 wide. This means the distance between the 
#upper and lower bound of the interval should be 0.05. How many times will you 
#have to toss the globe to do this?

#[MEGAN ADD YOUR ANSWER HERE]


#"HARD"

birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)
library(rethinking)
data(homeworkch3) #object 'homeworkch3' not found

#Compute total number of boys born across all births
boys <- sum(birth1) + sum(birth2)

#1 Using grid approximation, compute the posterior distribution for the 
#probability of a birth being a boy. Assume a uniform prior probability. Which 
#parameter value maximizes the posterior probability?
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 ) #the first number here (1) can be anything other than 0
#to produce a uniform prior
likelihood_boy <- dbinom( boys , size=200 , prob=p_grid )
posterior_boy <- likelihood_boy * prior
posterior_boy <- posterior_boy / sum(posterior_boy)
plot( posterior_boy ~ p_grid , type="l" )
p_grid[ which.max(posterior_boy) ] #0.5545546

#2 Using the sample function, draw 10,000 random parameter values from the 
#posterior distribution you calculated above. Use these samples to estimate 
#the 50%, 89%, and 97% highest posterior density intervals.
samples_boy <- sample( p_grid , prob=posterior_boy , size=1e4 , replace=TRUE )
HPDI( samples_boy , prob=0.50 ) #0.5255255 0.5725726
HPDI( samples_boy , prob=0.89 ) #0.5005005 0.6106106
HPDI( samples_boy , prob=0.97 ) #0.4734735 0.6276276

#3 Use rbinom to simulate 10,000 replicates of 200 births. You should end up 
#with 10,000 numbers, each one a count of boys out of 200 births. Compare the 
#distribution of predicted numbers of boys to the actual count in the data (111 
#boys out of 200 births). There are many good ways to visualize the simulations, 
#but the dens command (part of the rethinking package) is probably the easiest 
#way in this case. Does it look like the model fits the data well? That is, does
#the distribution of predictions include the actual observation as a central, 
#likely outcome?
sim_boy <- rbinom(10000, size = 200, prob = samples_boy)
dens(sim_boy)
abline(v=boys, col="red", lty=2) #looks great -- red line is observed outcome

#4 Now compare 10,000 counts of boys from 100 simulated first borns only to the 
#number of boys in the first births, birth1. How does the model look in this 
#light?
sim_boy2 <- rbinom(10000, size = 100, prob = samples_boy)
dens(sim_boy2)
abline(v=sum(birth1), col="red", lty=2) #less accurate model  -- red line is 
#observed outcome

#5 The model assumes that sex of first and second births are independent. To 
#check this assumption, focus now on second births that followed female first 
#borns. Compare 10,000 simulated counts of boys to only those second births that 
#followed girls. To do this correctly, you need to count the number of first 
#borns who were girls and simulate that many births, 10,000 times. Compare the 
#counts of boys in your simulations to the actual observed count of boys 
#following girls. How does the model look in this light? Any guesses what is 
#going on in these data?
birthorder_0x <- birth2[birth1==0]
sim_0x <- rbinom(10000, size = length(birthorder_0x), prob = samples_boy)
dens(sim_0x)
abline(v=sum(birthorder_0x), col="red", lty=2) #yikes -- non-independence of sex
#of first and second children born to same families? 
#Check out https://www.biorxiv.org/content/10.1101/031344v3:
#"There was a positive correlation between the sexes of successive siblings 
#(coefficient = 0.067, p < 0.001), i.e. a child was more likely to be of the 
#same sex as its preceding sibling."
