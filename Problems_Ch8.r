library(rethinking)

# Chapter 8 - Interactions

########################
### 8E1: For each of the causal relationships below, name a hypothetical
# third variable that would lead to an interaction effect.

# (1) Bread dough rises because of yeast
        # sugar
# (2) Education leads to higher income
        # type of degree
# (3) Gasoline makes a car go
        # but not if wheels are missing

########################
### 8E2: Which of the following explanations invokes an interaction
#   (1) - only one that has AND in sentence :)...the effect of heat depends on moisture

########################
### 8E3: Write linear model for each of 8E2
#   (1) Carmelization ~ heat * moisture 
#   (2) Speed ~ cylinders + fuel injector
#   (3) Belief system ~ parents + friends
#   (4) Intelligence ~ social + manipulative appendages

########################
### 8M1: (Tulips example) Add a temperature treatment (hot and cold) and find that none of the 
# plants grown under the hot temp developed any blooms, regardless of water and shade levels.
# Can this result be explained in terms of an interaction between water, shade, and temperature?
# Original study data was collected under cold temperatures.

#     `With temperature apparently influencing both shade and water then this could be a 
#       3-way interaction.

########################
### 8M2: Can you invent a regression equation that would make the bloom size zero, whenever the 
# temperature is hot?

#       If temperature is either 0 (cold) and 1 (hot) then anytime the value is 1 it needs to set 
#         the mean to zero?

########################
### 8M3: Ravens depend on wolves for food. Can you invent a hypothetical set of data on raven population
# size in which this relationship would manifest an interaction? 

#       Create data points such that wolves and ravens increase together...ex. for every one wolf 
#         there are 5 ravens.

# Do you think the biological interaction could be linear? Why or why not?

#         If all ravens are lazy and/or have behaviorally become dependent on wolves
#           or there are no small prey around that the ravens could kill themselves
#           then could be linear. 

########################
### 8M4: Repeat the tulips analysis but this time use priors that constrain the effect of water to 
# be positive and the effect of shade to be negative. Use prior predictive simulation. What do these
# prior assumptions mean for the interaction prior, if anything?

## Don't quote me this question :)....could be completely wrong!

data(tulips)
d<-tulips
str(d)

d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

tulip_original <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a ~ dnorm( 0.5, 0.25) ,
    bw ~ dnorm( 0 , 0.25 ) , 
    bs ~ dnorm( 0 , 0.25 ) , 
    bws ~ dnorm( 0 , 0.25 ) ,
    sigma ~ dexp( 1 )
  ),
  data = d)

precis(tulip_original)


tulip_constrain <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a ~ dnorm( 0.5, 0.25) ,
    bw ~ dnorm( 1 , 0.25 ) , # constrain to + ?
    bs ~ dnorm( -1 , 0.25 ) , # constrain to - ?
    bws ~ dnorm( 0 , 0.25 ) ,
    sigma ~ dexp( 1 )
  ),
  data = d)

precis(tulip_constrain)
plot(coeftab(tulip_original,tulip_constrain))

### hardly change anything so perhaps this is wrong...
### Suggested to maybe integrate a logNormal to force at least positive....

########################
### 8H1: Include bed as a predictor in interactions mode but include only as a main effect

d$bed_index <- coerce_index(d$bed)

tulip_bed <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a[bed_index] + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a[bed_index] ~ dnorm(0.5,0.25),
    bw ~ dnorm(0,0.25),
    bs ~ dnorm(0,0.25),
    bws ~ dnorm(0,0.25),
    sigma ~ dexp( 1 )
  ),
  
  data = d
)

precis(tulip_bed, depth = 2)

# beds b and c did better than a

########################
### 8H2: Use WAIC to compare 8H1 model and original without bed

compare(tulip_original, tulip_bed)
plot(coeftab(tulip_original, tulip_bed))
# nearly the same so bed doesn't help the prediction, treatment matters...

# Can you reconcile the results with the posterior distribution of the bed coefficients?
# ????? page 246 in book
postBed <- extract.samples(tulip_bed)
diff_a1_a2 <- postBed$a[,1] - postBed$a[,2]
PI( diff_a1_a2 )
diff_a2_a3 <- postBed$a[,2] - postBed$a[,3]
PI( diff_a2_a3 )
diff_a1_a3 <- postBed$a[,1] - postBed$a[,3]
PI( diff_a1_a3 )

# No difference between b and c but this isn't answering the question...errr

dens(postOriginal$a, col = "orange", xlim = c(0.10,0.60))
dens(postBed$a[,1], col = "lightblue", add = T)
dens(postBed$a[,2], col = "blue", add = T)
dens(postBed$a[,3], col = "cyan", add = T)



#### here's my reconcilation....totally cheated from this point forward...googled for answers cause 
# was annoyed with the amount of time it was taking me to answer the questions...
# BUT found these awesome websites! These may help others...

# https://www.erikkusch.com/post/rethinking/statistical-rethinking-chapter-08/#medium-exercises
#  https://jmgirard.com/statistical-rethinking-ch7/
# http://xcelab.net/rmpubs/rethinking/rethinking_solutions_2.pdf

#### Since they give answers to H3 and H4, you can see it there.


########################
### 8H5: Model score against double index of wine and judge
## Don't quote me on any of the answers from here on out...could all be completely wrong!

data("Wines2012")
wino<-Wines2012
wino$judge_index <- coerce_index(as.numeric(wino$judge))
wino$wine_index <- coerce_index(as.numeric(wino$wine))

plot(score ~ judge_index, data = wino)
plot(score ~ wine_index, data = wino)

dens(wino$score)
# Looks like scores are between 0 and 20
mean(wino$score) # 15
range(wino$score) # 7 to 19.5

aggregate(score ~ wine,FUN=mean, data = wino)
aggregate(score ~ wine,FUN=var, data = wino)
aggregate(score ~ judge,FUN=mean, data = wino)
aggregate(score ~ judge,FUN=var, data = wino)

wino$score.scale<-scale(wino$score)
mean(wino$score.scale) # 
max(wino$score.scale)-min(wino$score.scale)
range(wino$score.scale) # -2.7 to 1.9
# so making prior mean of 0 and std 2

vino <- quap(alist(
  score.scale ~ dnorm(mu, sigma),
  mu <- a + bW*wine_index + bJ*judge_index,
  a ~ dnorm(0, 1),
  bW[wine_index] ~ dnorm(0, 2),
  bJ[judge_index] ~ dnorm(0, 2),
  sigma ~ dexp(1)
),
data = wino
)
precis(vino, depth = 2)

a<-c("a")
labelsJudge2 <-paste("bJ[",1:9,"]:", levels(wino$judge), sep="") 
labelsWine2 <-paste("bW[",1:20,"]:", levels(wino$wine), sep="")
sig<-c("sig")
labelstotal2<-c(a,labelsJudge2,labelsWine2,sig)
plot(coeftab(vino), labels = labelstotal2)

# H6 use flight, wine.amer and judge.amer

# one = American, 2 = French
wino$wineAmer<-ifelse(wino$wine.amer == 0, 2, 1)
wino$judgeAmer<-ifelse(wino$judge.amer == 0, 2, 1)
# one = Red, 2 = White
wino$flightAlt<-coerce_index(as.numeric(wino$flight))

vino2 <- quap(alist(
  score.scale ~ dnorm(mu, sigma),
  mu <- a + bWA*wineAmer + bJA*judge.amer + bF*flightAlt,
  a ~ dnorm(0, 1),
  bWA[wineAmer] ~ dnorm(0, .5),
  bJA[judge.amer] ~ dnorm(0, .5),
  bF[flightAlt]~ dnorm(0, .5),
  sigma ~ dexp(1)
),
data = wino
)
precis(vino2, depth = 2)
plot(coeftab(vino2))

