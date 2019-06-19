# 538 Riddler Express - 02/02/2018

#You and your spouse each take two gummy vitamins every day. You share a single bottle of 60 vitamins, 
#which come in two flavors. You each prefer a different flavor, but it seems childish to fish out two
#of each type (but not to take gummy vitamins). So you just take the first four that fall out and then 
#divide them up according to your preferences. For example, if there are two of each flavor, you and 
#your spouse get the vitamins you prefer, but if three of your preferred flavor come out, you get two 
#of the ones you like and your spouse will get one of each.

#The question is, on average, what percentage of the vitamins you take are the flavor you prefer? 
#(Assume that the bottle starts out with a 50-50 split between flavors, and that the four selected 
#each day are selected uniformly at random.)


#===========================================================================
# setup intial variables
#===========================================================================

#user inputs
seed <- 6
n_trials <- 10000

#constants
vitamins <- c(1:60)



#===========================================================================
# function to simulate going through a bottle of vitamins
#===========================================================================

pop_a_bottle <- function(bottle) {
    favs <- 0
    while(length(bottle) > 3) {
        #get a day worth of vitamins and remove them from the bottle
        today <- sample(bottle, 4)
        bottle <- bottle[!bottle %in% today]
        
        #count how many are my preferred flavor (let's call odd numbers my preferred flavor)
        mine <- today %% 2
        favs <- favs + min(sum(mine), 2) #cap at 2 vitamins consumed per day
    }    
    favs
}


#===========================================================================
# run simulations and get result 
#===========================================================================

set.seed(seed)
results <- replicate(n_trials, pop_a_bottle(vitamins))
total_pills <- 30 * n_trials #n_trials is the number of bottles, and we take 30 pills per bottle
sum(results) / total_pills


