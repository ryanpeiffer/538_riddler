# 538 Riddler Classic - 05/10/2019

#Five brothers join the Riddler Baseball Independent Society, or RBIs. Each of them enjoys a lengthy career of 20 seasons, 
#with 160 games per season and four plate appearances per game. (To make this simple, assume each plate appearance results 
#in a hit or an out, so there are no sac flies or walks to complicate this math.)

#Given that their batting averages are .200, .250, .300, .350 and .400, what are each brother's chances of beating DiMaggio's 
#56-game hitting streak at some point in his career? (Streaks can span across seasons.)

#By the way, their cousin has a .500 average, but he will get tossed from the league after his 10th season when he tests
#positive for performance enhancers. What are his chances of beating the streak?

library(tictoc)

#===========================================================================
# Setup variables
#===========================================================================

#user inputs
n_sims <- 1000
seed_int <- 42



#constants
n_bros <- 5
avgs <- c(.2, .25, .3, .35, .4)
seasons <- 20
games_per_season <- 160
ab_per_game <- 4

#calculations
total_games <- seasons * games_per_season
total_abs <- seasons * games_per_season * ab_per_game
set.seed(seed_int)


#===========================================================================
# tibble approach 
#===========================================================================

#idea: make a big ol table of all the hits for a season and do some sort of 
#cumsum to calc their streak.


#===========================================================================
# vector approach 
#===========================================================================
tic()
dimaggios <- rep(0, n_bros)

for (i in 1:n_sims) {
    streaks <- rep(0, n_bros)
    max_streak <- rep(0, n_bros)

    for (j in 1:total_games) {
        rands1 <- runif(n_bros)
        rands2 <- runif(n_bros)
        rands3 <- runif(n_bros)
        rands4 <- runif(n_bros)
        
        hits1 <- ifelse(rands1 < avgs, 1, 0)
        hits2 <- ifelse(rands2 < avgs, 1, 0)
        hits3 <- ifelse(rands3 < avgs, 1, 0)
        hits4 <- ifelse(rands4 < avgs, 1, 0)
        
        hits <- pmax(hits1, hits2, hits3, hits4)
        streaks <- streaks + 1
        streaks <- streaks * hits
        
        max_streak <- pmax(max_streak, streaks)
    }
    
    dimaggios <- dimaggios + (max_streak > 56)
    
}

dimaggios/n_sims
toc()
#My answers: 
# seed=6,  0.0%, 0.0%, 0.0%, 1.7%, 13.5%
# seed=42, 0.0%, 0.0%, 0.0%, 0.4%, 12.8%