#538 Riddler Express - 9/6/2019

#You and your friend are playing a game of "Acchi, Muite, Hoi" (or what some on
#Twitter have been calling the "lookaway challenge"). In the first round, you 
#point in one of four directions: up, down, left or right. At the exact same 
#time, your friend also looks in one of those four directions. If your friend 
#looks in the same direction you're pointing, you win! Otherwise, you switch
#roles as the game continues to the next round - now your friend points in a 
#direction and you try to look away. As long as no one wins, you keep switching
#off who points and who looks.

#It seems like your chances of winning should be 50 percent, since there are 
#exactly two players. But surely it's not that simple. If both you and your 
#friend choose your directions randomly in each round, what are your chances of 
#winning?

#===========================================================================
# setup
#===========================================================================

library(tictoc)

#user inputs
set.seed(6)
n <- 10e3


#===========================================================================
# define function for simulation
#===========================================================================

acchi_muite_hoi <- function(n) {
    dirs <- 1:4
    round <- 1
    counter <- 1
    wins <- 0
    games <- 0
    
    while (round <= n) {
        my_dir <- sample(dirs, 1, replace = TRUE)
        opp_dir <- sample(dirs, 1, replace = TRUE)
        
        if(my_dir == opp_dir) {
            if(counter %% 2 != 0) {
                wins <- wins + 1  #we win on odd rounds, opponent wins on even rounds
            }
            counter <- 1 #reset counter so we can correctly assign winner next game
            games <- games + 1 #need to track number of complete games to calc win %
        } else {
            counter <- counter + 1 #game continues on another round if no winner!
        }
        
        round <- round + 1
    }
    
    paste0("My win %: ", sum(wins)/sum(games))
}

#===========================================================================
# conclusion
#===========================================================================

tic()
acchi_muite_hoi(1e6)
toc()

#able to run 1 million rounds worth in ~ 7 seconds
#my win percent is somewhere around 57%, probably greater than 50% due to first round advantage


