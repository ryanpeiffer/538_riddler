# 538 Riddler Classic - 05/31/2019

#Probability one person ends with an empty grid

library(tictoc)

#===========================================================================
#setup initial variables 
#===========================================================================

n_trials <- 100000
cards <- 54

#===========================================================================
# define loteria function 
#===========================================================================

loteria <- function(cards) {
    board1 <- sample(1:cards, 16)
    board2 <- sample(1:cards, 16)
    
    draws <- sample(1:cards, cards)

    b1_hits <- match(board1, draws)
    b2_hits <- match(board2, draws)
    
    ending_draw <- min(max(b1_hits), max(b2_hits))
    no_blanks <- min(min(b1_hits), min(b2_hits))
    
    #return whether one card is completed before the other gets a single picture drawn
    ending_draw < no_blanks 
}

tic()    
trials <- replicate(n_trials, loteria(cards))
toc()
    
trials <- trials * 1
sum(trials) / length(trials)

#I think this solution works, but the probability is so damn small that it's tough to really validate.
