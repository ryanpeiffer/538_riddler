#fiddler 8-18-2023

#   You are the manager for the New York Frets, a baseball team that has woefully underperformed
#   this season. In an effort to right the ship, you are tinkering with the batting order.
#   
#   Eight of your nine batters are "pure contact" hitters. One-third of the time, 
#   each of them gets a single, advancing any runners already on base by exactly one base. 
#   (Your team is very slow on the base paths. That means no one is fast enough to score 
#   from first or second base on a single-only from third). The other two-thirds of the time, 
#   they record an out, and no runners advance to the next base.
#
#   Your ninth batter is the slugger. One-tenth of the time, he hits a home run.
#   But the remaining nine-tenths of the time, he strikes out.
#
#   Your goal is to score as many runs as possible, on average, in the first inning.
#   Where in your lineup (first, second, third, etc.) should you place your home run slugger?

set.seed(54)


sim_inning <- function(hitters) {
    outs <- 0
    runs <- 0
    bases <- c(0,0,0) #to track if runners on 1st, 2nd, 3rd
    
    at_bats <- runif(50) #picking 50 as an arbitrary high number of ABs that we should never exceed
    current_batter <- 1
    
    #for loop for now, but should vectorize later
    for (at_bat in at_bats) {
        if(at_bat > hitters[current_batter]) {
            outs <- outs + 1
            if(outs == 3) {break} #once we have 3 outs, we can exit the loop. inning is over.
        } else {
            if(hitters[current_batter] == .1) {
                #home run!
                runs <- runs + sum(bases) + 1
                bases <- c(0,0,0)
            } else {
                #single
                if(bases[3] == 1) {runs <- runs + 1}
                bases[3] <- bases[2]
                bases[2] <- bases[1]
                bases[1] <- 1
            }
        }
        current_batter <- ifelse(current_batter == 9, 1, current_batter + 1)
    }
    return(runs)
}

#run sims for slugger in each position
avg_runs <- rep(0,9)
for(batter in c(1:9)) {
    hitters <- rep(.333, 9)
    hitters[batter] <= .1
    
    sim_result <- replicate(1e6, sim_inning(hitters))
    avg_runs[batter] <- sum(sim_result) / length(sim_result)
}

#results are showing as pretty darn stable, no clear winner!?
