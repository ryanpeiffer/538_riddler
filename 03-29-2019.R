#538 Riddler - March 29, 2019

####################################EXPRESS#######################################
#You are playing your first ever game of "Ticket to Ride," a board game in which players 
#compete to lay down railroad while getting so competitive they risk ruining their marriages. 
#At the start of the game, you are randomly dealt a set of three Destination Tickets out of a deck 
#of 30 different tickets. Each reveals the two terminals you must connect with a railroad to receive points. 
#During the game, you eventually pick up another set of three Destination Tickets, so you have now seen six 
#of the 30 tickets in the game.

#Later, because you enjoyed it so much, you and your friends play a second game. 
#The ticket cards are all returned and reshuffled. Again, you are dealt a set of three tickets to begin play. 
#Which is more likely: that you had seen at least one of these three tickets before, 
#or that they were all new to you?

n_trials <- 1e6

simulate_trains<-function() {
    first_round_cards <- sample(c(1:30),6,replace=FALSE)
    second_round_cards <- sample(c(1:30),3,replace=FALSE)
    
    test_match <- second_round_cards %in% first_round_cards

    return(min(sum(test_match),1)) #if at least 1 card matches return 1, else 0
}

rounds<-replicate(n_trials,simulate_trains())
prob_match <- sum(rounds)/length(rounds)
prob_match

#Looks like it may be exactly 50%? My answers are averaging slightly higher than 50% though.



#####################################CLASSIC##########################
#You are competing in a spelling bee alongside nine other contestants. You can each spell words perfectly 
#from a certain portion of the dictionary but will misspell any word not in that portion of the book. 
#Specifically, you have 99 percent of the dictionary down cold, and your opponents have 98 percent, 
#97 percent, 96 percent, and so on down to 90 percent memorized. The bee's rules are simple: 
#The contestants take turns spelling in some fixed order, which then restarts with the first surviving 
#speller at the end of a round. Miss a word and you're out, and the last speller standing wins. 
#The bee words are chosen randomly from the dictionary.
#
#First, say the contestants go in decreasing order of their knowledge, so that you go first. 
#What are your chances of winning the spelling bee? 
#
#Second, say the contestants go in increasing order of knowledge, so that you go last. 
#What are your chances of winning now?


#USER INPUTS-----------------------
num_bees <- 1e4
speller_order <- "decreasing" #choose 'decreasing' or 'increasing'


#DEFININING SIMULATION FUNCTION-------------------

#This runs fairly slowly (1000 sims takes ~ 5min), assuming it's due to creating the dataframe?
#See below for a new version of this that only uses vectors to be faster.
old_simulate_spelling_bee <- function(speller_abilities) {
    bee_finished <- FALSE
    game_rounds <- data.frame()
    
    while(!bee_finished) {
        #Simulate one round of spelling bee
        random_speller_values <- runif(10)
        round_results <- sign(speller_abilities-random_speller_values) #-1 is a miss, 1 is a hit
        
        #Adjust the vector of results for contestants who have already lost
        if(nrow(game_rounds)>0) {
            prior_results <- as.numeric(as.vector(game_rounds[nrow(game_rounds),]))
            round_results <- pmin(round_results,prior_results) 
        }
        
        #Append latest row to df
        game_rounds <- rbind(game_rounds,round_results)
        
        #Check if there is one or zero contestants left (multiple could get out in same round)
        if (sum(round_results)<=-8) {
            bee_finished <- TRUE
        }
        
    }
    spelling_bee_output <- list("rounds" = nrow(game_rounds),
                                "winner" = match(1,round_results))
    return(spelling_bee_output)
}  


#Faster version using vectors!
simulate_spelling_bee <- function(speller_abilities) {
    
    n_rounds <- 0
    bee_finished <- FALSE
    
    while(!bee_finished) {
        #Simulate one round of spelling bee
        random_speller_values <- runif(10)
        round_results <- sign(speller_abilities-random_speller_values) #-1 is a miss, 1 is a hit
        
        #Adjust the vector of results for contestants who have already lost
        if(n_rounds>0) {
            round_results <- pmin(round_results,prior_results) 
        }
        
        #Check if there is one or zero contestants left (multiple could get out in same round)
        if(sum(round_results)<=-8) {
            bee_finished <- TRUE
        }
        
        #create the prior_results vector to be used in the next round and increment n_rounds
        prior_results <- round_results
        n_rounds <- n_rounds+1
    }
    spelling_bee_output <- c(n_rounds,match(1,round_results)) #TODO determine winner when all out in same round
    return(spelling_bee_output)
}  


#GETTING TO AN ANSWER----------------------
spelling_bee_results <- data.frame(rounds=integer(),winner=integer())
speller_abilities <- c(99:90)/100
if(speller_order=="increasing") {
    speller_abilities <- rev(speller_abilities)
}

for(i in 1:num_bees) {
    spelling_bee_results[nrow(spelling_bee_results)+1,] <- simulate_spelling_bee(speller_abilities)
}

winner_table <- table(spelling_bee_results$winner)
win_prob <- ifelse(speller_order=="increasing",winner_table[10]/(sum(winner_table)),winner_table[1]/(sum(winner_table)))
win_prob

#Since we aren't capturing the results of bees where multiple contestants get out in the same round,
#we can't really determine how changing the order affects the win %.

#Looks like the probability of winning for the 99% speller is around 53%.










