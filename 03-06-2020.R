#538 Riddler Express - 3/6/2020

# If he randomly places all nine pieces in the nine slots on the tic-tac-toe
# board (with one piece in each slot), what's the probability that X wins? That
# is, what's the probability that there will be at least one occurrence of three
# Xs in a row at the same time there are no occurrences of three Os in a row?



# USER INPUTS #
set.seed(6)
n_iterations <- 1e6



tic_tac_toe <- function() {
    
    #build basic tic tac toe board with 5 X's (1) and 5 O's (0)
    pieces <- 1:9
    selection <- sample(pieces, 9, replace = FALSE) %% 2
    board <- matrix(data = selection, nrow = 3, ncol = 3)
    
    #preallocate list before for loop
    lines <- vector(mode = "list", length = 8)
    
    #add rows and cols to lines list
    for (i in 1:3) {
        row <- board[i, 1:3]
        col <- board[1:3, i]
        lines[[i]] <- row
        lines[[i + 3]] <- col 
    }
    
    #add diags to lines list
    diag1 <- c(board[1,1], board[2,2], board[3,3])
    diag2 <- c(board[3,1], board[2,2], board[1,3])
    lines[[7]] <- diag1
    lines[[8]] <- diag2
    
    # check to see who wins!
    x_wins <- FALSE
    line_sums <- sapply(lines, FUN = sum)
    if(max(line_sums) == 3 & min(line_sums) != 0) {x_wins <- TRUE}
    return(x_wins)
}


#run many iterations to get a probability
games <- replicate(n_iterations, tic_tac_toe())
sum(games)/length(games)

#results
#running 1M iterations took about 20 seconds (slower than I want!)
#calculated probability is about 49.2%