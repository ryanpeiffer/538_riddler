#Riddler Express 11/13/2020

#You're playing the (single) Jeopardy! Round, and your opponents are simply no match for you. You choose first and never relinquish control, working your way horizontally across the board by first selecting all six $200 clues, then all six $400 clues, and so on, until you finally select all the $1,000 clues. You respond to each clue correctly before either of your opponents can.
#One randomly selected clue is a Daily Double. Rather than award you the prize money associated with that clue, it instead allows you to double your current winnings or wager up to $1,000 should you have less than that. Being the aggressive player you are, you always bet the most you can. (In reality, the Daily Double is more likely to appear in certain locations on the board than others, but for this problem assume it has an equal chance of appearing anywhere on the board.)
#How much money do you expect to win during the Jeopardy! round?

basic_bet <- sort(rep(c(200,400,600,800,1000),6))
dd_bet <- cumsum(basic_bet) - basic_bet
dd_bet <- pmax(1000, dd_bet)

m1 <- diag(30)
basic_mat <- matrix(data = basic_bet, nrow = 30, ncol = 30)

cumsum_mat <- matrix(data = dd_bet, nrow = 30, ncol = 30)
cumsum_mat <- cumsum_mat * m1

bets <- pmax(basic_mat, cumsum_mat)

scores <- apply(bets, MARGIN = 2, FUN = sum)
answer <- mean(scores)
answer

#Extra credit: Suppose you change your strategy. Instead of working your way horizontally across the board, you select random clues from anywhere on the board, one at a time. Now how much money do you expect to win during the Jeopardy! round?
library(tictoc)

set.seed(6)
n <- 10e5
dd_squares <- sample(1:30, n, replace = TRUE)


jeopardy <- function() {
    round <- sample(basic_bet) #randomizes order 
    dd_bets <- pmax(cumsum(round) - round, 1000)
    dd_square <- sample(1:30, 1)
    round[dd_square] <- dd_bets[dd_square]
    return(sum(round))
}

tic()
scores2 <- replicate(n, jeopardy())
toc()
answer2 <- mean(scores2)
answer2
