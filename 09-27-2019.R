#538 Riddler Express - 9/27/2019

#If a baseball team is truly .500, meaning it has a 50 percent chance of winning
#each game, what's the probability that it has won two of its last four games
#and four of its last eight games?

set.seed(6)
p_win <- 0.5
n <- 1e6L

#initialize variables for more efficient looping
games <- c()
wins <- c()

#simulate rounds
for(i in 1:n) {
    games <- runif(8L)
    wins <- (games > p_win) * 1
    result <- result + ifelse(sum(wins[1:4]) == 2 & sum(wins) == 4, 1L, 0L)
    i = i + 1
}

#output answer
result/n



