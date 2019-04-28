# 538 Riddler - 04/26/2019

# Thanos, the all-powerful supervillain, can snap his fingers and destroy half of all the beings in the universe.
# 
# But what if there were 63 Thanoses, each snapping his fingers one after the other? 
# Out of 7.5 billion people on Earth, how many can we expect would survive?
#     
# If there were N Thanoses, what would the survival fraction be?
options(digits=5, scipen=100)
library(ggplot2)


population <- 7.5e9
n_thanos <- 63
prob_survival = 0.5

survivors <- population * (prob_survival ^ n_thanos)
survivors

#63 thanoses (thanii?) is enough to make it basically impossible to survive.

thanos_vec <- c(1:30)
survivor_vec <- population * (prob_survival ^ thanos_vec)
prob_survival_vec <- survivor_vec / population

gg_df <- data.frame(thanos_vec,survivor_vec,prob_survival_vec)
ggplot(gg_df, aes(thanos_vec,survivor_vec)) +
    geom_point() +
    theme_minimal() +
    ylab("number of survivors") +
    xlab("number of thanii")

