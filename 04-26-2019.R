# 538 Riddler - 04/26/2019

# Thanos, the all-powerful supervillain, can snap his fingers and destroy half of all the beings in the universe.
# 
# But what if there were 63 Thanoses, each snapping his fingers one after the other? 
# Out of 7.5 billion people on Earth, how many can we expect would survive?
#     
# If there were N Thanoses, what would the survival fraction be?


population <- 7.5e9
n_thanos <- 63
prob_survival <- 0.5


#simplest answer: Thanos snaps only affect humans
remaining_pop1 <- floor(population * prob_survival^n_thanos)
#answer = 0


#slightly more nuanced answer: each Thanos snap destroys exactly 50% of all humans and exactly 50% of all thanii
snap_yo_finger <- function(population, n_thanos) {
    pop <- population
    thanos_count <- n_thanos
    while(thanos_count > 0) {
        thanos_count <- thanos_count - 1 #reduce thanos count by 1 since this one has now snapped
        pop <- floor(pop * prob_survival)
        thanos_count <- floor(thanos_count * prob_survival) #half of the unsnapped thanii also get obliterated
    }
    return(pop)
}

remaining_pop2 <- snap_yo_finger(population, n_thanos)
#answer = 117,187,500 ... a much nicer situation than the first!


#ultimate nuance: assume the universe of all beings is humans+thanii. Randomly simulate whether each living Thanos is
#destroyed by a given snap. Theoretically this means the number of humans shouldn't get cut exactly in half, but I'm
#going to keep it simple. You could also go down a rabbit hole of saying the # beings in the universe is likely infinite,
#so you should really simulate whether each human falls into the destroyed half.... but that's too much for me.




