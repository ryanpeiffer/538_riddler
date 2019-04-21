# 538 Riddler - 4/19/2019

#Riddler Classic
# If N points are generated at random places on the perimeter of a circle,
# what is the probability that you can pick a diameter such that all of those
# points are on only one side of the newly halved circle?

#after playing around, it looks like once n>20 the probability is near 0
n_vals <- rep(1:20) #vector of different values of n to test
percents <- rep(0,length(n_vals))
n_trials <- 1e5 #number of sims per value of n

for (i in 1:length(n_vals)) {
    n <- n_vals[i]
    rand_points <- runif(n*n_trials) * 360 #generate a vector of random points on circle
    wins <- 0 #let a "win" be when the points can be captured by one diameter line
    for (j in 1:n_trials) {
        max_point <- max(rand_points[((j-1)*n+1):(j*n)])
        min_point <- min(rand_points[((j-1)*n+1):(j*n)])
        if(max_point - min_point <= 180) {wins <- wins + 1}
    }
    percents[i] <- wins/n_trials
}

plot(n_vals,percents)

#don't love that I have this double for loop... but this solution is much
#better than my first pass at it! Should try to continue vectorizing this.

#Now we have a vector of values for N and percent change of it working, how to determine relationship...
#Shape of our plot suggests percent decreases exponentially with n...


