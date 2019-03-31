#538 Riddler: Feb 8th, 2019
#This, naturally, raises an abstract mathematical question: 
#Given any three random integers - X, Y and Z - what are the chances that their product is divisible by 100?

n <- 1e5
x <- round(runif(n,1,10000))
y <- round(runif(n,1,10000))
z <- round(runif(n,1,10000))

products <- x*y*z

chances <- products%%100
chances <- replace(chances,chances>0,1)

1-sum(chances)/length(chances)

#Looks like the answer is in the ballpark of 12.4%.





# I wonder how I can make a vector of my probabilities and average those?
testInts <- function(n) {
    x <- round(runif(n,1,10000))
    y <- round(runif(n,1,10000))
    z <- round(runif(n,1,10000))
    
    products <- x*y*z
    
    chances <- products%%100
    chances <- replace(chances,chances>0,1)
    
    return(1-sum(chances)/length(chances))
    
}

probs <- replicate(100,testInts(1000))
mean(probs)

#Confirmed the answer is close to 12.4%. This method is probably more accurate since
#we are getting more simulations?

