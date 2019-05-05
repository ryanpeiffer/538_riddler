# 538 Riddler Express - 05/26/2017

#Four co-workers carpool to work each day. One is selected randomly for the drive
# to work and again randomly for the drive home. Each has a lead foot, and each has 
# a chance of being ticketed for speeding. 
#     Driver A has a 10 percent chance of a ticket each time he drives, 
#     Driver B a 15 percent chance, 
#     Driver C a 20 percent chance 
#     and Driver D a 25 percent chance. 
# The state will immediately revoke the license of a driver after his or her third
# ticket, and a driver will stop driving in the carpool once his license is revoked. 
# Since there is only one police officer on the carpool route, a maximum of one ticket
# will be issued per morning and a max of one per evening.
# 
# Assuming that all four drivers start with no tickets, how many days can we expect 
# the carpool to last until all the drivers have lost their licenses?

#Answer = 77 trips or 38.5 days

library(tidyverse)


#===========================================================================
# setup initial variables
#===========================================================================

set.seed(6)     #choose a seed for random values
n_sims <- 1e5  #choose number of simulations to run

drivers <- c(1:4)
remaining_drivers <- drivers

ticket_chances <- c(.1, .15, .2, .25)
tickets <- rep(0,4)

driver_table <- tibble(drivers, ticket_chances, tickets)

carpool <- tibble(trip = numeric(), driver = numeric(), 
                  chance_caught = numeric(), caught = numeric(),
                  a.tickets = numeric(), b.tickets = numeric(), 
                  c.tickets = numeric(), d.tickets = numeric())

trip_cnt <- 1


#===========================================================================
# function to simulate one trip
#===========================================================================

sim_trip <- function(valid_drivers) {
    trip_driver <- sample(valid_drivers,1)
    chance_caught <- runif(1)
    caught <- ifelse(chance_caught < ticket_chances[trip_driver], 1, 0)
    
    trip_vec <- c(trip_driver, chance_caught, caught)

    return(trip_vec)
}

#===========================================================================
# function to simulate entire carpool
#===========================================================================

sim_carpool <- function() {
    
    #reset variables
    remaining_drivers <- drivers
    tickets <- rep(0,4)
    trip_cnt <- 1
    
    #simulate trips until 12 tickets
    while(sum(tickets) < 12) {   
        
        trip_res <- sim_trip(remaining_drivers)
        if(trip_res[3] == 1) {tickets[trip_res[1]] = tickets[trip_res[1]] + 1} #if caught, give that driver a ticket!

        remaining_drivers <- drivers[tickets < 3] #remove drivers who have 3 tickets
        
        trip_cnt <- trip_cnt + 1
    }
    
    return(trip_cnt-1)
}

#===========================================================================
# Create a dataframe that shows the entire carpool, each row = 1 trip
# used for testing/building... sim_carpool function faster for simulating many carpools
#===========================================================================

# while(sum(tickets) < 12) {   
#     
#     trip_res <- sim_trip(remaining_drivers)
#     if(trip_res[3] == 1) {tickets[trip_res[1]] = tickets[trip_res[1]] + 1} #if caught, give that driver a ticket!
#     
#     carpool <- add_row(carpool, trip = trip_cnt, driver = trip_res[1], 
#                        chance_caught = trip_res[2], caught = trip_res[3],
#                        a.tickets = tickets[1], b.tickets = tickets[2], 
#                        c.tickets = tickets[3], d.tickets = tickets[4])
#     
#     remaining_drivers <- drivers[tickets < 3] #remove drivers who have 3 tickets
#     
#     trip_cnt <- trip_cnt + 1
# }


#===========================================================================
# Simulate a bunch of carpools to get an answer
#===========================================================================

results <- replicate(n_sims, sim_carpool())
mean(results)

