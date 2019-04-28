# 538 Riddler - 12/07/2018

# Riddler Express
# From Josh Vandenham, precipitation permutations:
#     
#Louie walks to and from work every day. In his city, there is a 50 percent chance of rain 
#each morning and an independent 40 percent chance each evening. His habit is to bring (and use) 
#an umbrella if it's raining when he leaves the house or office, but to leave them all behind if not.
#Louie owns three umbrellas.
# 
# On Sunday night, two are with him at home and one is at his office. Assuming it never starts raining
#during his walk to his home or office, what is the probability that he makes it through the work week without getting wet?

#load packages
library(tidyverse)

#input variables
rain_chance_am <- .5
rain_chance_pm <- .4

home_umbrellas <- 2
work_umbrellas <- 1

days_in_work_week <- 5
num_weeks_simulated <- 100000 #running 100,000 weeks took me about 10 seconds


#setup dataframe for problem
am_pm <- c("am","pm")
work_days <- c(1:days_in_work_week)
weeks <- c(1:num_weeks_simulated)

df <- crossing(weeks,work_days,am_pm)


#simulate rain for each morning/evening
df <- df %>%
    mutate(rand1=runif(nrow(df))) %>%
    mutate(rain = ifelse(am_pm=="am", ifelse(rand1<rain_chance_am, 1, 0),
                                      ifelse(rand1<rain_chance_pm, 1, 0)))


# #next step: get them umbrellas movin, boiiiii 

#let umbrella going from home to work = 1
#let umbrella going from work to home = -1
df <- df %>%
    mutate(umbrella_move = ifelse(rain==1, ifelse(am_pm=="am", 1, -1), 0)) %>%
    group_by(weeks) %>%
    mutate(umbrella_flow = cumsum(umbrella_move)) %>%
    mutate(home_umb = 2-umbrella_flow) %>%
    mutate(work_umb = 1+umbrella_flow)

#home_umb and work_umb columns represent umbrellas left at each location AFTER commute represented by a given row
#This means if either < 0 during a week then we got wet on a commutue

#we need to look at each week and determine if home_umb or work_umb are < 0 at any point
#we can easily achieve this through using our umbrella_flow column but that's harder for someone else to follow

results <- df %>%
    group_by(weeks) %>%
    transmute(result = min(min(home_umb), min(work_umb))) %>%
    unique() %>%
    transmute(got_wet = ifelse(result<0, 1, 0))

sum(results$got_wet)/nrow(results)

#looks like the answer is between 30% and 31% based on an average of simulations of 10,000 weeks
    