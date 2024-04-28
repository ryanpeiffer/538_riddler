# the fiddler 4/26/2024

'''
Suppose you have two players: A and B. Player A is the first to spin a giant wheel,
which spits out a real number chosen randomly and uniformly between 0 and 1. 
All spins are independent of each other. After spinning, A can either stick with 
the number they just got or spin the wheel one more time. If they spin again, 
their assigned number is the sum of the two spins, as long as that sum is less than or equal to 1.
If the sum exceeds 1, A is immediately declared the loser.

After A is done spinning (whether once or twice), B steps up to the wheel. 
Like A, they can choose to spin once or twice. If they spin twice and the sum exceeds 1, 
they are similarly declared the loser.

After both players are done, whoever has the greater value (that does not exceed 1) is declared the winner.
Assuming both players play the game optimally, what are Player A's chances of winning?
'''

#setup
import random

random.seed(9999)
n_trials = 10000000

#naive strategy
#player A re-spins if below or equal to 0.5, stays put if above 0.5
#player B re-spins only if below player A's score

#note technically python's random function is 0 <= X < 1, and the problem is inclusive of 1,
#but given we are working with all real numbers, it's an infinite set, so excluding 1 should be immaterial on result.

#main loop
rolls = [random.random() for i in range(4*n_trials)]
winners = []
for i in range(n_trials):
    round_rolls = rolls[i:i+4]
    
    a_score = round_rolls[0]
    if round_rolls[0] < 0.5:  
        a_score += round_rolls[1]
        if a_score > 1:
            a_score = 0
    b_score = round_rolls[2]
    if round_rolls[2] < a_score: 
        b_score += round_rolls[3]
        if b_score > 1:
            b_score = 0

    if a_score > b_score:
        winner = 'a'
    else:
        winner = 'b'
    
    result = [a_score, b_score]
    winners.append(winner)

#analyze results
a_win_pct = len(list(filter(lambda x: x=='a', winners))) / len(winners)
print("naive strat win pct for A: " + str(a_win_pct))

#using the naive strat for A, win percent is around 45%
#lets iterate over strategies to see if A can optimize

def run_the_wheel(threshold, n_trials):
    rolls = [random.random() for i in range(4*n_trials)]
    winners = []
    for i in range(n_trials):
        round_rolls = rolls[i:i+4]
        
        a_score = round_rolls[0]
        if round_rolls[0] < threshold:  
            a_score += round_rolls[1]
            if a_score > 1:
                a_score = 0
        b_score = round_rolls[2]
        if round_rolls[2] < a_score: 
            b_score += round_rolls[3]
            if b_score > 1:
                b_score = 0

        if a_score > b_score:
            winner = 'a'
        else:
            winner = 'b'
        
        result = [a_score, b_score]
        winners.append(winner)

    a_win_pct = len(list(filter(lambda x: x=='a', winners))) / len(winners)
    return a_win_pct
'''
thresholds = [i/100 for i in range(100)]
results = []

for threshold in thresholds:
    a_win_pct_tmp = run_the_wheel(threshold, n_trials)
    results.append([threshold, a_win_pct_tmp])
    print([threshold, a_win_pct_tmp])
best_strat = max(results, key = lambda x: x[1])
print(f"A can achieve a win percent of {round(best_strat[1],4)*100}% by rerolling when below {best_strat[0]}")

#using 1 million trials, I see that A maximizes by rerolling when below .55 with a win rate of 45.4%.
#I suspected the optimal strategy would be a little above .5, gotta take some risk to win since B has an information advantage
#now that I've narrowed down the range, let's get even more exact!
'''
thresholds_granular = [i/1000 for i in range(500, 600)]
results_granular = []

for threshold in thresholds_granular:
    a_win_pct_tmp = run_the_wheel(threshold, n_trials)
    results_granular.append([threshold, a_win_pct_tmp])
    print([threshold, a_win_pct_tmp])
best_strat_optimized = max(results_granular, key = lambda x: x[1])
print(f"A can achieve a win percent of {round(best_strat_optimized[1],6)*100}% by rerolling when below {best_strat_optimized[0]}")

#getting varying results here, even when using 10M sims, but rarely seeing anything jump above 45.4%
#seems like the right reroll threshold for A is somewhere between .53 and .57