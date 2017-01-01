# Handle jail states 

# Since a player can only get out of jail by rolling doubles in their next two turns, 
# define the possible squares that can be reached if doubles are rolled (i.e 
# 1-1 (2), 2-2 (4), 3-3 (6), 4-4 (8), 5-5 (10), 6-6 (12) ). 
Jail.states <- c(13, 15, 17, 19, 21, 23)

# Initialize jail state vectors

# The 31st row corresponds to jail state 1 (first turn being in jail)
Jail.state.1 <- c(rep(0, 41))
# The 41st row corresponds to jail state 2 (second turn being in jail) 
Jail.state.2 <- c(rep(0, 41))

# distribute the 1/6 (probability of rolling doubles) across 6 different squares (i.e
# the spots defined by the jail.states) to get 1/36 for each specific double roll
for (state in Jail.states){
  Jail.state.1[state] <- 1/36
  Jail.state.2[state] <- 1/36
}

# probability of going from the first jail state (31st row) to the second jail state
# (41st row) - equal to the probability of not rolling doubles (5/6)
Jail.state.1[41] <- 5/6
# probability of going from the second jail state (41st row) to the third jail state
# (11th row) - equal to the probability of not rolling doubles (5/6)
Jail.state.2[11] <- 5/6



