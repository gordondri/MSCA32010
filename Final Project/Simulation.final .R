# Simulate a game of monopoly
source('MSCA32010.R')

# STEP 1: Perform manual multiplications of the transition matrix by the probability 
# distribution vectors at states 1, 2 and 3

# Use the following as a demonstration of multiplying the transition matrix by the 
# probability distribution vector (three iterations are performed here and the complete 
# set of iterations is performed in the for loop below)

# initialize the probability distribution vector for state 0 representing the player 
# on the first position (i.e 'Go')
x0 <- as.matrix(c(1, rep(0, 40)))

# multiply the transition matrix by the probability distribution vector at state 0 
# to obtain the probabiliy distribution vector at state 1 (1 turn later)
x.1 <- (t(transition.matrix)) %*% x0; x.1
# multiply the transition matrix by the probability distribution vector at state 1 
# to obtain the probabiliy distribution vector at state 2 (2 turns later)
x.2 <- (t(transition.matrix)) %*% x1; x.2
# multiply the transition matrix by the probability distribution vector at state 2 
# to obtain the probabiliy distribution vector at state 3 (3 turns later)
x.3 <- (t(transition.matrix)) %*% x2; x.3

# STEP 2: Perform programmatic multiplications of the transition matrix by the probability 
# distribution vectors at states 1, 2, 3 and until the steady state (i.e state i)

# initialize the probability distribution vector at state 0
xk <- as.matrix(c(1, rep(0, 40)))
# initialize the threshold at which we will make a decision that xk+1 = xk
epsilon <- 1e-100

# set 1000 iterations but the loop may not run until exhaustion 
for (i in 1:1000) {
  # obtain the prob. dist. vector of state i by multiplying the transition matrix by 
  # the probability dist. vector of state i - 1
  xk.1 <- t(transition.matrix) %*% xk
  if (sum((abs(xk.1 - xk) < epsilon)) == 41) {
    break
  }
  xk <- xk.1
}

# STEP 3: Check the results 

# check the number of iterations to get to steady state
i
# Note: comment on the feasibility of this number 

# check xk+1
xk.1

# confirm the xk+1 is roughly equal to steady state probabilities obtained from the 
# markovchain package in the 'MSCA32010.R' file
xk.1 - stdy.states

# STEP 4: Use the general formula of Markov chain (i.e xk = M^k*x0) where k is 
# equal to i from the previous FOR loop to confirm our calculations 

# run the line below the first time to install the expm package 
# install.packages("expm")
library(expm)

# right-side of the equation 
x0 <- as.matrix(c(1, rep(0, 40)))
M.exp.k <- transition.matrix %^% i
M.exp.k
RHS <- t(M.exp.k) %*% x0

# left-side of the equation
LHS <- xk.1

# check if left-side of the equation roughly equals the right-side of the equation 
LHS - RHS
