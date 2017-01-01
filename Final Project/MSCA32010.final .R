# set the working directory according to the location that the source files are in 
setwd("/Users/gordondri/Desktop/MSCA32010")

# source the necessary files 
source('CourseProject_Chance_Comm.R')
source('CourseProject_Jail.R')
source('RentsVector.R')

############################## PART A: Set-Up ##############################
# Fill in initial probabilities of each row of the transition matrix 

# STEP 1: Initialize probability vector (probabilities of rolling various combinations of 
# two die (i.e between 2 and 12) ) and the transition matrix 
prob.vector <- c(1/36, 1/18, 1/12, 1/9, 5/36, 1/6, 5/36, 1/9, 1/12, 1/18, 1/36)
transition.matrix <- matrix(nrow = 40, ncol = 40, 0)

# STEP 2: Loop through each row and put the prob.vector at its appropriate location 
# Note: The square 'Go' is considered square # 1. 
for (i in 1:40) {
  if (i < 29){
    transition.matrix[i, (i+2):(i+12)] <- prob.vector
    # If the row is >= 29, we have to begin inputting the probabilities at the beginning
    # of the row 
  } else if (i >= 29 && i < 39) {
    transition.matrix[i, (i+2):((i+2)+(40-(i+2)))] <- prob.vector[1:(41-(i+2))]
    transition.matrix[i, (1:(11-(41-(i+2))))] <- prob.vector[(42-(i+2)):11]
    # If the row is == 39, we put the probability vector at the beginning of the row
  } else if (i == 39) {
    transition.matrix[i, 1:11] <- prob.vector
    # If the row is == 40, we put the probability vector at the beginning of the row
    # but offset by one space 
  } else {
    transition.matrix[i, 2:12] <- prob.vector
  }
}

# Check the transition matrix 
# View(transition.matrix)

# Check that all rows add to 1
(apply(transition.matrix, 1, sum))

############################## PART B: Handle Chance Cards ##############################
# Adjust probabilities in the transition matrix to account for the chance cards

# STEP 1: Set the chance.rows vector as locations 8, 23 and 37 which correspond to the 
# squares on the board that have chance cards
chance.rows <- c(8, 23, 37)

# STEP 2: Adjust the probabilities in the transition matrix for the chance card rows 
# when a player selects a chance card that requires them to stay in their current position 

# Since there are 9 scenarios outlined in the CourseProject_Chance_Comm.R file in which a 
# player would move positions and there are 16 chance cards in total, there are 7 cards
# which require the player to stay 
prob.chance.stay <- 7/16

# multiply the existing probabilities of the chance rows with the probability of choosing
# a card that requires the player to stay. This will yield the probabilities of a player
# following the regular distribution (i.e those outlined in the prob. vector) of moving 
# to another square 
for (row in chance.rows) {
  transition.matrix[row, ] <- (prob.chance.stay * transition.matrix[row, ])
}

# check the transition matrix 
# View(transition.matrix)

# STEP 3: Adjust the probabilities in the transition matrix for the chance card rows 
# when a player selects a chance card that requires them to move their current position 

# add the existing probabilities of the chance rows to the probabilities of moving to 
# other positions defined by the chance card chosen 
transition.matrix[8, ] <- transition.matrix[8, ] + Chance.Scenario.Total.8 
transition.matrix[23, ] <- transition.matrix[23, ] + Chance.Scenario.Total.23 
transition.matrix[37, ] <- transition.matrix[37, ] + Chance.Scenario.Total.37 

# check the transition matrix 
# View(transition.matrix)

######################### PART C: Handle Community Chest Cards #########################
# Adjust probabilities in the transition matrix to account for the community chest cards 

# STEP 1: Set the community.chest.rows vector as locations 3, 18 and 34 which correspond
# to the squares on the board that have chance cards
community.chest.rows <- c(3, 18, 34)

# STEP 2: Adjust the probabilities in the transition matrix for the community chest rows 
# when a player selects a community chest card that requires them to stay in their current
# position 

# Since there are 2 scenarios outlined in the CourseProject_Chance_Comm.R file in which a 
# player would move positions and there are 16 community chest cards in total, there are 
# 14 cards which require the player to stay 
prob.community.chest.stay <- 14/16

# multiply the existing probabilities of the community chest rows with the probability of 
# choosing a card that requires the player to stay. This will yield the probabilities of 
# a player following the regular distribution (i.e those outlined in the prob. vector) of 
# moving to another square 
for (row in community.chest.rows) {
  transition.matrix[row, ] <- prob.community.chest.stay * transition.matrix[row, ]
}

# check the transition matrix 
# View(transition.matrix)

# STEP 3: Adjust the probabilities in the transition matrix for the community chest rows 
# when a player selects a community chest card that requires them to move their current 
# position

# add the existing probabilities of the community chest rows to the probabilities of 
# moving to other positions defined by the community chest card chosen 
transition.matrix[3, ] <- transition.matrix[3, ] + Comm.Scenario
transition.matrix[18, ] <- transition.matrix[18, ] + Comm.Scenario
transition.matrix[34, ] <- transition.matrix[34, ] + Comm.Scenario

# check the transition matrix 
# View(transition.matrix)

# Check all rows sum to 1
(apply(transition.matrix, 1, sum))

############################## PART D: Handle Jail States ##############################
# Adjust probabilities in the transition matrix to account for the jail states 

# STEP 1: Expand transition matrix to include an additional jail state corresponding to 
# being in jail after the first turn 

# Create additional columns for jail states
new.values <- c(rep(0, 40))
transition.matrix <- cbind(transition.matrix, new.values)
# Create additional rows for jail states
transition.matrix <- rbind(transition.matrix, new.values)

# Ensure the number of columns and rows equal 41 
ncol(transition.matrix) == 41
nrow(transition.matrix) == 41

# STEP 2: Adjust the probabilities in the transition matrix for the jail state rows 

# Input the calculated jail state probabilities in the 31st row (jail state 1) and the 
# 41st row (jail state 2)
transition.matrix[31, ] <- Jail.state.1
transition.matrix[41, ] <- Jail.state.2

# Check all rows sum to 1
(apply(transition.matrix, 1, sum))

############################## PART E: Change Row/Col Names ##############################
# Change the row and column names to the actual titles that appear in the game Monopoly 

# initialize a vector that contains all the actual titles
titles <- c("Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue", 
            "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance", 
            "Vermont Avenue", "Connecticut Avenue", "In Jail Visiting", 
            "St Charles Place", "Electric Company", "States Avenue", "Virginia Avenue", 
            "Pennsylvania Railroad", "St James Place", "Community Chest", 
            "Tennessee Avenue", "New York Avenue", "Free Parking", "Kentucky Avenue", 
            "Chance", "Indiana Avenue", "Illinois Avenue", "B & O Railroad", 
            "Atlantic Avenue", "Ventnor Avenue", "Water Works", "Marvin Garden", 
            "Go to Jail", "Pacific Avenue", "North Carolina Avenue", "Community Chest", 
            "Pennsylvania Avenue", "Short Line", "Chance", "Park Place", "Luxury Tax", 
            "Boardwalk", "Jail")
# change the row and column names to reflect these titles 
rownames(transition.matrix) <- titles
colnames(transition.matrix) <- titles

# check final transition matrix
View(transition.matrix)

############## PART F: Solve for Steady-State using Null Spaces #####################
# Get the steady-state distribution by finding the basis of the null space of
# (P - I)^T where I is the identity matrix. 

# STEP 1: Install the required packages 
# install.packages("pracma")
# install.packages("quadprog")
# install.packages("expm")
library(pracma)
library(expm)

# STEP 2: Set up the identity matrix and matrix A
n <- ncol(transition.matrix)
A = t(transition.matrix - diag(n))

# STEP 3: Find the basis of the nullspace
basis = nullspace(A)

# STEP 4: Normalize the basis of nullspace --  equivalent to our SS vector
NullspaceSS = basis / sum(basis)
NullspaceSS

############### PART H: Solve for Steady-State using Eigenvectors ############### 
# Because mu is a solution to muP = mu, mu is a left-eigenvector of the square
# matrix P corresponding to the eigenvalue landa = 1

# STEP 1: Use function to find the eigen values and eigen vectors
e = eigen(t(transition.matrix))
head(e)

# STEP 2: print the first eigen value
e$values[1]

# save and print the first eigenvalue
firstEigenvector = Re(e$vectors[ ,1])
firstEigenvector

# STEP 3: Normalize first eigenvector
NfirstEigenvector = firstEigenvector / sum(firstEigenvector)

# print eigenvector -- equivalent to our SS vector
NfirstEigenvector

############### PART H: Solve for Steady-State using MarkovChain Package ############### 
# Use the markovchain package to solve steady state given a transition matrix 

# STEP 1: Get the required markovchain package and load it into the directory 

# run the line below the first time to install the markovchain package 
# install.packages("markovchain")
# library(markovchain)

# STEP 2: Convert the transition matrix into a new object from the class 'markovchain'
# such that we can use the steadystate() function within the package markovchain

# convert the transition.matrix from class dataframe to class markovchain 
mcw <- new("markovchain", states = titles, transitionMatrix = transition.matrix)

# STEP 3: Obtain the steady state vector 

# call the steadyState function to find the steady state vector and transpose it to 
# obtain a column vector 
stdy.states <- t(data.frame(steadyStates(mcw)))

# view the steady state vector 
print(stdy.states)

# plot the steady state vector (note: this may not be very useful given the size of 
# the matrix) 
plot(mcw)

######### PART I: Compare steady state vectors from three methods above ######

cbind(SS.Basis = NullspaceSS, SS.Eigen = NfirstEigenvector, SS.Package = stdy.states)

###### PART J: Find the expected income given the steady states and property rents ######

# Multiply the probabilities for each square from the steady state vector with the
# expected property rents defined in 'RentsVector.R'
rent.expected <- stdy.states * rents

