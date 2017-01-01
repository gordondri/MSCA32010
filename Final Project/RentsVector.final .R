#Create rent vector

# Estimate the "payoff" for the community chest squares. The expected payoff of 
# all the cards are summed and divided by the number of cards to obtain the expected 
# payoff per community chest card 
total <- 200 - 50 + 50 + 150 + 100 + 20 + 100 - 100 - 150 + 25 - 345 + 10 + 100
cards <- 13
(CC <- total / cards)

# Estimate the "payoff" for the chance squares. The expected payoff of 
# all the cards are summed and divided by the number of cards to obtain the expected 
# payoff per chance card 
total.chance <- 50 - 300 - 15 - 150 + 150 + 100
cards.chance <- 6
(Chance <- total.chance / cards.chance) 

# The rents for each square is listed in its respective position assuming a hotel is 
# on each square 
rents <- c(0,250,CC,450,-200,200,550,Chance,550,600,0,750,70,750,900,200,950,CC,
           950,1000,150,1050,Chance,1050,1100,200,1150,1150,70,1200,0,1275,1275,CC,
           1400,200,Chance,1500,-100,2000,0)

