# Coding Chance Occurence

#Starting Vector of 0's
Chance.Scenario <- c(rep(0,40))
Chance.Scenario

#Insert Easy Vectors
Chance.Scenario[1] <- 1/16 #Go to Go
Chance.Scenario[6] <- 1/16 #Go to Reading Railroad
Chance.Scenario[11] <- 1/16 #Go to Jail
Chance.Scenario[12] <- 1/16 #Go to St Charles
Chance.Scenario[25] <- 1/16 #Go to IL Ave
Chance.Scenario[40] <- 1/16 #Go to Boardwalk
Chance.Scenario

#Create Function for Hard Scenarios
Chance.Fnc <- function(n) {

#Go Back 3 Spaces
Chance.Scenario.Back3 <- c(rep(0,40))
if (n == 8) { #scenario of getting to chance @ 8
  Chance.Scenario.Back3[5] <- 1/16
} else if (n == 23) { #scenario of getting to chance @ 23
  Chance.Scenario.Back3[20] <- 1/16
} else if (n == 37) { #scenario of getting to chance @ 37
  Chance.Scenario.Back3[34] <- 1/16 #which is community chest...
}

#Go to Nearest RR
Chance.Scenario.RR <- c(rep(0,40))
if (n == 8) { #scenario of getting to chance @ 8
  Chance.Scenario.RR[16] <- 1/16
} else if (n == 23) { #scenario of getting to chance @ 23
  Chance.Scenario.RR[26] <- 1/16
} else if (n == 37) { #scenario of getting to chance @ 37
  Chance.Scenario.RR[6] <- 1/16 #which is community chest...
}

#Go to Nearest Utility
Chance.Scenario.Utl <- c(rep(0,40))
if (n == 8) { #scenario of getting to chance @ 8
  Chance.Scenario.Utl[13] <- 1/16
} else if (n == 23) { #scenario of getting to chance @ 23
  Chance.Scenario.Utl[29] <- 1/16
} else if (n == 37) { #scenario of getting to chance @ 37
  Chance.Scenario.Utl[13] <- 1/16 #which is community chest...
}
  Chance.fnc.vector <- Chance.Scenario.Back3 + Chance.Scenario.RR + Chance.Scenario.Utl
  return(Chance.fnc.vector)
}

Chance.Scenario.Total.8 <- Chance.Scenario + Chance.Fnc(8)
Chance.Scenario.Total.23 <- Chance.Scenario + Chance.Fnc(23)
Chance.Scenario.Total.37 <- Chance.Scenario + Chance.Fnc(37)

#------------------------------------------------------------

#Coding Community Chest

#Starting Vector of 0's
Comm.Scenario <- c(rep(0,40))
Comm.Scenario

#Insert Easy Vectors
Comm.Scenario[1] <- 1/16 #Go to Go
Comm.Scenario[11] <- 1/16 #Go to Reading Railroad
Comm.Scenario

Comm.Scenario
