### 3 players and 3 states
findMarketClearing <- function(W, Mu, alpha = 0.5, iterations = 80)
{

## Normalise wealth distribution
totalW <- sum(W)
normW <- W/totalW

## Choose initial value for Q
Q <- Mu

for (t in 1:iterations)
{

### STEP 1.1:  Find prices from quantities
# Q %*% P = W

xP <- solve(Q) %*% normW
P <- xP/sum(xP)

### STEP 2:  Find quantities from prices
# mu alpha q^{alpha -1} = (lambda) p
# This section of code needs reviewing

yP <- as.matrix(t(rbind(
  c(1/P[1],0,0), c(0, 1/P[2], 0), c(0, 0, 1/P[3])
)))


yQ <- alpha*(Mu%*%yP)
LambdaQ <-(yQ)^{1/(1 - alpha)}

###  This section normalises Q so that columns sum to 1 - and assumes lambda constant across players
#  Either lambda is constant by coincidence (assumption above)
#  Either W is endogenous (and lambda = 1, since players have choice to not participate)
#  Or Lambda is not constant (and above does not make sense)

### What needs to be done is take "LambdaQ"
#  Divide each row by constant
#  Choose constant such that each column sums to 1
Q <- LambdaQ
Q[,1] <- LambdaQ[,1]/colSums(LambdaQ)[1]
Q[,2] <- LambdaQ[,2]/colSums(LambdaQ)[2]
Q[,3] <- LambdaQ[,3]/colSums(LambdaQ)[3]
}

# All money distributed whichever state occurs
finalQ <- Q*totalW

# Prices sum to 1
finalP <- P 

# Put together everything for final output  
finalOutput <- cbind(Mu, finalQ, c(0,0,0), W, finalP)

return(finalOutput)
}

###  Wealth distribution
userInputW <- matrix(c(400/15, 500/15, 600/15))

### Beliefs
userInputMu <- as.matrix(t(rbind(
  c(0.1,0.5,0.2), c(0.6, 0.3, 0.2), c(0.3, 0.2, 0.6)
)))

finalOutput <- findMarketClearing(W = userInputW, Mu = userInputMu, alpha = 0.5)
