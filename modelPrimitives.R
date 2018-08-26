###  Number of states
J <- 3

### Number of players
I <- 3

###  Wealth
W <- matrix(c(4,5,6))

### Beliefs
Mu <- as.matrix(t(rbind(
                  c(0.1,0.5,0.2), c(0.6, 0.3, 0.2), c(0.3, 0.2, 0.6)
                    )))

#if apply(Mu, 1, sum)) <> c(1,1,1) then stop

###  Certainty
Alpha <- as.matrix(c(0.5,0.95,0.5))

### Initial quantity
Q <- Mu


#### Find prices from quantities

for (t in 1:30)
{
xP <- solve(Q) %*% W
P <- xP/sum(xP)

## check
#max((Q %*% P) / W) - min((Q %*% P) / W)

### Find quantities from prices

yP <- as.matrix(t(rbind(
  c(1/P[1],0,0), c(0, 1/P[2], 0), c(0, 0, 1/P[3])
)))

## mu alpha q^{alpha -1} = 1 / p
yQ <- 0.5*(Mu%*%yP)

xQ <-(yQ)^{1/(1 - 0.5)}

# check
#  Mu[1,2]*0.5*xQ[1,2]^{0.5 - 1} - (P[2])

normXQ <- colSums(xQ)

Q <- xQ
Q[,1] <- xQ[,1]/normXQ[1]
Q[,2] <- xQ[,2]/normXQ[2]
}