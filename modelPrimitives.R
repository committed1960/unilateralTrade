###  Number of goods
J = 5

### Number of players
I = 3

###  Wealth
W <- matrix(c(4,5,6))

### Beliefs
Mu <- as.matrix(t(rbind(
                  c(0.1,0.05,0.15), c(0.2, 0.2, 0.2), c(0.3, 0.2, 0.35), c(0.05, 0.1, 0.05), c(0.35, 0.45, 0.25)
                    )))

###  Prices
P <- matrix(1/J,J,1)



### Quantities
Q <- matrix(1/I, I, J)


### p^T Q^^ = 1
### Q p = w