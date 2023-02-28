#First need to import both datasets "agent_data" and "product_data"
agent_data <- read.csv("agent_data.csv", header=TRUE)
product_data <- read.csv("product_data.csv", header=TRUE)

#Define the number of markets, agents, and products
T <- length(unique(agent_data$market_ids)) #94 different markets
I <- length(which(agent_data$market_ids=="C01Q1")) #20 agents per market
J <- length(which(product_data$market_ids=="C01Q1")) #24 products per market

#Create matrix of 1's for latter use
ones_JxI <- array(1, dim = c(J,I,T)) #matrix of ones with dimension JxIxT
ones_IxJ <- array(1, dim = c(I,J,T)) #matrix of ones with dimension IxJxT

#creating matrix Z of instrumental variables
Z_matrix <- array(0, dim = c(J*T,21)) #the 20 IVs + sugar
for (i in 1:21) {
  Z_matrix[,i]<- product_data[,5+i]
}

Z_matrix<- as.matrix(cbind(rep(1, J*T), Z_matrix))  # add vector of 1's

#Create matrix of products' characteristics
sugar <- array(0, dim = c(J,I,T)) #matrix of product characteristic: sugar
price <- array(0, dim = c(J,I,T)) #matrix of product characteristic: price
data_shares <- array(0, dim = c(J,I,T)) #product shares from data
for (t in 1:T){
  for (j in 1:J){
    sugar[j,,t] <- product_data$sugar[j+(t-1)*J]
    price[j,,t] <- product_data$prices[j+(t-1)*J]
    data_shares[j,,t] <-product_data$shares[j+(t-1)*J]
  }
}

#Create matrix of agents' characteristics
income <- array(0, dim = c(J,I,T)) #matrix of agent's income
node <- array(0, dim = c(J,I,T)) #matrix of agent's random component
weights <- array(0, dim = c(J,I,T)) #matrix of agent's weights
for (t in 1:T){
  for (i in 1:I){
    income[,i,t] <- agent_data$income[i+(t-1)*I]
    node[,i,t] <- agent_data$nodes0[i+(t-1)*I]
    weights[,i,t] <- agent_data$weights[i+(t-1)*I]
  }
}


#Code timer
code_time <- proc.time()

#Define the GMM function that will be minimize by nonlinear optimization
GMM_fun <- function(x) {

#guesses of alphas, betas, deltas
betas <- matrix(0, 3, 1)
alphas <- matrix(0, 3, 1)
betas[1:3] <- x[1:3] #values to be solved for
alphas[1:3] <- x[4:6] #values to be solved for

deltas <- array(0, dim = c(J,I,T)) #mean utilities for each product in each market
deltas_prime <- array(1, dim = c(J,I,T)) #matrix for the updated deltas

tolerance <- 10^(-12)

#While loop for the first step 1 and 2 of the algorithm
while (max(abs(deltas_prime-deltas))> tolerance)  {
  
deltas <- deltas_prime

numerator <- exp( deltas + betas[2,1]*income + betas[3,1]*income*sugar + alphas[2,1]*income*price + alphas[3,1]*node*price )

denominator1 <- colSums(numerator) + ones_JxI[1,,]
denominator <- aperm(array(rep(denominator1,J),c(dim(denominator1),J)), c(3,1,2))

shares1 <- colSums(aperm(numerator*weights/denominator, c(2,1,3)))
shares <- aperm(array(rep(shares1,I),c(dim(shares1),I)), c(1,3,2))

#Berry inversion to find the deltas=shares
deltas_prime <- deltas + log(data_shares) - log(shares)

}

#Step 3 of algorithm
#computing unobserved demand shock
xi1 <- deltas[,1,] - betas[1,1]*ones_JxI[,1,] - alphas[1,1]*price[,1,]
xi <- array(0, dim = c(J*T)) #vector for the unobserved demand shocks xi
for (t in 1:T){
  for (j in 1:J){
    xi[j+(t-1)*J] <- xi1[j,t]
  }
}

#Creating GMM objective function
t(xi) %*% Z_matrix %*% solve(t(Z_matrix) %*% Z_matrix) %*% t(Z_matrix) %*% xi
}

#Nonlinear optimization
blp_solution <- optim(c(-3.8,	4.4,	0.1,	-2.2,	-33.2,	1.1), GMM_fun, control = list(maxit = 10000))
theta <- blp_solution[[1]] #estimated coefficients

proc.time() - code_time #end of timer

###########
#Elasticities
#Plug in the estimated coefficients
betas <- matrix(0, 3, 1)
alphas <- matrix(0, 3, 1)
betas[1:3] <- theta[1:3]
alphas[1:3] <- theta[4:6]

#Run the while loop of Steps 1 and 2 for the estimated coefficients

deltas <- array(0, dim = c(J,I,T)) #mean utilities for each product in each market
deltas_prime <- array(1, dim = c(J,I,T)) #matrix for the updated deltas

tolerance <- 10^(-12)

while (max(abs(deltas_prime-deltas))> tolerance)  {
  
  deltas <- deltas_prime
  
  numerator <- exp( deltas + betas[2,1]*income + betas[3,1]*income*sugar + alphas[2,1]*income*price + alphas[3,1]*node*price )
  
  denominator1 <- colSums(numerator) + ones_JxI[1,,]
  denominator <- aperm(array(rep(denominator1,J),c(dim(denominator1),J)), c(3,1,2))
  
  shares1 <- colSums(aperm(numerator*weights/denominator, c(2,1,3)))
  shares <- aperm(array(rep(shares1,I),c(dim(shares1),I)), c(1,3,2))
  
  #Berry inversion to find the deltas=shares
  deltas_prime <- deltas + log(data_shares) - log(shares)
  
}

#Probability that i purchases j in market t
individual_shares <- numerator*weights/denominator

#Calculating the integral's elements
integral <- array(0, dim = c(I,J)) 
for (j in 1:J){
  for (i in 1:I){
    integral[i,j] <- individual_shares[j,i,1] * (1-individual_shares[j,i,1]) * (theta[4] + theta[5]*income[1,i,1] + theta[6]*node[1,i,1])
  }
}

#Summing up the integral's elements by product j
integral_product <- colSums(integral)

#Calculating the own-prices elasticities
elasticities <- array(0, dim = c(J))
for (j in 1:J){
  elasticities[j] <- price[j,1,1] / shares[j,1,1] * integral_product[j]
}

#Plotting elasticities and prices
plot(price[,1,1], elasticities, main="Price elasticity in market C01Q1 BLP",
     xlab="Price ", ylab="Elasticity ", pch=19)


