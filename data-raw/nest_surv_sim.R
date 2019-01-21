
#True nest survival----
#Number of days in season
days <- 120
#Number of nests to simulate
nests <- 100

#Matrix of nest survival
n.mat <- matrix(NA, nrow=nests, ncol=days)
#First day every nest gets a 1
n.mat[,1] <- 1
#Simulate daily survival
daily.surv <- 0.99
set.seed(1)
for (i in 1:nests){
  for (j in 2:days){
    n.mat[i,j] <- n.mat[i, (j-1)] * rbinom(n=1, size=1, prob=daily.surv)
  }
}
#How many survived to the end?
sum(n.mat[,120])

#Pr(Detection|Suvival)----
#Probability of being detected at the nest
p.nest <- 0.4
#Probability of GPS fix
p.gps <- 0.9
#Number of chances each day to be detected
n.fix <- 12

#Matrix of observation
o.mat <- matrix(NA, nrow=nests, ncol=days*n.fix)
for (i in 1:nests){
  for (j in 1:days){
    o.mat[i, (j*12-11):(j*12)] <- n.mat[i,j] * rbinom(n=n.fix, size=1, prob=p.nest) * rbinom(n=n.fix, size=1, prob=p.gps) 
    
  }
}

#Save the data----
saveRDS(o.mat, "sim_dat.RData")

#Simply to binomial outcome for each day----
bin.mat <- matrix(NA, nrow=nests, ncol=days)
for (i in 1:nests){
  for (j in 1:days){
    bin.mat[i,j] <- n.mat[i, j] * rbinom(n=1, size=12, prob=p.nest)
  }
}

#Save the data----
saveRDS(bin.mat, "sim_dat_binom.RData")
