#Adapted from CJS analysis here:
shell.exec("https://oliviergimenez.github.io/post/sim_with_jags/")

library(rjags)

#Load data
dat <- readRDS("sim_dat_binom.RData")

#Fix matrix (# of successful GPS fixes that day)
fix.mat <- matrix(12, nrow=nrow(dat), ncol=ncol(dat))

#Starting values for survival status
# Initial values
known.state.cjs <- function(ch){
  state <- ch
  for (i in 1:dim(ch)[1]){
    n1 <- 1
    n2 <- max(which(ch[i,]>0))
    state[i,n1:n2] <- 1
    state[i,n1] <- NA
  }
  state[state==0] <- NA
  return(state)
}

s1 <- known.state.cjs(dat)

##Define jags model
jags <- jags.model(file="sim_nest_binom.bug",
                   data=list("N"=nrow(dat),
                             "D"=ncol(dat),
                             "H"=fix.mat,
                             "y"=dat),
                   inits=list("mean.phi"=runif(1,0,1),
                              "mean.p"=runif(1,0,1),
                              "z"=s1),
                   n.chain=2, n.adapt=1000)
#Run the burn-in
update(jags, 1000)
#Generate posterior samples
post <- coda.samples(jags, c("mean.phi", "mean.p"), 
                     n.iter=10000, thin=5)

#Summary plot
plot(post)
#Trace plots
traceplot(post)
#Autocorrelation
autocorr.plot(post) #should thin by 5
#Posterior PDF
densplot(post)


#View estimates

