#' Sets initial known-state values prior to JAGS MCMC
#'
#' This function is used in \code{estimate_outcomes}
#' Used to intialize the latent variable \code{z} in the JAGS MCMC.
#' Sets \code{z} = 1 for all time steps in between the first sighting
#' and the last sighting.
#'
#' @export
initialize_z <- function(ch){
  # Initialize state using the "capture history" (in CMR parlance)
  state <- ch

  # Loop through each nest
  for (i in 1:nrow(ch)){
    # The earliest "sighting" will always be the first day of the attempt
    n1 <- 1

    # The last sighting is the last time the animal was observed at the nest
    n2 <- max(which(ch[i,]>0))

    # Set all states between first and last to 1
    state[i, n1:n2] <- 1

    # Reset first to NA (because always see them on first day by definition)
    state[i, n1] <- NA
  }

  # Now set any states remaining as 0 to NA so that JAGS will estimate them
  state[state==0] <- NA

  # Return
  return(state)
}

#' Estimates nesting outcomes
#'
#' \code{estimate_outcomes} does blah blah blah...
#' @details Data can be passed from \code{\link{format_visits}}... blah blah blah
#'
#' @param fixes A matrix of the number of GPS fixes on each day of the nesting attempt
#'   as returned by \code{\link{format_visits}}.
#' @param visits A matrix of the number of nest visits on each day of the nesting attempt
#'   as returned by \code{\link{format_visits}}.
#'
#' @export
estimate_outcomes <- function(fixes, visits,
                              mcmc_params = list(burn_in = 1000, n_chain = 2, thin = 5,
                                                 n_adapt = 1000, n_iter = 10000)
){
  # Path to the JAGS file
  jags_file <- file.path(system.file(package = "nestR"), "jags", "nest_outcome.txt")

  # Starting values for survival status
  s1 <- initialize_z(ch = visits)

  # Define JAGS model
  jags <- jags.model(file = jags_file,
                     data = list("nests" = nrow(visits),
                               "days" = ncol(visits),
                               "gps_fixes" = fixes,
                               "y" = visits),
                     inits = list("mean.phi" = runif(1,0,1),
                                "mean.p" = runif(1,0,1),
                                "z" = s1),
                     n.chain = mcmc_params$n_chain,
                     n.adapt = mcmc_params$n_adapt)

  #Run the burn-in
  update(object = jags, n.iter = mcmc_params$burn_in)

  #Generate posterior samples
  post <- coda.samples(model = jags,
                       variable.names = c("mean.phi", "mean.p"),
                       n.iter = mcmc_params$n_iter,
                       thin = mcmc_params$thin)

  return(post)

}
