% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcome_funs.R
\name{initialize_z}
\alias{initialize_z}
\title{Set initial known-state values prior to JAGS MCMC}
\usage{
initialize_z(ch)
}
\arguments{
\item{ch}{Capture history, i.e., matrix of nest visits}
}
\value{
A matrix of initial states for each nesting attempt
}
\description{
This function is used in \code{estimate_outcomes}
Used to initialize the latent variable \code{z} in the JAGS MCMC.
Sets \code{z} = 1 for all time steps in between the first sighting
and the last sighting.
}
