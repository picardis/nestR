% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcome_funs.R
\name{summarize_outcomes}
\alias{summarize_outcomes}
\title{Summary of outcomes from MCMC run}
\usage{
summarize_outcomes(mcmc_obj, ci = 0.95)
}
\arguments{
\item{mcmc_obj}{List of \code{mcarrays} output by \code{estimate_outcomes}.}

\item{ci}{Numeric. Credible interval level.}
}
\value{
A list with (a) the population-level survival and (b) detection
parameters and (c) the individual nest fates
}
\description{
\code{summarize_outcomes} returns summary statistics of estimated nesting
attempt outcomes.
}
\details{
The function takes as input a list of \code{mcarrays} output by
\code{estimate_outcomes} and returns a list including the following:

\itemize{

\item mean, lower and upper credible interval values (at the level
specified by the user) for both survival and detection probability,
where applicable (depending on which model formula was chosen);
\item for each attempt, mean, lower and upper credible interval values
of the probability of success and of the failure date. If the estimated
probability of success is 1, the failure date corresponds to the duration
of a complete nesting attempt.

}
}
