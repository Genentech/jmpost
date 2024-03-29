% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimSurvival.R
\name{SimSurvivalExponential}
\alias{SimSurvivalExponential}
\title{Simulate Survival Data from a Exponential Proportional Hazard Model}
\usage{
SimSurvivalExponential(
  lambda,
  time_max = 2000,
  time_step = 1,
  lambda_censor = 1/3000,
  beta_cont = 0.2,
  beta_cat = c(A = 0, B = -0.4, C = 0.2)
)
}
\arguments{
\item{lambda}{(\code{number})\cr the rate parameter.}

\item{time_max}{(\code{number})\cr the maximum time to simulate to.}

\item{time_step}{(\code{number})\cr the time interval between evaluating the log-hazard function.}

\item{lambda_censor}{(\code{number})\cr the censoring rate.}

\item{beta_cont}{(\code{number})\cr the continuous covariate coefficient.}

\item{beta_cat}{(\code{numeric})\cr the categorical covariate coefficients.}
}
\description{
Simulate Survival Data from a Exponential Proportional Hazard Model
}
\section{Hazard Evaluation}{


Event times are simulated by sampling a cumulative hazard limit from a \eqn{U(0, 1)} distribution
for
each subject and then counting how much hazard they've been exposed to by evaluating the
log-hazard function at a set interval. The \code{time_max} argument sets the upper bound for the
number of time points to evaluate the log-hazard function at with subjects who have not had an
event being censored at \code{time_max}. The \code{time_step} argument sets the interval at which to
evaluate the log-hazard function. Setting smaller values for \code{time_step} will increase the
precision of the simulation at the cost of increased computation time. Likewise, setting large
values for \code{time_max} will minimize the number of censored subjects at the cost of
increased computation time.
}

\seealso{
Other SimSurvival: 
\code{\link{SimSurvival-class}},
\code{\link{SimSurvivalLogLogistic}()},
\code{\link{SimSurvivalWeibullPH}()}
}
\concept{SimSurvival}
