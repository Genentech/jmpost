% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prior.R
\name{as_stan_list.Prior}
\alias{as_stan_list.Prior}
\title{\code{Prior} -> \code{list}}
\usage{
\method{as_stan_list}{Prior}(object, name, ...)
}
\arguments{
\item{object}{(\code{\link{Prior}})\cr a prior Distribution}

\item{name}{(\code{character})\cr the name of the parameter the prior distribution is for}

\item{...}{Not Used.}
}
\description{
Converts a Prior object to a list of parameter data values
for a Stan model.
}
\seealso{
Other as_stan_list: 
\code{\link{as_stan_list}()},
\code{\link{as_stan_list.DataSubject}()},
\code{\link{as_stan_list.Parameter}()},
\code{\link{as_stan_list.ParameterList}()}

Other Prior-internal: 
\code{\link{Prior-Getter-Methods}},
\code{\link{Prior-class}},
\code{\link{as.StanModule.Prior}()},
\code{\link{as.character.Prior}()}
}
\concept{Prior-internal}
\concept{as_stan_list}
