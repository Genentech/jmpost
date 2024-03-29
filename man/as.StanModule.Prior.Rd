% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prior.R
\name{as.StanModule.Prior}
\alias{as.StanModule.Prior}
\title{\code{Prior} -> \code{StanModule}}
\usage{
\method{as.StanModule}{Prior}(object, name, ...)
}
\arguments{
\item{object}{(\code{\link{Prior}})\cr a prior Distribution}

\item{name}{(\code{character})\cr the name of the parameter the prior distribution is for}

\item{...}{Not Used.}
}
\description{
Converts a \code{\link{Prior}} object to a \code{\link{StanModule}} object
}
\seealso{
Other Prior-internal: 
\code{\link{Prior-Getter-Methods}},
\code{\link{Prior-class}},
\code{\link{as.character.Prior}()},
\code{\link{as_stan_list.Prior}()}

Other as.StanModule: 
\code{\link{as.StanModule}()},
\code{\link{as.StanModule.JointModel}()},
\code{\link{as.StanModule.Link}()},
\code{\link{as.StanModule.LinkComponent}()},
\code{\link{as.StanModule.Parameter}()},
\code{\link{as.StanModule.ParameterList}()}
}
\concept{Prior-internal}
\concept{as.StanModule}
