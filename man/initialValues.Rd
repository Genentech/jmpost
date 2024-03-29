% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/StanModel.R, R/LinkComponent.R,
%   R/Link.R, R/JointModel.R
\name{initialValues}
\alias{initialValues}
\alias{initialValues.StanModel}
\alias{initialValues.LinkComponent}
\alias{initialValues.Link}
\alias{initialValues.JointModel}
\title{\code{initialValues}}
\usage{
initialValues(object, ...)

\method{initialValues}{StanModel}(object, n_chains, ...)

\method{initialValues}{LinkComponent}(object, n_chains, ...)

\method{initialValues}{Link}(object, ...)

\method{initialValues}{JointModel}(object, n_chains, ...)
}
\arguments{
\item{object}{where to get the initial values from.}

\item{...}{Not currently used.}

\item{n_chains}{the number of initial values to generate. See details.}
}
\description{
Obtain the \code{list} of initial values to be passed to the Stan sampler.
}
\details{
There are multiple ways of specifying initial values to Stan, see the \code{init} argument
in \link[cmdstanr:model-method-sample]{cmdstanr::model-method-sample} for full details. Within this package we supply
initial values via a list of lists where each inner list contains the initial values
for a single chain. As such the \code{n_chains} argument specifies the number of inner lists
to generate.

See the Vignette for further details of how to specify initial values.
}
\seealso{
Other LinkComponent: 
\code{\link{LinkComponent-class}},
\code{\link{as.StanModule.LinkComponent}()},
\code{\link{as.list.LinkComponent}()},
\code{\link{getParameters}()}
}
\concept{LinkComponent}
