% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinkComponent.R
\docType{class}
\name{LinkComponent-class}
\alias{LinkComponent-class}
\alias{.LinkComponent}
\alias{LinkComponent}
\title{\code{LinkComponent}}
\usage{
LinkComponent(stan, parameters = ParameterList(), key = "", ...)
}
\arguments{
\item{stan}{(\code{StanModule} or \code{function})\cr Stan code. See Details.}

\item{parameters}{(\code{ParameterList})\cr The parameter specification.}

\item{key}{(\code{character})\cr Link identifier. See Details.}

\item{...}{additional arguments for \code{\link[=StanModel]{StanModel()}}.}
}
\description{
\code{LinkComponent}
}
\details{
This object provides key information needed to construct a link contribution in the
survival model based on the parameters of the longitudinal model.

Each link component defines a stan function of the longitudinal model parameters which is
multiplied by a model coefficient and added to the survival models hazard function.

For full details about the specification of a \code{LinkComponent} please see
\code{vignette("extending-jmpost", package = "jmpost")}.

The \code{stan} argument can be either a \code{StanModule} object or a function.
If a function is provided, it must take a single argument, a
\code{LongitudinalModel} object, and return a \code{StanModule} object. This allows for
generic functions to be used for links such as \code{dsld} which allows for each model
to provide their own model specific implementation.
}
\section{Slots}{

\describe{
\item{\code{stan}}{(\code{StanModule})\cr See Arguments.}

\item{\code{parameters}}{(\code{ParameterList})\cr See Arguments.}

\item{\code{name}}{(\code{character})\cr See Arguments.}
}}

\seealso{
Other LinkComponent: 
\code{\link{as.StanModule.LinkComponent}()},
\code{\link{as.list.LinkComponent}()},
\code{\link{getParameters}()},
\code{\link{initialValues}()}
}
\concept{LinkComponent}
