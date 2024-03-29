% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/LongitudinalGSF.R,
%   R/LongitudinalRandomSlope.R, R/LongitudinalSteinFojo.R
\name{standard-link-methods}
\alias{standard-link-methods}
\alias{enableLink}
\alias{linkTTG}
\alias{linkDSLD}
\alias{linkIdentity}
\alias{enableLink.LongitudinalGSF}
\alias{linkDSLD.LongitudinalGSF}
\alias{linkTTG.LongitudinalGSF}
\alias{linkIdentity.LongitudinalGSF}
\alias{enableLink.LongitudinalRandomSlope}
\alias{linkDSLD.LongitudinalRandomSlope}
\alias{linkIdentity.LongitudinalRandomSlope}
\alias{enableLink.LongitudinalSteinFojo}
\alias{linkDSLD.LongitudinalSteinFojo}
\alias{linkTTG.LongitudinalSteinFojo}
\alias{linkIdentity.LongitudinalSteinFojo}
\title{Standard Link Methods}
\usage{
enableLink(object, ...)

linkTTG(object, ...)

linkDSLD(object, ...)

linkIdentity(object, ...)

\method{enableLink}{LongitudinalGSF}(object, ...)

\method{linkDSLD}{LongitudinalGSF}(object, ...)

\method{linkTTG}{LongitudinalGSF}(object, ...)

\method{linkIdentity}{LongitudinalGSF}(object, ...)

\method{enableLink}{LongitudinalRandomSlope}(object, ...)

\method{linkDSLD}{LongitudinalRandomSlope}(object, ...)

\method{linkIdentity}{LongitudinalRandomSlope}(object, ...)

\method{enableLink}{LongitudinalSteinFojo}(object, ...)

\method{linkDSLD}{LongitudinalSteinFojo}(object, ...)

\method{linkTTG}{LongitudinalSteinFojo}(object, ...)

\method{linkIdentity}{LongitudinalSteinFojo}(object, ...)
}
\arguments{
\item{object}{(\code{\link{StanModel}}) \cr A \code{\link{StanModel}} object.}

\item{...}{Not used.}
}
\description{
These generic functions enable \code{\link{LongitudinalModel}} objects to provide
their own implementations for the most common link functions.
}
\details{
Each of these methods should return a \code{\link{StanModule}} argument that implements
the models corresponding version of that link type.
For \code{enableLink} this is called once for a model regardless of how many links
are used and its purpose is to provide the stan code to initialise any
link specific objects (to avoid clashes with each individual link function declaring
the same required stan objects).

For further details on how to use these methods please see
\code{vignette("extending-jmpost", package = "jmpost")}.
}
\section{Functions}{
\itemize{
\item \code{enableLink()}: hook to include any common link code to be shared across all
link functions

\item \code{linkTTG()}: Time to growth link

\item \code{linkDSLD()}: Derivative of the SLD over time link

\item \code{linkIdentity()}: Current SLD link

}}
