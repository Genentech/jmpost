% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Promise.R
\name{resolvePromise.PromiseLinkComponent}
\alias{resolvePromise.PromiseLinkComponent}
\title{Resolve a \code{PromiseLinkComponent}}
\usage{
\method{resolvePromise}{PromiseLinkComponent}(object, model, ...)
}
\arguments{
\item{object}{(\code{\link{PromiseLinkComponent}}) \cr the promise to resolve}

\item{model}{(\code{\link{LongitudinalModel}}) \cr the model to resolve the promise with}

\item{...}{Not used.}
}
\value{
(\code{\link{LinkComponent}}) \cr the resolved \code{LinkComponent} object
}
\description{
Resolves a \code{\link{PromiseLinkComponent}} object to a \code{\link{LinkComponent}} object.
An error will be thrown if the returned \code{\link{LinkComponent}} object
does not have the same \code{key} slot value as the original \code{\link{PromiseLinkComponent}}.
}
