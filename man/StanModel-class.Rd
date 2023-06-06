% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StanModel.R
\docType{class}
\name{StanModel-class}
\alias{StanModel-class}
\alias{.StanModel}
\alias{StanModel}
\title{\code{StanModel}}
\usage{
StanModel(stan, parameters, ...)
}
\arguments{
\item{stan}{(\code{StanModule})\cr code containing the Stan code specification.}

\item{parameters}{(\code{ParameterList})\cr the parameter specification.}

\item{...}{additional arguments for \code{\link[=StanModel]{StanModel()}}.}
}
\description{
\code{StanModel}
}
\section{Slots}{

\describe{
\item{\code{stan}}{(\code{StanModule})\cr code containing the Stan code specification.}

\item{\code{parameters}}{(\code{ParameterList})\cr the parameter specification.}
}}
