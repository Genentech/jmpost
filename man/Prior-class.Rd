% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prior.R
\docType{class}
\name{Prior-class}
\alias{Prior-class}
\alias{.Prior}
\alias{Prior}
\title{Prior Object and Constructor Function}
\usage{
Prior(
  parameters,
  display,
  repr_model,
  repr_data,
  centre,
  validation,
  sample,
  limits = c(-Inf, Inf)
)
}
\arguments{
\item{parameters}{(\code{list})\cr the prior distribution parameters.}

\item{display}{(\code{string})\cr the string to display when object is printed.}

\item{repr_model}{(\code{string})\cr the Stan code representation for the model block.}

\item{repr_data}{(\code{string})\cr the Stan code representation for the data block.}

\item{centre}{(\code{numeric})\cr the central point of distribution to shrink sampled values towards}

\item{validation}{(\code{list})\cr the prior distribution parameter validation functions. Must have
the same names as the \code{paramaters} slot.}

\item{sample}{(\code{function})\cr a function to sample from the prior distribution.}

\item{limits}{(\code{numeric})\cr the lower and upper limits for a truncated distribution}
}
\description{
Specifies the prior distribution in a Stan Model
}
\section{Slots}{

\describe{
\item{\code{parameters}}{(\code{list})\cr See arguments.}

\item{\code{repr_model}}{(\code{string})\cr See arguments.}

\item{\code{repr_data}}{(\code{string})\cr See arguments.}

\item{\code{centre}}{(\code{numeric})\cr See arguments.}

\item{\code{validation}}{(\code{list})\cr See arguments.}

\item{\code{display}}{(\code{string})\cr See arguments.}

\item{\code{sample}}{(\code{function})\cr See arguments.}

\item{\code{limits}}{(\code{numeric})\cr See arguments.}
}}

\seealso{
Other Prior-internal: 
\code{\link{Prior-Getter-Methods}},
\code{\link{as.StanModule.Prior}()},
\code{\link{as.character.Prior}()},
\code{\link{as_stan_list.Prior}()}
}
\concept{Prior-internal}
