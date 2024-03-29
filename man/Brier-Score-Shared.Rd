% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brier_score.R
\name{Brier-Score-Shared}
\alias{Brier-Score-Shared}
\title{Re-used documentation for Brier Score components}
\arguments{
\item{t}{(\code{numeric})\cr timepoints to calculate the desired quantity at.}

\item{times}{(\code{numeric})\cr observed times.}

\item{events}{(\code{numeric})\cr event indicator for \code{times}. Either 1 for an event or 0 for censor.}

\item{event_offset}{(\code{logical})\cr If \code{TRUE} then \eqn{G(T_i)} is evaluated at \eqn{G(T_i-)}.
Setting this as \code{TRUE} mirrors the implementation of the \code{{pec}} package.}

\item{maintain_cen_order}{(\code{logical})\cr If \code{TRUE} then, in the case of ties,
censor times are always considered
to have occurred after the event times when calculating the "reverse Kaplan-Meier" for the
IPCW estimates. Setting this to \code{TRUE} mirrors the implementation of the \code{{prodlim}}
package.}

\item{...}{not used.}
}
\description{
Re-used documentation for Brier Score components
}
\keyword{internal}
