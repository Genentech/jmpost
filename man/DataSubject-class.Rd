% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSubject.R
\docType{class}
\name{DataSubject-class}
\alias{DataSubject-class}
\alias{.DataSubject}
\alias{DataSubject}
\title{Subject Data Object and Constructor Function}
\usage{
DataSubject(data, subject, arm, study)
}
\arguments{
\item{data}{(\code{data.frame})\cr the subject-level data.}

\item{subject}{(\code{character})\cr the name of the variable containing the subject identifier.}

\item{arm}{(\code{character})\cr the name of the variable containing the arm identifier.}

\item{study}{(\code{character})\cr the name of the variable containing the study identifier.}
}
\description{
The \code{\link{DataSubject}} class handles the processing of the subject data for
fitting a \code{\link{JointModel}}.
}
\section{Slots}{

\describe{
\item{\code{data}}{(\code{data.frame})\cr the subject-level data.}

\item{\code{subject}}{(\code{character})\cr the name of the variable containing the subject identifier.}

\item{\code{arm}}{(\code{character})\cr the name of the variable containing the arm identifier.}

\item{\code{study}}{(\code{character})\cr the name of the variable containing the study identifier.}
}}

\seealso{
Other DataObjects: 
\code{\link{DataJoint-class}},
\code{\link{DataLongitudinal-class}},
\code{\link{DataSurvival-class}}

Other DataSubject: 
\code{\link{as.data.frame.DataSubject}()},
\code{\link{as_print_string.DataSubject}()},
\code{\link{as_stan_list.DataSubject}()},
\code{\link{extractVariableNames.DataSubject}()}
}
\concept{DataObjects}
\concept{DataSubject}
