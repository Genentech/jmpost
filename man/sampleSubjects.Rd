% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/SimLongitudinalGSF.R,
%   R/SimLongitudinalRandomSlope.R, R/SimLongitudinalSteinFojo.R,
%   R/SimSurvival.R
\name{sampleSubjects}
\alias{sampleSubjects}
\alias{sampleSubjects.SimLongitudinalGSF}
\alias{sampleSubjects.SimLongitudinalRandomSlope}
\alias{sampleSubjects.SimLongitudinalSteinFojo}
\alias{sampleSubjects.SimSurvival}
\title{Generate Simulated Subjects}
\usage{
sampleSubjects(object, subjects_df)

\method{sampleSubjects}{SimLongitudinalGSF}(object, subjects_df)

\method{sampleSubjects}{SimLongitudinalRandomSlope}(object, subjects_df)

\method{sampleSubjects}{SimLongitudinalSteinFojo}(object, subjects_df)

\method{sampleSubjects}{SimSurvival}(object, subjects_df)
}
\arguments{
\item{object}{(\code{SimLongitudinal} or \code{SimSurvival}) \cr object to generate subjects from.}

\item{subjects_df}{(\code{data.frame}) \cr the subjects to generate observations for. See details.}
}
\description{
Generate Simulated Subjects
}
\details{
The \code{subjects_df} argument should be a \code{data.frame} with 1 row per desired subject to create
with the following columns:
\itemize{
\item \code{study} (\code{factor}) the study identifier.
\item \code{arm} (\code{factor}) the treatment arm identifier.
\item \code{pt} (\code{character}) the subject identifier.
}

This method takes care of generating all the individual subject data required for the
\code{\link{sampleObservations}} method to generate the observations.
}
