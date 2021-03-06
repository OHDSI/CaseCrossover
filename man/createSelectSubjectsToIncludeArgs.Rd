% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateArgFunctions.R
\name{createSelectSubjectsToIncludeArgs}
\alias{createSelectSubjectsToIncludeArgs}
\title{Create a parameter object for the function selectSubjectsToInclude}
\usage{
createSelectSubjectsToIncludeArgs(
  firstOutcomeOnly = TRUE,
  washoutPeriod = 180,
  matchingCriteria = NULL,
  minAge = NULL,
  maxAge = NULL
)
}
\arguments{
\item{firstOutcomeOnly}{Use the first outcome per person?}

\item{washoutPeriod}{Minimum required numbers of days of observation for inclusion as either
case or control.}

\item{matchingCriteria}{If provided, a case-time-control analysis will be performed and controls
will be matched based on these criteria.}

\item{minAge}{Minimum age at which patient time will be included in the analysis. Note
that information prior to the min age is still used to determine exposure
status after the minimum age (e.g. when a prescription was started just
prior to reaching the minimum age). Also, outcomes occurring before the
minimum age is reached will be considered as prior outcomes when using
first outcomes only. Age should be specified in years, but non-integer
values are allowed. If not specified, no age restriction will be applied.}

\item{maxAge}{Maximum age at which patient time will be included in the analysis. Age
should be specified in years, but non-integer values are allowed. If not
specified, no age restriction will be applied.}
}
\description{
Create a parameter object for the function selectSubjectsToInclude
}
\details{
Create an object defining the parameter values.
}
