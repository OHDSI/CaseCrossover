% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateArgFunctions.R
\name{createGetDbCaseCrossoverDataArgs}
\alias{createGetDbCaseCrossoverDataArgs}
\title{Create a parameter object for the function getDbCaseCrossoverData}
\usage{
createGetDbCaseCrossoverDataArgs(
  useNestingCohort = FALSE,
  useObservationEndAsNestingEndDate = TRUE,
  getVisits = FALSE,
  studyStartDate = "",
  studyEndDate = "",
  getTimeControlData = FALSE,
  maxNestingCohortSize = 1e+07,
  maxCasesPerOutcome = 5e+05
)
}
\arguments{
\item{useNestingCohort}{Should the study be nested in a cohort (e.g. people with
a specific indication)? If not, the study will be nested
in the general population.}

\item{useObservationEndAsNestingEndDate}{When using a nesting cohort, should the observation
period end date be used instead of the cohort end date?}

\item{getVisits}{Get data on visits? This is needed when performing a
time- case-control study and matching on visit date is
requested later on.}

\item{studyStartDate}{A calendar date specifying the minimum date where data is
used. Date format is 'yyyymmdd'.}

\item{studyEndDate}{A calendar date specifying the maximum date where data is
used. Date format is 'yyyymmdd'.}

\item{getTimeControlData}{Should data for time controls be fetched? (needed for
case-time-control analyses).}

\item{maxNestingCohortSize}{If the nesting cohort is larger than this number it will
be sampled to this size. maxCohortSize = 0 indicates no
maximum size. (needed for case-time-control analyses).}

\item{maxCasesPerOutcome}{If there are more than this number of cases for a single
outcome cases will be sampled to this size.
maxCasesPerOutcome = 0 indicates no maximum size.}
}
\description{
Create a parameter object for the function getDbCaseCrossoverData
}
\details{
Create an object defining the parameter values.
}
