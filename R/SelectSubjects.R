# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CaseCrossover
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Select subjects to include
#'
#' @details
#' Subject to include in the study are selected for a specific outcome, optionally filtering using a
#' washout period, restricting to first occurrences of the outcome only, and restricting on age.
#' If matching criteria are provided controls will be selected for each case. These controls will be
#' used to adjust for time trends in exposure, turning the analysis into a case-time-control analysis
#' (Suissa, 1995).
#'
#' @param caseCrossoverData   An object of type \code{caseCrossoverData} as generated using the
#'                            \code{\link{getDbCaseCrossoverData}} function.
#' @param outcomeId           The outcome ID of the cases for which we need to pick controls.
#' @param firstOutcomeOnly    Use the first outcome per person?
#' @param washoutPeriod       Minimum required numbers of days of observation for inclusion as either
#'                            case or control.
#' @param matchingCriteria    If provided, a case-time-control analysis will be performed and controls
#'                            will be matched based on these criteria.
#' @param minAge              Minimum age at which patient time will be included in the analysis. Note
#'                            that information prior to the min age is still used to determine exposure
#'                            status after the minimum age (e.g. when a prescription was started just
#'                            prior to reaching the minimum age). Also, outcomes occurring before the
#'                            minimum age is reached will be considered as prior outcomes when using
#'                            first outcomes only. Age should be specified in years, but non-integer
#'                            values are allowed. If not specified, no age restriction will be applied.
#' @param maxAge              Maximum age at which patient time will be included in the analysis. Age
#'                            should be specified in years, but non-integer values are allowed. If not
#'                            specified, no age restriction will be applied.
#'
#' @return
#' A data frame with these columns: \describe{ \item{personId}{The person ID} \item{indexDate}{The
#' index date} \item{isCase}{Is the person a case or a control?} \item{stratumId}{The ID linking cases
#' and controls in a matched set} \item{observationPeriodStartDate}{The observation period start
#' date}}
#'
#' @references
#' Suissa S (1995) The case-time-control design. Epidemiology; 6:248-253.
#'
#' @export
selectSubjectsToInclude <- function(caseCrossoverData,
                                    outcomeId,
                                    firstOutcomeOnly = TRUE,
                                    washoutPeriod = 180,
                                    matchingCriteria = NULL,
                                    minAge = NULL,
                                    maxAge = NULL) {
  if (!missing(matchingCriteria) && !is.null(matchingCriteria)) {
    # Case-time-control -------------------------------------------------------
    cases <- caseCrossoverData$cases %>%
      collect()

    if (caseCrossoverData$nestingCohorts %>% count() %>% pull() == length(unique(cases$nestingCohortId)))
        stop("Case-time-control analysis specified, but data does not contain time control data. Please set getTimeControlData to TRUE when loading the data.")
    matchingCriteria <- CaseControl::createMatchingCriteria(controlsPerCase = matchingCriteria$controlsPerCase,
                                                            matchOnAge = matchingCriteria$matchOnAge,
                                                            ageCaliper = matchingCriteria$ageCaliper,
                                                            matchOnGender = matchingCriteria$matchOnGender,
                                                            matchOnProvider = matchingCriteria$matchOnProvider,
                                                            matchOnCareSite = matchingCriteria$matchOnCareSite,
                                                            matchOnVisitDate = matchingCriteria$matchOnVisitDate,
                                                            visitDateCaliper = matchingCriteria$visitDateCaliper,
                                                            matchOnTimeInCohort = matchingCriteria$matchOnTimeInCohort,
                                                            daysInCohortCaliper = matchingCriteria$daysInCohortCaliper,
                                                            removedUnmatchedCases = TRUE)
    caseControls <- CaseControl::selectControls(caseData = caseCrossoverData,
                                                outcomeId = outcomeId,
                                                firstOutcomeOnly = firstOutcomeOnly,
                                                washoutPeriod = washoutPeriod,
                                                minAge = minAge,
                                                maxAge = maxAge,
                                                controlSelectionCriteria = matchingCriteria)
  } else {
    # Case-crossover ----------------------------------------------------------
    matchingCriteria <- CaseControl::createMatchingCriteria(controlsPerCase = 0,
                                                            removedUnmatchedCases = FALSE)
    caseControls <- CaseControl::selectControls(caseData = caseCrossoverData,
                                                outcomeId = outcomeId,
                                                firstOutcomeOnly = firstOutcomeOnly,
                                                washoutPeriod = washoutPeriod,
                                                minAge = minAge,
                                                maxAge = maxAge,
                                                controlSelectionCriteria = matchingCriteria)
  }
  metaData <- attr(caseControls, "metaData")

  # Need to join to nestingCohorts to get observation period start date back:
  caseControls <- caseCrossoverData$nestingCohorts %>%
    filter(.data$personId %in% local(caseControls$personId)) %>%
    select(.data$personId, .data$observationPeriodStartDate, .data$startDate, .data$endDate) %>%
    collect() %>%
    inner_join(caseControls, by = "personId") %>%
    filter(.data$indexDate >= .data$startDate & .data$indexDate <= .data$endDate) %>%
    select(-.data$startDate, -.data$endDate)

  attr(caseControls, "metaData") <- metaData
  return(caseControls)
}


#' Create matching criteria
#'
#' @param controlsPerCase       Maximum number of controls to select per case.
#' @param matchOnAge            Match on age?
#' @param ageCaliper            Maximum difference (in years) in age when matching on age.
#' @param matchOnGender         Match on gender?
#' @param matchOnProvider       Match on provider (as specified in the person table)?
#' @param matchOnCareSite       Match on care site (as specified in the person table)?
#' @param matchOnVisitDate      Should the index date of the control be changed to the nearest visit
#'                              date?
#' @param visitDateCaliper      Maximum difference (in days) between the index date and the visit date
#'                              when matching on visit date.
#' @param matchOnTimeInCohort   Match on time in nesting cohort? When not using nesting, this is
#'                              interpreted as time observed prior to index.
#' @param daysInCohortCaliper   Maximum difference (in days) in time in cohort.
#'
#' @export
createMatchingCriteria <- function(controlsPerCase = 1,
                                   matchOnAge = TRUE,
                                   ageCaliper = 2,
                                   matchOnGender = TRUE,
                                   matchOnProvider = FALSE,
                                   matchOnCareSite = FALSE,
                                   matchOnVisitDate = FALSE,
                                   visitDateCaliper = 30,
                                   matchOnTimeInCohort = FALSE,
                                   daysInCohortCaliper = 30) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createMatchingCriteria))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "matchingCriteria"
  return(analysis)
}

#' Get the attrition table for a set of subjects
#'
#' @param subjects   A data frame of subjects as generated by the function
#'                       \code{\link{selectSubjectsToInclude}}.
#'
#' @return
#' A data frame specifying the number of cases and events after various steps of filtering.
#'
#'
#' @export
getAttritionTable <- function(subjects) {
  if (is.null(attr(subjects, "metaData")))
    stop("Metadata not found. Has this object been generated using the selectSubjectsToInclude function?")

  return(attr(subjects, "metaData")$counts)
}
