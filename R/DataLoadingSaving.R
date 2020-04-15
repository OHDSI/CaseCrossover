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

#' Load case-crossover data from the database
#'
#' @description
#' Load all data about the cases from the database.
#'
#' @return
#' Returns an object of type \code{caseCrossoverData}, containing information on the cases, the
#' nesting cohort, exposures, and optionally visits. Information about multiple outcomes can be
#' captured at once for efficiency reasons. The generic \code{summary()} function has been implemented
#' for this object.
#'
#' @param connectionDetails                   An R object of type \code{ConnectionDetails} created
#'                                            using the function \code{createConnectionDetails} in the
#'                                            \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                   The name of the database schema that contains the OMOP
#'                                            CDM instance.  Requires read permissions to this
#'                                            database. On SQL Server, this should specify both the
#'                                            database and the schema, so for example
#'                                            'cdm_instance.dbo'.
#' @param oracleTempSchema                    A schema where temp tables can be created in Oracle.
#' @param outcomeDatabaseSchema               The name of the database schema that is the location
#'                                            where the data used to define the outcome cohorts is
#'                                            available. If outcomeTable = CONDITION_ERA,
#'                                            outcomeDatabaseSchema is not used.  Requires read
#'                                            permissions to this database.
#' @param outcomeTable                        The tablename that contains the outcome cohorts.  If
#'                                            outcomeTable is not CONDITION_OCCURRENCE or
#'                                            CONDITION_ERA, then expectation is outcomeTable has
#'                                            format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                            COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeIds                          A list of ids used to define outcomes.  If outcomeTable =
#'                                            CONDITION_OCCURRENCE, the list is a set of ancestor
#'                                            CONCEPT_IDs, and all occurrences of all descendant
#'                                            concepts will be selected.  If outcomeTable <>
#'                                            CONDITION_OCCURRENCE, the list contains records found in
#'                                            COHORT_DEFINITION_ID field.
#' @param useNestingCohort                    Should the study be nested in a cohort (e.g. people with
#'                                            a specific indication)? If not, the study will be nested
#'                                            in the general population.
#' @param nestingCohortDatabaseSchema         The name of the database schema that is the location
#'                                            where the nesting cohort is defined.
#' @param nestingCohortTable                  Name of the table holding the nesting cohort. This table
#'                                            should have the same structure as the cohort table.
#' @param nestingCohortId                     A cohort definition ID identifying the records in the
#'                                            nestingCohortTable to use as nesting cohort.
#' @param useObservationEndAsNestingEndDate   When using a nesting cohort, should the observation
#'                                            period end date be used instead of the cohort end date?
#' @param getVisits                           Get data on visits? This is needed when performing a
#'                                            time- case-control study and matching on visit date is
#'                                            requested later on.
#' @param exposureDatabaseSchema              The name of the database schema that is the location
#'                                            where the exposure data used to define the exposure
#'                                            cohorts is available. If exposureTable = DRUG_ERA,
#'                                            exposureDatabaseSchema is not used but assumed to be
#'                                            cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                       The tablename that contains the exposure cohorts.  If
#'                                            exposureTable <> DRUG_ERA, then expectation is
#'                                            exposureTable has format of COHORT table:
#'                                            cohort_concept_id, SUBJECT_ID, COHORT_START_DATE,
#'                                            COHORT_END_DATE.
#' @param exposureIds                         A list of identifiers to define the exposures of
#'                                            interest. If exposureTable = DRUG_ERA, exposureIds should
#'                                            be CONCEPT_ID. If exposureTable <> DRUG_ERA, exposureIds
#'                                            is used to select the cohort_concept_id in the
#'                                            cohort-like table. If no exposureIds are provided, all
#'                                            drugs or cohorts in the exposureTable are included as
#'                                            exposures.
#' @param studyStartDate                      A calendar date specifying the minimum date where data is
#'                                            used. Date format is 'yyyymmdd'.
#' @param studyEndDate                        A calendar date specifying the maximum date where data is
#'                                            used. Date format is 'yyyymmdd'.
#' @param getTimeControlData                  Should data for time controls be fetched? (needed for
#'                                            case-time-control analyses).
#' @param maxNestingCohortSize                If the nesting cohort is larger than
#'                                            this number it will be sampled to this size. \code{maxCohortSize = 0}
#'                                            indicates no maximum size. (needed for case-time-control analyses).
#' @param maxCasesPerOutcome                  If there are more than this number of cases for a single
#'                                            outcome cases will be sampled to this size. \code{maxCasesPerOutcome = 0}
#'                                            indicates no maximum size.
#'
#' @export
getDbCaseCrossoverData <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   oracleTempSchema = cdmDatabaseSchema,
                                   outcomeDatabaseSchema = cdmDatabaseSchema,
                                   outcomeTable = "condition_era",
                                   outcomeIds = c(),
                                   useNestingCohort = FALSE,
                                   nestingCohortDatabaseSchema = cdmDatabaseSchema,
                                   nestingCohortTable = "cohort",
                                   nestingCohortId = NULL,
                                   useObservationEndAsNestingEndDate = TRUE,
                                   getVisits = FALSE,
                                   exposureDatabaseSchema = cdmDatabaseSchema,
                                   exposureTable = "drug_era",
                                   exposureIds = c(),
                                   studyStartDate = "",
                                   studyEndDate = "",
                                   getTimeControlData = FALSE,
                                   maxNestingCohortSize = 1e7,
                                   maxCasesPerOutcome = 5e5) {
  if (!getTimeControlData)
    attr(useNestingCohort, "caseCrossover") <- TRUE
  result <- CaseControl::getDbCaseData(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                       oracleTempSchema = oracleTempSchema,
                                       outcomeDatabaseSchema = outcomeDatabaseSchema,
                                       outcomeTable = outcomeTable,
                                       outcomeIds = outcomeIds,
                                       useNestingCohort = useNestingCohort,
                                       nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                                       nestingCohortTable = nestingCohortTable,
                                       nestingCohortId = nestingCohortId,
                                       useObservationEndAsNestingEndDate = useObservationEndAsNestingEndDate,
                                       getVisits = getVisits,
                                       getExposures = TRUE,
                                       exposureDatabaseSchema = exposureDatabaseSchema,
                                       exposureTable = exposureTable,
                                       exposureIds = exposureIds,
                                       studyStartDate = studyStartDate,
                                       studyEndDate = studyEndDate,
                                       maxNestingCohortSize = maxNestingCohortSize,
                                       maxCasesPerOutcome = maxCasesPerOutcome)

  class(result) <- "caseCrossoverData"
  return(result)
}

#' Save the case-crossover data to folder
#'
#' @description
#' \code{saveCaseCrossoverData} saves an object of type caseCrossoverData to folder.
#'
#' @param caseCrossoverData   An object of type \code{caseCrossoverData} as generated using
#'                            \code{\link{getDbCaseCrossoverData}}.
#' @param folder              The name of the folder where the data will be written. The folder should not
#'                            yet exist.
#'
#' @details
#' The data will be written to a set of files in the specified folder.
#'
#' @export
saveCaseCrossoverData <- function(caseCrossoverData, folder) {
  if (missing(caseCrossoverData))
    stop("Must specify caseCrossoverData")
  if (missing(folder))
    stop("Must specify folder")
  if (class(caseCrossoverData) != "caseCrossoverData")
    stop("Data not of class caseCrossoverData")

  nestingCohorts <- caseCrossoverData$nestingCohorts
  cases <- caseCrossoverData$cases
  if (caseCrossoverData$metaData$hasVisits) {
    visits <- caseCrossoverData$visits
    if (caseCrossoverData$metaData$hasExposures) {
      exposures <- caseCrossoverData$exposures
      ffbase::save.ffdf(nestingCohorts, cases, visits, exposures, dir = folder)
      open(caseCrossoverData$exposures)
    } else {
      ffbase::save.ffdf(nestingCohorts, cases, visits, dir = folder)
    }
    open(caseCrossoverData$visits)
  } else {
    if (caseCrossoverData$metaData$hasExposures) {
      exposures <- caseCrossoverData$exposures
      ffbase::save.ffdf(nestingCohorts, cases, exposures, dir = folder)
      open(caseCrossoverData$exposures)
    } else {
      ffbase::save.ffdf(nestingCohorts, cases, dir = folder)
    }
  }
  open(caseCrossoverData$nestingCohorts)
  open(caseCrossoverData$cases)
  saveRDS(caseCrossoverData$metaData, file = file.path(folder, "metaData.rds"))
  invisible(TRUE)
}

#' Load the case data from a folder
#'
#' @description
#' \code{loadCaseCrossoverData} loads an object of type caseCrossoverData from a folder in the file
#' system.
#'
#' @param folder     The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class \code{caseCrossoverData}.
#'
#' @export
loadCaseCrossoverData <- function(folder, readOnly = TRUE) {
  if (!file.exists(folder))
    stop(paste("Cannot find folder", folder))
  if (!file.info(folder)$isdir)
    stop(paste("Not a folder:", folder))

  metaData <- readRDS(file.path(folder, "metaData.rds"))
  caseCrossoverData <- list(metaData = metaData)

  temp <- setwd(folder)
  absolutePath <- setwd(temp)
  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  caseCrossoverData$nestingCohorts <- get("nestingCohorts", envir = e)
  caseCrossoverData$cases <- get("cases", envir = e)
  open(caseCrossoverData$nestingCohorts, readonly = readOnly)
  open(caseCrossoverData$cases, readonly = readOnly)
  if (caseCrossoverData$metaData$hasVisits) {
    caseCrossoverData$visits <- get("visits", envir = e)
    open(caseCrossoverData$visits, readonly = readOnly)
  }
  if (caseCrossoverData$metaData$hasExposures) {
    caseCrossoverData$exposures <- get("exposures", envir = e)
    open(caseCrossoverData$exposures, readonly = readOnly)
  }
  rm(e)
  class(caseCrossoverData) <- "caseCrossoverData"
  return(caseCrossoverData)
}

#' @export
print.caseCrossoverData <- function(x, ...) {
  writeLines("Case-crossover data object")
  writeLines("")
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  if (x$metaData$nestingCohortId != -1) {
    writeLines(paste("Nesting cohort ID:", x$metaData$nestingCohortId))
  }
  writeLines(paste("Exposure concept ID(s):",
                   paste(ff::as.ram(ffbase::unique.ff(x$exposures$exposureId)), collapse = ",")))
}

#' @export
summary.caseCrossoverData <- function(object, ...) {
  populationCount <- length(ffbase::unique.ff(object$nestingCohorts$personId))
  populationWindowCount <- nrow(object$nestingCohorts)
  outcomeCounts <- data.frame(outcomeConceptId = object$metaData$outcomeIds,
                              eventCount = 0,
                              caseCount = 0)
  for (i in 1:nrow(outcomeCounts)) {
    cases <- object$cases[object$cases$outcomeId == object$metaData$outcomeIds[i], "nestingCohortId"]
    outcomeCounts$eventCount[i] <- length(cases)
    if (outcomeCounts$eventCount[i] == 0) {
      outcomeCounts$caseCount[i] <- 0
    } else {
      t <- is.na(ffbase::ffmatch(object$nestingCohorts$nestingCohortId, ff::as.ff(cases)))
      outcomeCounts$caseCount[i] <- length(ffbase::unique.ff(object$nestingCohorts[ffbase::ffwhich(t,
                                                                                                   t == FALSE),
                                                                                   "personId"]))
    }
  }

  exposureCounts <- data.frame(exposureId = ff::as.ram(ffbase::unique.ff(object$exposures$exposureId)),
                               exposureCount = 0,
                               personCount = 0)
  for (i in 1:nrow(exposureCounts)) {
    exposures <- object$exposures[object$exposures$exposureId == exposureCounts$exposureId]
    exposureCounts$exposureCount[i] <- nrow(exposures)
    exposureCounts$personCount[i] <- length(unique(exposures$personId))
  }

  result <- list(metaData = object$metaData,
                 populationCount = populationCount,
                 populationWindowCount = populationWindowCount,
                 outcomeCounts = outcomeCounts,
                 exposureCounts = exposureCounts)
  class(result) <- "summary.caseCrossoverData"
  return(result)
}

#' @export
print.summary.caseCrossoverData <- function(x, ...) {
  writeLines("Case-crossover data object summary")
  writeLines("")
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  if (x$metaData$nestingCohortId != -1) {
    writeLines(paste("Nesting cohort ID:", x$metaData$nestingCohortId))
  }
  writeLines("")
  writeLines(paste("Population count:", paste(x$populationCount)))
  writeLines(paste("Population window count:", paste(x$populationWindowCount)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Case count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Exposure counts:")
  exposureCounts <- x$exposureCounts
  rownames(exposureCounts) <- exposureCounts$exposureId
  exposureCounts$exposureId <- NULL
  colnames(exposureCounts) <- c("Exposure count", "Person count")
  printCoefmat(exposureCounts)
}
