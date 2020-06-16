# @file RunAnalyses.R
#
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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the exposure-outcome-nesting cohorts of interest. This function will run
#' all specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(ccrAnalysisList) * length(exposureOutcomeNestingCohortList)`. When you provide
#' several analyses it will determine whether any of the analyses have anything in common, and will
#' take advantage of this fact. For example, if we specify several analyses that only differ in the way
#' the control windows are specified then this function will extract the data and select the subjects
#' only once, and re-use this in all the analysis.
#'
#' @param connectionDetails                  An R object of type \code{ConnectionDetails} created using
#'                                           the function \code{createConnectionDetails} in the
#'                                           \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                  The name of the database schema that contains the OMOP CDM
#'                                           instance.  Requires read permissions to this database. On
#'                                           SQL Server, this should specify both the database and the
#'                                           schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema                   A schema where temp tables can be created in Oracle.
#' @param outcomeDatabaseSchema              The name of the database schema that is the location where
#'                                           the data used to define the outcome cohorts is available.
#'                                           If outcomeTable = CONDITION_ERA, outcomeDatabaseSchema is
#'                                           not used.  Requires read permissions to this database.
#' @param outcomeTable                       The tablename that contains the outcome cohorts.  If
#'                                           outcomeTable is not CONDITION_OCCURRENCE or CONDITION_ERA,
#'                                           then expectation is outcomeTable has format of COHORT
#'                                           table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                           COHORT_START_DATE, COHORT_END_DATE.
#' @param exposureDatabaseSchema             The name of the database schema that is the location where
#'                                           the exposure data used to define the exposure cohorts is
#'                                           available. If exposureTable = DRUG_ERA,
#'                                           exposureDatabaseSchema is not used but assumed to be
#'                                           cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                      The tablename that contains the exposure cohorts.  If
#'                                           exposureTable <> drug_era, then expectation is
#'                                           exposureTable has format of COHORT table:
#'                                           cohort_definition_id, subject_id, cohort_start_date,
#'                                           cohort_end_date.
#' @param nestingCohortDatabaseSchema        The name of the database schema that is the location where
#'                                           the nesting cohort is defined.
#' @param nestingCohortTable                 Name of the table holding the nesting cohort. This table
#'                                           should have the same structure as the cohort table.
#' @param ccrAnalysisList                     A list of objects of type \code{ccrAnalysis} as created
#'                                           using the \code{\link{createCcrAnalysis}} function.
#' @param exposureOutcomeNestingCohortList   A list of objects of type
#'                                           \code{exposureOutcomeNestingCohort} as created using the
#'                                           \code{\link{createExposureOutcomeNestingCohort}} function.
#' @param outputFolder                       Name of the folder where all the outputs will written to.
#' @param getDbCaseCrossoverDataThreads               The number of parallel threads to use for building the
#'                                           caseControlData objects.
#' @param selectSubjectsToIncludeThreads              The number of parallel threads to use for selecting
#'                                           subjects
#' @param getExposureStatusThreads           The number of parallel threads to use for getting exposure
#'                                           status.
#' @param fitCaseCrossoverModelThreads       The number of parallel threads to use for fitting the
#'                                           models.
#'
#' @export
runCcrAnalyses <- function(connectionDetails,
                           cdmDatabaseSchema,
                           oracleTempSchema = cdmDatabaseSchema,
                           exposureDatabaseSchema = cdmDatabaseSchema,
                           exposureTable = "drug_era",
                           outcomeDatabaseSchema = cdmDatabaseSchema,
                           outcomeTable = "condition_era",
                           nestingCohortDatabaseSchema = cdmDatabaseSchema,
                           nestingCohortTable = "condition_era",
                           outputFolder = "./CcrOutput",
                           ccrAnalysisList,
                           exposureOutcomeNestingCohortList,
                           getDbCaseCrossoverDataThreads = 1,
                           selectSubjectsToIncludeThreads = 1,
                           getExposureStatusThreads = 1,
                           fitCaseCrossoverModelThreads = 1) {
  for (exposureOutcomeNestingCohort in exposureOutcomeNestingCohortList) stopifnot(class(exposureOutcomeNestingCohort) ==
                                                                                     "exposureOutcomeNestingCohort")
  for (ccrAnalysis in ccrAnalysisList) stopifnot(class(ccrAnalysis) == "ccrAnalysis")
  uniqueExposureOutcomeNcList <- unique(ParallelLogger::selectFromList(exposureOutcomeNestingCohortList,
                                                                    c("exposureId",
                                                                      "outcomeId",
                                                                      "nestingCohortId")))
  if (length(uniqueExposureOutcomeNcList) != length(exposureOutcomeNestingCohortList))
    stop("Duplicate exposure-outcome-nesting cohort combinations are not allowed")
  uniqueAnalysisIds <- unlist(unique(ParallelLogger::selectFromList(ccrAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(ccrAnalysisList))
    stop("Duplicate analysis IDs are not allowed")

  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  outcomeReference <- data.frame()
  for (ccrAnalysis in ccrAnalysisList) {
    analysisId <- ccrAnalysis$analysisId
    for (exposureOutcomeNc in exposureOutcomeNestingCohortList) {
      exposureId <- .selectByType(ccrAnalysis$exposureType, exposureOutcomeNc$exposureId, "exposure")
      outcomeId <- .selectByType(ccrAnalysis$outcomeType, exposureOutcomeNc$outcomeId, "outcome")
      nestingCohortId <- .selectByType(ccrAnalysis$nestingCohortType,
                                       exposureOutcomeNc$nestingCohortId,
                                       "nestingCohort")
      if (is.null(nestingCohortId)) {
        nestingCohortId <- NA
      }
      row <- data.frame(exposureId = exposureId,
                        outcomeId = outcomeId,
                        nestingCohortId = nestingCohortId,
                        analysisId = analysisId)
      outcomeReference <- rbind(outcomeReference, row)
    }
  }

  cdObjectsToCreate <- list()
  getDbCaseCrossoverDataArgsList <- unique(ParallelLogger::selectFromList(ccrAnalysisList,
                                                                       c("getDbCaseCrossoverDataArgs")))
  for (d in 1:length(getDbCaseCrossoverDataArgsList)) {
    getDbCaseCrossoverDataArgs <- getDbCaseCrossoverDataArgsList[[d]]
    analyses <- ParallelLogger::matchInList(ccrAnalysisList, getDbCaseCrossoverDataArgs)
    analysesIds <- unlist(ParallelLogger::selectFromList(analyses, "analysisId"))
    if (getDbCaseCrossoverDataArgs$getDbCaseCrossoverDataArgs$useNestingCohort) {
      nestingCohortIds <- unique(outcomeReference$nestingCohortId[outcomeReference$analysisId %in%
                                                                    analysesIds])
      for (nestingCohortId in nestingCohortIds) {
        if (is.na(nestingCohortId)) {
          idx <- outcomeReference$analysisId %in% analysesIds & is.na(outcomeReference$nestingCohortId)
        } else {
          idx <- outcomeReference$analysisId %in% analysesIds & outcomeReference$nestingCohortId ==
            nestingCohortId
        }
        outcomeIds <- unique(outcomeReference$outcomeId[idx])

        cdDataFileName <- .createCaseCrossoverDataFileName(d, nestingCohortId)
        outcomeReference$caseCrossoverDataFolder[idx] <- cdDataFileName
        if (!file.exists(file.path(outputFolder, cdDataFileName))) {
          args <- list(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       outcomeDatabaseSchema = outcomeDatabaseSchema,
                       outcomeTable = outcomeTable,
                       nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                       nestingCohortTable = nestingCohortTable,
                       outcomeIds = outcomeIds,
                       nestingCohortId = nestingCohortId,
                       exposureDatabaseSchema = exposureDatabaseSchema,
                       exposureTable = exposureTable,
                       exposureIds = unique(outcomeReference$exposureId[idx]))
          args <- append(args, getDbCaseCrossoverDataArgs$getDbCaseCrossoverDataArgs)
          if (is.na(nestingCohortId)) {
            args$nestingCohortId <- NULL
            args$useObservationEndAsNestingEndDate <- FALSE
          }
          cdObjectsToCreate[[length(cdObjectsToCreate) + 1]] <- list(args = args,
                                                                     cdDataFileName = file.path(outputFolder, cdDataFileName))
        }
      }
    } else {
      idx <- outcomeReference$analysisId %in% analysesIds
      outcomeIds <- unique(outcomeReference$outcomeId[idx])
      cdDataFileName <- .createCaseCrossoverDataFileName(d)
      idx <- outcomeReference$analysisId %in% analysesIds
      outcomeReference$caseCrossoverDataFolder[idx] <- cdDataFileName
      if (!file.exists(file.path(outputFolder, cdDataFileName))) {
        args <- list(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     oracleTempSchema = oracleTempSchema,
                     outcomeDatabaseSchema = outcomeDatabaseSchema,
                     outcomeTable = outcomeTable,
                     nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                     nestingCohortTable = nestingCohortTable,
                     outcomeIds = outcomeIds,
                     nestingCohortId = nestingCohortId,
                     exposureDatabaseSchema = exposureDatabaseSchema,
                     exposureTable = exposureTable,
                     exposureIds = unique(outcomeReference$exposureId[idx]))
        args <- append(args, getDbCaseCrossoverDataArgs$getDbCaseCrossoverDataArgs)
        cdObjectsToCreate[[length(cdObjectsToCreate) + 1]] <- list(args = args,
                                                                   cdDataFileName = file.path(outputFolder, cdDataFileName))
      }
    }
  }

  subsObjectsToCreate <- list()
  selectSubjectsArgsList <- unique(ParallelLogger::selectFromList(ccrAnalysisList,
                                                               c("selectSubjectsToIncludeArgs")))
  for (i in 1:length(selectSubjectsArgsList)) {
    selectSubjectsArgs <- selectSubjectsArgsList[[i]]
    analyses <- ParallelLogger::matchInList(ccrAnalysisList, selectSubjectsArgs)
    analysesIds <- unlist(ParallelLogger::selectFromList(analyses, "analysisId"))
    cdDataFileNames <- unique(outcomeReference$caseCrossoverDataFolder[outcomeReference$analysisId %in% analysesIds])
    for (cdDataFileName in cdDataFileNames) {
      cdId <- gsub("^.*caseCrossoverData_", "", cdDataFileName)
      idx <- outcomeReference$analysisId %in% analysesIds & outcomeReference$caseCrossoverDataFolder ==
        cdDataFileName
      outcomeIds <- unique(outcomeReference$outcomeId[idx])
      for (outcomeId in outcomeIds) {
        subsFilename <- .createSubjectsFileName(cdId, i, outcomeId)
        outcomeReference$subjectsFile[idx & outcomeReference$outcomeId == outcomeId] <- subsFilename
        if (!file.exists(file.path(outputFolder, subsFilename))) {
          args <- list(outcomeId = outcomeId)
          args <- append(args, selectSubjectsArgs$selectSubjectsToIncludeArgs)
          subsObjectsToCreate[[length(subsObjectsToCreate) + 1]] <- list(args = args,
                                                                         cdDataFileName = file.path(outputFolder, cdDataFileName),
                                                                         subsFilename = file.path(outputFolder, subsFilename))
        }
      }
    }
  }

  esObjectsToCreate <- list()
  for (subsFilename in unique(outcomeReference$subjectsFile)) {
    analysisIds <- unique(outcomeReference$analysisId[outcomeReference$subjectsFile == subsFilename])
    esArgsList <- unique(sapply(ccrAnalysisList, function(x) if (x$analysisId %in% analysisIds)
      return(x$getExposureStatusArgs), simplify = FALSE))
    esArgsList <- esArgsList[!sapply(esArgsList, is.null)]
    for (es in 1:length(esArgsList)) {
      esArgs <- esArgsList[[es]]
      analysisIds <- unlist(unique(ParallelLogger::selectFromList(ParallelLogger::matchInList(ccrAnalysisList,
                                                                                        list(getExposureStatusArgs = esArgs)),
                                                               "analysisId")))
      idx <- outcomeReference$subjectsFile == subsFilename & outcomeReference$analysisId %in% analysisIds
      exposureIds <- unique(outcomeReference$exposureId[idx])
      for (exposureId in exposureIds) {
        esFilename <- .createExposureStatusFileName(subsFilename, exposureId, es)
        cdFilename <- outcomeReference$caseCrossoverDataFolder[outcomeReference$subjectsFile == subsFilename][1]
        outcomeReference$exposureStatusFile[idx & outcomeReference$exposureId == exposureId] <- esFilename
        if (!file.exists(file.path(outputFolder, esFilename))) {
          args <- esArgs
          args$exposureId <- exposureId
          esObjectsToCreate[[length(esObjectsToCreate) + 1]] <- list(args = args,
                                                                     subsFilename = file.path(outputFolder, subsFilename),
                                                                     cdFilename = file.path(outputFolder, cdFilename),
                                                                     esFilename = file.path(outputFolder, esFilename))
        }
      }
    }
  }

  modelObjectsToCreate <- list()
  for (ccrAnalysis in ccrAnalysisList) {
    # ccAnalysis = ccAnalysisList[[1]]
    analysisFolder <- paste("Analysis_", ccrAnalysis$analysisId, sep = "")
    if (!file.exists(file.path(outputFolder, analysisFolder)))
      dir.create(file.path(outputFolder, analysisFolder))
    for (i in which(outcomeReference$analysisId == ccrAnalysis$analysisId)) {
      # i = 1
      exposureId <- outcomeReference$exposureId[i]
      outcomeId <- outcomeReference$outcomeId[i]
      esFilename <- outcomeReference$exposureStatusFile[i]
      modelFilename <- .createModelFileName(analysisFolder, exposureId, outcomeId)
      outcomeReference$modelFile[i] <- modelFilename
      if (!file.exists(file.path(outputFolder, modelFilename))) {
        modelObjectsToCreate[[length(modelObjectsToCreate) + 1]] <- list(esFilename = file.path(outputFolder, esFilename),
                                                                         modelFilename = file.path(outputFolder, modelFilename))
      }
    }
  }

  saveRDS(outcomeReference, file.path(outputFolder, "outcomeModelReference.rds"))

  ### Actual construction of objects ###

  ParallelLogger::logInfo("*** Creating caseCrossoverData objects ***")
  if (length(cdObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(getDbCaseCrossoverDataThreads)
    ParallelLogger::clusterRequire(cluster, "CaseCrossover")
    dummy <- ParallelLogger::clusterApply(cluster, cdObjectsToCreate, createCaseCrossoverDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating subjects objects ***")
  if (length(subsObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(selectSubjectsToIncludeThreads)
    ParallelLogger::clusterRequire(cluster, "CaseCrossover")
    dummy <- ParallelLogger::clusterApply(cluster, subsObjectsToCreate, createSubjectsObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating exposureStatus objects ***")
  if (length(esObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(getExposureStatusThreads)
    ParallelLogger::clusterRequire(cluster, "CaseCrossover")
    dummy <- ParallelLogger::clusterApply(cluster, esObjectsToCreate, createExposureStatusObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating case-crossover model objects ***")
  if (length(modelObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(fitCaseCrossoverModelThreads)
    ParallelLogger::clusterRequire(cluster, "CaseCrossover")
    dummy <- ParallelLogger::clusterApply(cluster, modelObjectsToCreate, createModelObject)
    ParallelLogger::stopCluster(cluster)
  }

  invisible(outcomeReference)
}
createCaseCrossoverDataObject <- function(params) {
  caseCrossoverData <- do.call("getDbCaseCrossoverData", params$args)
  saveCaseCrossoverData(caseCrossoverData, params$cdDataFileName)
  return(NULL)
}

createSubjectsObject <- function(params) {
  caseCrossoverData <- loadCaseCrossoverData(params$cdDataFileName)
  params$args$caseCrossoverData <- caseCrossoverData
  subjects <- do.call("selectSubjectsToInclude", params$args)
  saveRDS(subjects, params$subsFilename)
  return(NULL)
}

createExposureStatusObject <- function(params) {
  subjects <- readRDS(params$subsFilename)
  caseCrossoverData <- loadCaseCrossoverData(params$cdFilename)
  params$args$subjects <- subjects
  params$args$caseCrossoverData <- caseCrossoverData
  exposureStatus <- do.call("getExposureStatus", params$args)
  saveRDS(exposureStatus, params$esFilename)
  return(NULL)
}

createModelObject <- function(params) {
  exposureStatus <- readRDS(params$esFilename)
  params$args$exposureStatus <- exposureStatus
  model <- do.call("fitCaseCrossoverModel", params$args)
  saveRDS(model, params$modelFilename)
  return(NULL)
}

.createCaseCrossoverDataFileName <- function(loadId, nestingCohortId = NULL) {
  name <- paste0("caseCrossoverData_cd", loadId)
  if (!is.null(nestingCohortId) && !is.na(nestingCohortId))
    name <- paste0(name, "_n", nestingCohortId)
  return(name)
}

.createSubjectsFileName <- function(cdId, i, outcomeId) {
  name <- paste0("subjects_", cdId, "_subs", i, "_o", outcomeId, ".rds")
  return(name)
}

.createExposureStatusFileName <- function(subsFilename, exposureId, es) {
  name <- gsub("subjects_", "exposureStatus_", subsFilename)
  name <- gsub(".rds", "", name)
  name <- paste0(name, "_e", exposureId, "_es", es, ".rds")
  return(name)
}

.createModelFileName <- function(folder, exposureId, outcomeId) {
  name <- paste("model_e", exposureId, "_o", outcomeId, ".rds", sep = "")
  return(file.path(folder, name))
}

.selectByType <- function(type, value, label) {
  if (is.null(type)) {
    if (is.list(value)) {
      stop(paste("Multiple ",
                 label,
                 "s specified, but none selected in analyses (comparatorType).",
                 sep = ""))
    }
    return(value)
  } else {
    if (!is.list(value) || is.null(value[type])) {
      stop(paste(label, "type not found:", type))
    }
    return(value[type])
  }
}

#' Create a summary report of the analyses
#'
#' @param outcomeReference   A data.frame as created by the \code{\link{runCcrAnalyses}} function.
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @export
summarizeCcrAnalyses <- function(outcomeReference, outputFolder) {
  columns <- c("analysisId", "exposureId", "nestingCohortId", "outcomeId")
  result <- outcomeReference[, columns]
  result$rr <- 0
  result$ci95lb <- 0
  result$ci95ub <- 0
  result$p <- 1
  result$cases <- 0
  result$controls <- 0
  result$casesControlWindows <- 0
  result$controlsControlWindows <- 0
  result$exposedCasesCaseWindow <- 0
  result$exposedCasesControlWindow <- 0
  result$exposedControlsCaseWindow <- 0
  result$exposedControlsControlWindow <- 0
  for (i in 1:nrow(outcomeReference)) {
    if (outcomeReference$modelFile[i] != "") {
      model <- readRDS(file.path(outputFolder, outcomeReference$modelFile[i]))
      result$rr[i] <- if (is.null(coef(model)))
        NA else exp(coef(model))
      result$ci95lb[i] <- if (is.null(coef(model)))
        NA else exp(confint(model)[1])
      result$ci95ub[i] <- if (is.null(coef(model)))
        NA else exp(confint(model)[2])
      if (is.null(coef(model))) {
        result$p[i] <- NA
      } else {
        z <- coef(model)/model$outcomeModelTreatmentEstimate$seLogRr
        result$p[i] <- 2 * pmin(pnorm(z), 1 - pnorm(z))
      }
      result$cases[i] <- model$outcomeCounts$cases
      result$controls[i] <- model$outcomeCounts$controls
      result$casesControlWindows[i] <- model$outcomeCounts$casesControlWindows
      result$controlsControlWindows[i] <- model$outcomeCounts$controlsControlWindows
      result$exposedCasesCaseWindow[i] <- model$outcomeCounts$exposedCasesCaseWindow
      result$exposedCasesControlWindow[i] <- model$outcomeCounts$exposedCasesControlWindow
      result$exposedControlsCaseWindow[i] <- model$outcomeCounts$exposedControlsCaseWindow
      result$exposedControlsControlWindow[i] <- model$outcomeCounts$exposedControlsControlWindow
      result$logRr[i] <- if (is.null(coef(model)))
        NA else coef(model)
      result$seLogRr[i] <- if (is.null(coef(model)))
        NA else model$outcomeModelTreatmentEstimate$seLogRr
    }
  }
  return(result)
}
