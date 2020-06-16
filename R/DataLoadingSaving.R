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
#' Returns an object of type \code{CaseCrossoverData}, containing information on the cases, the
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

  class(result) <- "CaseCrossoverData"
  attr(class(result), "package") <- "CaseCrossover"
  return(result)
}
