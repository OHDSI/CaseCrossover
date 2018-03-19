# @file MultiAnalysesVignetteDataFetch.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of CaseControl
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

# This code should be used to fetch the data that is used in the vignettes.
library(SqlRender)
library(DatabaseConnector)
library(CaseCrossover)
options(fftempdir = "s:/fftemp")

pw <- NULL
dbms <- "pdw"
user <- NULL
cdmDatabaseSchema <- "cdm_truven_mdcd_v521.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
cohortTable <- "mschuemi_cc_vignette"
server <- Sys.getenv("PDW_SERVER")
port <- Sys.getenv("PDW_PORT")
outputFolder <- "s:/temp/vignetteCaseCrossover2"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)

sql <- SqlRender::loadRenderTranslateSql("vignette.sql",
                                         packageName = "CaseControl",
                                         dbms = dbms,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            cohortTable = cohortTable)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)
DatabaseConnector::disconnect(connection)

negativeControls <- c(705178,
                      705944,
                      710650,
                      714785,
                      719174,
                      719311,
                      735340,
                      742185,
                      780369,
                      781182,
                      924724,
                      990760,
                      1110942,
                      1111706,
                      1136601,
                      1317967,
                      1501309,
                      1505346,
                      1551673,
                      1560278,
                      1584910,
                      19010309,
                      19044727,
                      40163731)
diclofenac <- 1124300
giBleed <- 1
rheumatoidArthritis <- 2

exposureOutcomeNcList <- list()
for (exposureId in c(diclofenac, negativeControls)) {
  exposureOutcomeNc <- createExposureOutcomeNestingCohort(exposureId = exposureId,
                                                          outcomeId = giBleed,
                                                          nestingCohortId = rheumatoidArthritis)
  exposureOutcomeNcList[[length(exposureOutcomeNcList) + 1]] <- exposureOutcomeNc
}

getDbCaseCrossoverDataArgs1 <- createGetDbCaseCrossoverDataArgs(useNestingCohort = FALSE)

selectSubjectsToIncludeArgs1 <- createSelectSubjectsToIncludeArgs(firstOutcomeOnly = FALSE,
                                                                  washoutPeriod = 180)

getExposureStatusArgs1 <- createGetExposureStatusArgs(firstExposureOnly = FALSE,
                                                      riskWindowStart = 0,
                                                      riskWindowEnd = 0,
                                                      controlWindowOffsets = -30)

ccrAnalysis1 <- createCcrAnalysis(analysisId = 1,
                                 description = "Simple case-crossover",
                                 getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs1,
                                 selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs1,
                                 getExposureStatusArgs = getExposureStatusArgs1)

getDbCaseCrossoverDataArgs2 <- createGetDbCaseCrossoverDataArgs(useNestingCohort = TRUE,
                                                       getTimeControlData = TRUE,
                                                       getVisits = TRUE)

ccrAnalysis2 <- createCcrAnalysis(analysisId = 2,
                                description = "Nested case-crossover",
                                getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs2,
                                selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs1,
                                getExposureStatusArgs = getExposureStatusArgs1)

matchingCriteria1 <- createMatchingCriteria(matchOnAge = TRUE,
                                           ageCaliper = 2,
                                           matchOnGender = TRUE)

selectSubjectsToIncludeArgs2 <- createSelectSubjectsToIncludeArgs(firstOutcomeOnly = FALSE,
                                                                  washoutPeriod = 180,
                                                                  matchingCriteria = matchingCriteria1)

ccrAnalysis3 <- createCcrAnalysis(analysisId = 3,
                                description = "Nested case-time-control, matching on age and gender",
                                getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs2,
                                selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs2,
                                getExposureStatusArgs = getExposureStatusArgs1)

matchingCriteria2 <- createMatchingCriteria(matchOnAge = TRUE,
                                            ageCaliper = 2,
                                            matchOnGender = TRUE,
                                            matchOnVisitDate = TRUE)

selectSubjectsToIncludeArgs3 <- createSelectSubjectsToIncludeArgs(firstOutcomeOnly = FALSE,
                                                                  washoutPeriod = 180,
                                                                  matchingCriteria = matchingCriteria2)

ccrAnalysis4 <- createCcrAnalysis(analysisId = 4,
                                description = "Nested case-time-control, matching on age, gender, and visit",
                                getDbCaseCrossoverDataArgs = getDbCaseCrossoverDataArgs2,
                                selectSubjectsToIncludeArgs = selectSubjectsToIncludeArgs3,
                                getExposureStatusArgs = getExposureStatusArgs1)

ccrAnalysisList <- list(ccrAnalysis1, ccrAnalysis2, ccrAnalysis3, ccrAnalysis4)

saveExposureOutcomeNestingCohortList(exposureOutcomeNcList,
                                     "s:/temp/vignetteCaseCrossover2/exposureOutcomeNestingCohortList.txt")
saveCcrAnalysisList(ccrAnalysisList, "s:/temp/vignetteCaseCrossover2/ccrAnalysisList.txt")

# exposureOutcomeNcList <- loadExposureOutcomeNestingCohortList('s:/temp/vignetteCaseCrossover2/exposureOutcomeNestingCohortList.txt')
# ccrAnalysisList <- loadCcrAnalysisList('s:/temp/vignetteCaseCrossover2/ccrAnalysisList.txt')

outcomeDatabaseSchema <- cohortDatabaseSchema
outcomeTable <- cohortTable
nestingCohortDatabaseSchema <- cohortDatabaseSchema
nestingCohortTable <- cohortTable
exposureDatabaseSchema <- cdmDatabaseSchema
exposureTable <- "drug_era"
getDbCaseCrossoverDataThreads = 1
selectSubjectsToIncludeThreads = 1
getExposureStatusThreads = 1
fitCaseCrossoverModelThreads = 1
exposureOutcomeNestingCohortList <- exposureOutcomeNcList


result <- runCcrAnalyses(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = cdmDatabaseSchema,
                        exposureDatabaseSchema = cdmDatabaseSchema,
                        exposureTable = "drug_era",
                        outcomeDatabaseSchema = cohortDatabaseSchema,
                        outcomeTable = cohortTable,
                        nestingCohortDatabaseSchema = cohortDatabaseSchema,
                        nestingCohortTable = cohortTable,
                        outputFolder = outputFolder,
                        exposureOutcomeNestingCohortList = exposureOutcomeNcList,
                        ccrAnalysisList = ccrAnalysisList,
                        getDbCaseCrossoverDataThreads = 1,
                        selectSubjectsToIncludeThreads = 4,
                        getExposureStatusThreads = 3,
                        fitCaseCrossoverModelThreads = 4)

# result <- readRDS('s:/temp/sccsVignette2/outcomeModelReference.rds')

analysisSum <- summarizeCcrAnalyses(result)
saveRDS(analysisSum, "s:/temp/vignetteCaseCrossover2/analysisSummary.rds")

x <- readRDS(result$modelFile[1])
summary(x)

