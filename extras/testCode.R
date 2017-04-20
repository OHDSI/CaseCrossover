library(CaseCrossover)
options(fftempdir = "s:/fftemp")

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v521.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
cohortTable <- "mschuemi_cc_vignette"
port <- 17001

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

RJDBC::dbDisconnect(connection)



# Case-crossover ----------------------------------------------------------
caseCrossoverData <- getDbCaseCrossoverData(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            oracleTempSchema = oracleTempSchema,
                                            outcomeDatabaseSchema = cohortDatabaseSchema,
                                            outcomeTable = cohortTable,
                                            outcomeId = 1,
                                            exposureDatabaseSchema = cdmDatabaseSchema,
                                            exposureTable = "drug_era",
                                            exposureIds = 1124300)

saveCaseCrossoverData(caseCrossoverData, "s:/temp/vignetteCaseCrossover/caseCrossoverData")

caseCrossoverData <- loadCaseCrossoverData("s:/temp/vignetteCaseCrossover/caseCrossoverData")

caseCrossoverData

summary(caseCrossoverData)

subjects <- selectSubjectsToInclude(caseCrossoverData = caseCrossoverData,
                                    outcomeId = 1,
                                    firstOutcomeOnly = TRUE,
                                    washoutPeriod = 183)

exposureStatus <- getExposureStatus(subjects = subjects,
                                    caseCrossoverData = caseCrossoverData,
                                    exposureId = 1124300,
                                    firstExposureOnly = FALSE,
                                    riskWindowStart = -30,
                                    riskWindowEnd = 0,
                                    controlWindowOffsets = c(-60))

model <- fitCaseCrossoverModel(exposureStatus)


# Case-time-control -------------------------------------------------------
caseCrossoverData2 <- getDbCaseCrossoverData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             oracleTempSchema = oracleTempSchema,
                                             outcomeDatabaseSchema = cohortDatabaseSchema,
                                             outcomeTable = cohortTable,
                                             outcomeId = 1,
                                             exposureDatabaseSchema = cdmDatabaseSchema,
                                             exposureTable = "drug_era",
                                             exposureIds = 1124300,
                                             getTimeControlData = TRUE)

saveCaseCrossoverData(caseCrossoverData2, "s:/temp/vignetteCaseCrossover/caseCrossoverData2")

caseCrossoverData2 <- loadCaseCrossoverData2("s:/temp/vignetteCaseCrossover/caseCrossoverData2")

caseCrossoverData2

summary(caseCrossoverData2)

matchingCriteria <- createMatchingCriteria(controlsPerCase = 1,
                                           matchOnAge = TRUE,
                                           ageCaliper = 2,
                                           matchOnGender = TRUE)

subjects <- selectSubjectsToInclude(caseCrossoverData = caseCrossoverData2,
                                    outcomeId = 1,
                                    firstOutcomeOnly = TRUE,
                                    washoutPeriod = 183,
                                    matchingCriteria = matchingCriteria)

saveRDS(subjects, "s:/temp/vignetteCaseCrossover/subjects2")

exposureStatus <- getExposureStatus(subjects = subjects,
                                    caseCrossoverData = caseCrossoverData,
                                    exposureId = 1124300,
                                    firstExposureOnly = FALSE,
                                    riskWindowStart = -30,
                                    riskWindowEnd = 0,
                                    controlWindowOffsets = c(-60))

model <- fitCaseCrossoverModel(exposureStatus)

summary(model)

