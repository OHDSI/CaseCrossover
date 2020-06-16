library("testthat")

test_that("Washout period for cases", {

  caseCrossoverData <- Andromeda::andromeda(cases = tibble(nestingCohortId = c(1),
                                                  outcomeId = c(1),
                                                  indexDate = as.Date(c("2000-07-01"))),
                                   nestingCohorts = tibble(nestingCohortId = c(1),
                                                           personId = c(1),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01")),
                                                           genderConceptId = c(8532),
                                                           careSiteId = c(1),
                                                           providerId = c(1)))

  # Case after washout period:
  cc <- selectSubjectsToInclude(caseCrossoverData = caseCrossoverData,
                                outcomeId = 1,
                                washoutPeriod = 180)
  expect_equal(cc$personId, c(1))

  # Case before washout period:
  cc <- selectSubjectsToInclude(caseCrossoverData = caseCrossoverData,
                                outcomeId = 1,
                                washoutPeriod = 365)
  expect_equal(nrow(cc), 0)

  close(caseCrossoverData)
})

test_that("exposure status: overlap with 1-day case window", {
  subjects <- tibble(personId = c(1),
                         indexDate = as.Date(c("2010-07-01")),
                         isCase = c(TRUE),
                         stratumId = c(1),
                         observationPeriodStartDate = as.Date(c("2000-01-01")))

  caseCrossoverData <- Andromeda::andromeda(cases = tibble(nestingCohortId = c(1),
                                                           outcomeId = c(1),
                                                           indexDate = as.Date(c("2010-07-01"))),
                                            nestingCohorts = tibble(nestingCohortId = c(1),
                                                                    personId = c(1),
                                                                    observationPeriodStartDate = as.Date(c("2000-01-01")),
                                                                    startDate = as.Date(c("2000-01-01")),
                                                                    endDate = as.Date(c("2020-01-01")),
                                                                    dateOfBirth = as.Date(c("2000-01-01")),
                                                                    genderConceptId = c(8532),
                                                                    careSiteId = c(1),
                                                                    providerId = c(1)),
                                            exposures = tibble(personId = c(1),
                                                                               exposureId = c(2),
                                                                               exposureStartDate = as.Date(c("2010-06-21")),
                                                                               exposureEndDate = as.Date(c("2010-07-05"))))

  exposureStatus <- getExposureStatus(subjects = subjects,
                                      caseCrossoverData = caseCrossoverData,
                                      exposureId = 2,
                                      firstExposureOnly = TRUE,
                                      riskWindowStart = 0,
                                      riskWindowEnd = 0,
                                      controlWindowOffsets = c(-60))

  expect_equal(exposureStatus$isCaseWindow, c(TRUE, FALSE))
  expect_equal(exposureStatus$exposed, c(1, 0))

  close(caseCrossoverData)

})

test_that("exposure status: overlap with 1-day control window", {
  subjects <- tibble(personId = c(1),
                         indexDate = as.Date(c("2010-07-01")),
                         isCase = c(TRUE),
                         stratumId = c(1),
                         observationPeriodStartDate = as.Date(c("2000-01-01")))

  caseCrossoverData <- Andromeda::andromeda(cases = tibble(nestingCohortId = c(1),
                                                           outcomeId = c(1),
                                                           indexDate = as.Date(c("2010-07-01"))),
                                            nestingCohorts = tibble(nestingCohortId = c(1),
                                                                    personId = c(1),
                                                                    observationPeriodStartDate = as.Date(c("2000-01-01")),
                                                                    startDate = as.Date(c("2000-01-01")),
                                                                    endDate = as.Date(c("2020-01-01")),
                                                                    dateOfBirth = as.Date(c("2000-01-01")),
                                                                    genderConceptId = c(8532),
                                                                    careSiteId = c(1),
                                                                    providerId = c(1)),
                                            exposures = tibble(personId = c(1),
                                                               exposureId = c(2),
                                                               exposureStartDate = as.Date(c("2010-05-01")),
                                                               exposureEndDate = as.Date(c("2010-05-30"))))

  exposureStatus <- getExposureStatus(subjects = subjects,
                                      caseCrossoverData = caseCrossoverData,
                                      exposureId = 2,
                                      firstExposureOnly = TRUE,
                                      riskWindowStart = 0,
                                      riskWindowEnd = 0,
                                      controlWindowOffsets = c(-60))

  expect_equal(exposureStatus$isCaseWindow, c(TRUE, FALSE))
  expect_equal(exposureStatus$exposed, c(0, 1))
  close(caseCrossoverData)
})
