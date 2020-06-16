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

#' Fit case-crossover model
#'
#' @details
#' Fits a conditional logistic regression on the case-crossover data.
#'
#' @param exposureStatus   A data frame as generated using the \code{\link{getExposureStatus}}
#'                         function.
#'
#' @export
fitCaseCrossoverModel <- function(exposureStatus) {
  start <- Sys.time()
  coefficients <- NULL
  treatmentEstimate <- NULL
  fit <- NULL
  status <- "NO MODEL FITTED"
  if (nrow(exposureStatus) == 0) {
    fit <- "NO DATA SO COULD NOT FIT"
  } else {
    caseTimeControl <- any(!exposureStatus$isCase)
    if (caseTimeControl) {
      ParallelLogger::logInfo("Fitting case-time-control model")
      # StratumId links cases and controls. We just need a stratum ID that linkes windows:
      exposureStatus$newStratumId <- exposureStatus$stratumId
      exposureStatus$newStratumId[exposureStatus$isCase] <- exposureStatus$stratumId[exposureStatus$isCase] + max(exposureStatus$stratumId)

      form <- formula(isCaseWindow ~ exposed +  exposed * isCase + strata(newStratumId))
      # cyclopsData <- Cyclops::createCyclopsData(isCaseWindow ~ exposed + isCase + exposed * isCase + strata(stratumId),
      #                                           data = exposureStatus,
      #                                           modelType = "clr_exact")
      treatmentVar <- "exposed:isCaseTRUE"
    } else {
      ParallelLogger::logInfo("Fitting case-crossover model")
      form <- formula(isCaseWindow ~ exposed + strata(stratumId))
      # cyclopsData <- Cyclops::createCyclopsData(isCaseWindow ~ exposed + strata(stratumId),
      #                                           data = exposureStatus,
      #                                           modelType = "clr_exact")
      treatmentVar <- "exposed"
    }
    fit <- tryCatch({
      # Cyclops::fitCyclopsModel(cyclopsData, prior = Cyclops::createPrior("none"))
      suppressWarnings(survival::clogit(form, data = exposureStatus))
    }, error = function(e) {
      e$message
    })
  }
  if (is.character(fit)) {
    status <- fit
  # } else if (fit$return_flag == "ILLCONDITIONED") {
  #   status <- "ILL CONDITIONED, CANNOT FIT"
  # } else if (fit$return_flag == "MAX_ITERATIONS") {
  #   status <- "REACHED MAXIMUM NUMBER OF ITERATIONS, CANNOT FIT"
  } else {
    status <- "OK"
    coefficients <- coef(fit)
    logRr <- coefficients[names(coefficients) == treatmentVar]
    ci <- tryCatch({
      confint(fit, parm = treatmentVar)
    }, error = function(e) {
      missing(e)  # suppresses R CMD check note
      c(-Inf, Inf)
      # c(0, -Inf, Inf)
    })
    if (identical(ci, c(0, -Inf, Inf)))
      status <- "ERROR COMPUTING CI"
    seLogRr <- (ci[2] - ci[1])/(2 * qnorm(0.975))
    treatmentEstimate <- data.frame(logRr = logRr,
                                    logLb95 = ci[1],
                                    logUb95 = ci[2],
                                    seLogRr = seLogRr)
    # seLogRr <- (ci[3] - ci[2])/(2 * qnorm(0.975))
    # treatmentEstimate <- data.frame(logRr = logRr,
    #                                 logLb95 = ci[2],
    #                                 logUb95 = ci[3],
    #                                 seLogRr = seLogRr)
  }
  outcomeModel <- list()
  outcomeModel$outcomeModelTreatmentEstimate <- treatmentEstimate
  outcomeModel$outcomeModelCoefficients <- coefficients
  outcomeModel$outcomeModelStatus <- status
  if (nrow(exposureStatus) == 0) {
    outcomeCounts <- data.frame(cases = 0,
                                controls = 0,
                                casesControlWindows = 0,
                                controlsControlWindows = 0,
                                exposedCasesCaseWindow = 0,
                                exposedCasesControlWindow = 0,
                                exposedControlsCaseWindow = 0,
                                exposedControlsControlWindow = 0)
  } else {
    outcomeCounts <- data.frame(cases = sum(exposureStatus$isCase & exposureStatus$isCaseWindow),
                                controls = sum(!exposureStatus$isCase & exposureStatus$isCaseWindow),
                                casesControlWindows = sum(exposureStatus$isCase & !exposureStatus$isCaseWindow),
                                controlsControlWindows = sum(!exposureStatus$isCase & !exposureStatus$isCaseWindow),
                                exposedCasesCaseWindow = sum(exposureStatus$isCase & exposureStatus$isCaseWindow & exposureStatus$exposed),
                                exposedCasesControlWindow = sum(exposureStatus$isCase & !exposureStatus$isCaseWindow & exposureStatus$exposed),
                                exposedControlsCaseWindow = sum(!exposureStatus$isCase & exposureStatus$isCaseWindow & exposureStatus$exposed),
                                exposedControlsControlWindow = sum(!exposureStatus$isCase & !exposureStatus$isCaseWindow & exposureStatus$exposed))
  }
  outcomeModel$outcomeCounts <- outcomeCounts
  class(outcomeModel) <- "ccrOutcomeModel"
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Fitting model took", signif(delta, 3), attr(delta, "units")))
  return(outcomeModel)
}



#' @export
coef.ccrOutcomeModel <- function(object, ...) {
  return(object$outcomeModelTreatmentEstimate$logRr)
}

#' @export
confint.ccrOutcomeModel <- function(object, parm, level = 0.95, ...) {
  missing(parm)  # suppresses R CMD check note
  if (level != 0.95)
    stop("Only supporting 95% confidence interval")
  return(c(object$outcomeModelTreatmentEstimate$logLb95,
           object$outcomeModelTreatmentEstimate$logUb95))
}

#' @export
print.ccrOutcomeModel <- function(x, ...) {
  writeLines("Case-Crossover fitted model")
  writeLines(paste("Status:", x$outcomeModelStatus))
  writeLines("")
  d <- x$outcomeModelTreatmentEstimate
  output <- data.frame(exp(d$logRr), exp(d$logLb95), exp(d$logUb95), d$logRr, d$seLogRr)
  colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr", "seLogRr")
  rownames(output) <- "treatment"
  printCoefmat(output)
}

