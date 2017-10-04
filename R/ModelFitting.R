# Copyright 2017 Observational Health Data Sciences and Informatics
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

  caseTimeControl <- any(!exposureStatus$isCase)
  if (caseTimeControl) {
    cyclopsData <- Cyclops::createCyclopsData(isCaseWindow ~ exposed + isCase + exposed * isCase + strata(stratumId),
                                              data = exposureStatus,
                                              modelType = "clr_exact")
    treatmentVar <- "exposed:isCaseTRUE"
  } else {
    cyclopsData <- Cyclops::createCyclopsData(isCaseWindow ~ exposed + strata(stratumId),
                                              data = exposureStatus,
                                              modelType = "clr_exact")
    treatmentVar <- "exposed"
  }
  fit <- tryCatch({
    Cyclops::fitCyclopsModel(cyclopsData, prior = Cyclops::createPrior("none"))
  }, error = function(e) {
    e$message
  })
  if (is.character(fit)) {
    status <- fit
  } else if (fit$return_flag == "ILLCONDITIONED") {
    status <- "ILL CONDITIONED, CANNOT FIT"
  } else if (fit$return_flag == "MAX_ITERATIONS") {
    status <- "REACHED MAXIMUM NUMBER OF ITERATIONS, CANNOT FIT"
  } else {
    status <- "OK"
    coefficients <- coef(fit)
    logRr <- coefficients[names(coefficients) == treatmentVar]
    ci <- tryCatch({
      confint(fit, parm = treatmentVar)
    }, error = function(e) {
      missing(e)  # suppresses R CMD check note
      c(0, -Inf, Inf)
    })
    if (identical(ci, c(0, -Inf, Inf)))
      status <- "ERROR COMPUTING CI"
    seLogRr <- (ci[3] - ci[2])/(2 * qnorm(0.975))
    treatmentEstimate <- data.frame(logRr = logRr,
                                    logLb95 = ci[2],
                                    logUb95 = ci[3],
                                    seLogRr = seLogRr)
  }
  outcomeModel <- list()
  outcomeModel$outcomeModelTreatmentEstimate <- treatmentEstimate
  outcomeModel$outcomeModelCoefficients <- coefficients
  outcomeModel$outcomeModelStatus <- status
  outcomeCounts <- data.frame(cases = sum(exposureStatus$isCase & exposureStatus$isCaseWindow),
                              controls = sum(!exposureStatus$isCase &
    exposureStatus$isCaseWindow), casesControlWindows = sum(exposureStatus$isCase & !exposureStatus$isCaseWindow), controlsControlWindows = sum(!exposureStatus$isCase & !exposureStatus$isCaseWindow), exposedCasesCaseWindow = sum(exposureStatus$isCase &
      exposureStatus$isCaseWindow & exposureStatus$exposed), exposedCasesControlWindow = sum(exposureStatus$isCase &
      !exposureStatus$isCaseWindow & exposureStatus$exposed), exposedControlsCaseWindow = sum(!exposureStatus$isCase &
      exposureStatus$isCaseWindow & exposureStatus$exposed), exposedControlsControlWindow = sum(!exposureStatus$isCase &
      !exposureStatus$isCaseWindow & exposureStatus$exposed))
  outcomeModel$outcomeCounts <- outcomeCounts
  class(outcomeModel) <- "outcomeModel"
  delta <- Sys.time() - start
  writeLines(paste("Fitting model took", signif(delta, 3), attr(delta, "units")))
  return(outcomeModel)
}


#' @export
summary.outcomeModel <- function(object, ...) {
  class(object) <- "summary.outcomeModel"
  return(object)
}

#' @export
print.summary.outcomeModel <- function(x, ...) {
  print.outcomeModel(x)

  writeLines("")
  writeLines("Counts")
  d <- x$outcomeCounts
  colnames(d) <- c("Cases",
                   "Controls",
                   "Control win. (cases)",
                   "Control win. (controls)",
                   "Exposed case win. (cases)",
                   "Exposed control win. (cases)",
                   "Exposed case win. (controls)",
                   "Exposed control win. (controls)")
  rownames(d) <- "Count"

  printCoefmat(d)
}

#' @export
coef.outcomeModel <- function(object, ...) {
  return(object$outcomeModelTreatmentEstimate$logRr)
}

#' @export
confint.outcomeModel <- function(object, parm, level = 0.95, ...) {
  missing(parm)  # suppresses R CMD check note
  if (level != 0.95)
    stop("Only supporting 95% confidence interval")
  return(c(object$outcomeModelTreatmentEstimate$logLb95,
           object$outcomeModelTreatmentEstimate$logUb95))
}

#' @export
print.outcomeModel <- function(x, ...) {
  writeLines("Case-Crossover fitted model")
  writeLines(paste("Status:", x$outcomeModelStatus))
  writeLines("")
  d <- x$outcomeModelTreatmentEstimate
  output <- data.frame(exp(d$logRr), exp(d$logLb95), exp(d$logUb95), d$logRr, d$seLogRr)
  colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr", "seLogRr")
  rownames(output) <- "treatment"
  printCoefmat(output)
}

