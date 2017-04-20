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

#' Get the exposure status for cases (and controls).
#'
#' @details
#' This function determines the exposure status for a give, exposure ID in various windows relative to the
#' index date.
#'
#' @param subjects                A data frame as generated using the
#'                                \code{\link{selectSubjectsToInclude}} function.
#' @param caseCrossoverData       An object of type \code{caseCrossoverData} as generated using the
#'                                \code{\link{getDbCasecrossoverData}} function.
#' @param riskWindowStart        The start of the risk window (in days) relative to the index date.
#'                               This number should be non-positive.
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index date. This
#'                               number should be non-positive.
#' @param controlWindowOffsets   Offsets in days of the control windows relative to the case window.
#'
#' @return
#' A data frame with these columns: \describe{ \item{personId}{The person ID} \item{indexDate}{The
#' index date} \item{isCase}{Is the person a case or a control?} \item{stratumId}{The ID linking cases
#' and controls in a matched set} \item{isCaseWindow}{Is this a case window (as opposed to a control window)?}
#' \item{exposed}{Was the person exposed during the window?}}
#'
#' @export
getExposureStatus <- function(subjects,
                              caseCrossoverData,
                              exposureId,
                              firstExposureOnly = FALSE,
                              riskWindowStart = -30,
                              riskWindowEnd = 0,
                              controlWindowOffsets = c(-60)) {
  if (riskWindowStart > riskWindowEnd)
    stop("riskWindowStart cannot be after riskWindowEnd")
  if (riskWindowStart > 0)
    stop("Risk window cannot start after index date")
  if (riskWindowEnd > 0)
    stop("Risk window cannot end after index date")

  metaData <- attr(subjects, "metaData")

  # Create case window:
  windows <- subjects
  windows$start <- windows$indexDate + riskWindowStart
  windows$end <- windows$indexDate + riskWindowEnd
  windows$isCaseWindow <- TRUE

  # Create control windows (remove those outside of observation):
  for (offset in controlWindowOffsets){
    controlWindows <- subjects
    controlWindows$start <- controlWindows$indexDate + riskWindowStart + offset
    controlWindows$end <- controlWindows$indexDate + riskWindowEnd + offset
    controlWindows$isCaseWindow <- FALSE
    controlWindows <- controlWindows[controlWindows$start >= controlWindows$observationPeriodStartDate, ]
    windows <- rbind(windows, controlWindows)
  }
  windows$observationPeriodStartDate <- NULL
  windows$rowId <- 1:nrow(windows)

  # Subset exposures by exposureId, personId, and first exposures (if specified)
  exposures <- caseCrossoverData$exposures[caseCrossoverData$exposures$exposureId == exposureId, ]
  idx <- ffbase::`%in%`(exposures$personId, unique(subjects$personId))
  if (ffbase::any.ff(idx)) {
    subset <- exposures[idx, c("personId", "exposureStartDate", "exposureEndDate")]
    subset <- ff::as.ram(subset)
  } else {
    subset <- data.frame(personId = c(), exposureStartDate = c(), exposureEndDate = c())
  }
  if (firstExposureOnly) {
    subset <- subset[order(subset$personId, subset$exposureStartDate), ]
    idx <- duplicated(subset$personId)
    subset <- subset[!idx, ]
  }

  # Determine exposure status
  temp <- merge(windows, subset)
  temp <- temp$rowId[temp$exposureEndDate >= temp$start & temp$exposureEndDate <= temp$end]

  windows$exposed <- 0
  windows$exposed[windows$rowId %in% temp] <- 1
  windows$rowId <- NULL
  windows$start <- NULL
  windows$end <- NULL
  attr(windows, "metaData") <- metaData
  return(windows)
}
