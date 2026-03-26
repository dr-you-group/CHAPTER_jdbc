# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of CHAPTER
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

#' Get the COVID related cohorts of the CHAPTER study
#' @details 
#' This function applies the inclusion criteria for some cohorts which
#' must be created with bespoke code, and gets their attrition.
#' 
#' @param cdm                  A cdm object as created using the
#'                             \code{\link[CDMConnector]{cdmFromCon}} function 
#'                             in the CDMConnector package.
#' @param tempDir              Directory where the output is stored     
#' @param cohortNames          Names of all the cohorts stored in the cdm    
#' @param latestDataAvailability    Date of the latest data availability
#' 
#' @importFrom dplyr "%>%"
#' @export
#' 
createCovidCohorts <- function(cdm,
                               tempDir,
                               cohortNames,
                               latestDataAvailability) {

  # observation period + death table
  observationDeath <- cdm$observation_period %>%
    dplyr::select("subject_id" = "person_id", "observation_period_end_date") %>%
    dplyr::left_join(cdm$death %>% dplyr::select("subject_id" = "person_id", "death_date"),
              by = "subject_id") %>%
    dplyr::mutate(death = ifelse(!(is.na(death_date)), 1,0)) %>%
    dplyr::compute()

  # Output folder for Attrition
  outputAt <- file.path(tempDir,"Attrition")
  if (!file.exists(outputAt)){
    dir.create(outputAt, recursive = TRUE)}

  # ------------------------------------------------------------------------------
  # INFECTION COHORT FOR COVID RELATED CONDITIONS
  ParallelLogger::logInfo("- Getting COVID related outcome cohorts")
  
  newinfInit <- cdm[["acute_cohorts"]] %>%
    dplyr::filter(.data$cohort_definition_id == 1) %>% 
    dplyr::select(
      "subject_id",
      "cohort_definition_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %>% 
    dplyr::compute(temporary = FALSE, name = "infected")
  
  attr(newinfInit, "cohort_attrition") <- attr(newinfInit, "cohort_attrition") %>%
    dplyr::filter(cohort_definition_id == 1)
  attr(newinfInit, "cohort_set") <- attr(newinfInit, "cohort_set") %>%
    dplyr::filter(cohort_definition_id == 1)
  
  # No COVID infection previous 42 days
  newinfInit <- newinfInit %>%
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "acute_cohorts",
      targetCohortId = 1, 
      window = list(c(-Inf,-1)),
      nameStyle = "date_previous",
      order = "last") %>%
    dplyr::filter(is.na(.data$date_previous) | .data$date_previous < -42) %>%
    dplyr::select(-"date_previous") %>%
    dplyr::compute(temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "Event washout")

  # Check the individuals are in observation at cohort entry
  newinfInit <- newinfInit %>%
    PatientProfiles::addInObservation() %>%
    dplyr::filter(.data$in_observation == 1) %>%
    dplyr::select(-"in_observation") %>%
    dplyr::compute(temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "In observation at cohort entry")

  # Prior history 365 days
  newinfInit <- newinfInit %>%
    PatientProfiles::addPriorObservation() %>%
    dplyr::filter(.data$prior_observation >= 365) %>%
    dplyr::select(-c("prior_observation")) %>%
    dplyr::compute(temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "365 days of prior history")

  # Historical influenza 90 days
  newinfInit <- newinfInit %>%
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "acute_cohorts",
      targetCohortId = 2, 
      window = list(c(-90,-1)),
      nameStyle = "last_flu") %>%
    dplyr::filter(is.na(last_flu)) %>%
    dplyr::select(-c("last_flu")) %>%
    dplyr::compute(temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "Historical influenza")

  # keep only people starting after study_start_date and 1 year before latest data availability
  oneYearLda <- latestDataAvailability - 365
  newinfInit <- newinfInit %>%
    dplyr::filter(.data$cohort_start_date > as.Date("2018-01-01") & # studyStartDate
                    .data$cohort_start_date < .env$oneYearLda) %>%
    dplyr::compute(temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = paste0("Entry after 2018-01-01 & before ",
                                                        oneYearLda))

  # censor on observation_end, death, end of covid testing or study end date
  newinfInit <- newinfInit %>%
    PatientProfiles::addCohortIntersectDate(
      targetCohortTable = "acute_cohorts",
      targetCohortId = 1, 
      window = list(c(1, 365)),
      order = "first", 
      nameStyle = "next_covid") %>%
    dplyr::compute(temporary = FALSE)
  # censor on observation_end, death or covid (re)infection
  newinfInit <- newinfInit %>%
    dplyr::left_join(observationDeath, by = c("subject_id")) %>%
    dplyr::compute(temporary = FALSE)

  newinfInit <- newinfInit %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(dplyr::if_else(
      !(is.na(.data$death_date)) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(dplyr::if_else(
      !(is.na(.data$next_covid)) & .data$cohort_end_date > .data$next_covid, .data$next_covid, .data$cohort_end_date))) %>%
    dplyr::mutate(follow_up_days = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date")) %>%
    compute(temporary = FALSE)

  # exclude if follow-up < 120 days
  newinfInit <- newinfInit %>%
    dplyr::filter(.data$follow_up_days >= 120) %>%
    dplyr::select(-follow_up_days, -next_covid, -death_date, -death, -observation_period_end_date) %>%
    dplyr::compute(temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = ">120 days of follow-up")

  newinfInit <- newinfInit %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::compute(temporary = FALSE)

  att_infection <- newinfInit %>%
      CohortCharacteristics::summariseCohortAttrition()

  # ------------------------------------------------------------------------------
  # GETTING FINAL OUTCOME COHORTS
  numLcSymp <- as.integer(cdm[["long_covid_cohorts"]] %>%
                              dplyr::select("cohort_definition_id") %>% 
                            dplyr::distinct() %>% 
                            dplyr::tally() %>% 
                            dplyr::pull())
  numPasc <- as.integer(cdm[["pasc_cohorts"]] %>%
                           dplyr::select("cohort_definition_id") %>% 
                           dplyr::distinct() %>% 
                           dplyr::tally() %>% 
                           dplyr::pull())

  overlap <- list()
  # Only doing this for the 25 symptoms, the "lc code" is treated as another one of the chronic conditions
  for(i in 1:numLcSymp) {
    outcome <- cdm[["long_covid_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i) %>%
      dplyr::compute()

    overlap[[i]] <- newinfInit %>%
      dplyr::inner_join(
        outcome %>%
          dplyr::select(subject_id, outcome_date = cohort_start_date,
                        outcome_end = cohort_end_date),
        by = "subject_id"
      ) %>% 
      dplyr::distinct() %>% 
      dplyr::compute()

    overlap[[i]] <- overlap[[i]] %>%
      dplyr::mutate(cohort_definition_id = i) %>%
      dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
      dplyr::filter(time_diff < -90 & time_diff > -366) %>%
      dplyr::select(-time_diff) %>%
      dplyr::compute()

    overlap[[i]] <- overlap[[i]] %>%
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "long_covid_cohorts",
        targetCohortId = i,
        window = list(c(-180,-1)), 
        nameStyle = "event") %>%
      dplyr::compute()

    if("event" %in% colnames(overlap[[i]])) {
      overlap[[i]] <- overlap[[i]] %>%
        dplyr::filter(event == 0) %>%
        dplyr::select(-c(event)) %>%
        dplyr::compute()
    }

    overlap[[i]] <- overlap[[i]] %>%
      dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
      dplyr::rename("cohort_start_date" = "outcome_date") %>%
      dplyr::rename("cohort_end_date" = "outcome_end") %>%
      dplyr::distinct() %>%
      dplyr::compute()
  }

  overlapFinal <- overlap[[1]]
  for(i in 2:numLcSymp) {
    overlapFinal <- dplyr::union_all(
      overlapFinal,
      overlap[[i]]
    )
  }

  overlap <- overlapFinal %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::compute()

  overlapLcOne <- overlap %>%
    dplyr::group_by(subject_id) %>%
    dbplyr::window_order(cohort_start_date) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapLcTwo <- overlapLcOne %>%
    dplyr::inner_join(overlap %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_start_date) %>%
                 dplyr::filter(row_number() > 1) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 2) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapLcThree <- overlapLcOne %>%
    dplyr::inner_join(overlap %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_start_date) %>%
                 dplyr::filter(row_number() > 2) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 3) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapPasc <- list()
  for(i in 1:numPasc) {
    outcome <- cdm[["pasc_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i) %>%
      dplyr::compute()

    overlapPasc[[i]] <- newinfInit %>%
      dplyr::inner_join(
        outcome %>%
          dplyr::select(subject_id, outcome_date = cohort_start_date,
                        outcome_end = cohort_end_date),
        by = "subject_id"
      ) %>% 
      dplyr::distinct() %>% 
      dplyr::compute()

    overlapPasc[[i]] <- overlapPasc[[i]] %>%
      dplyr::mutate(cohort_definition_id = i) %>%
      dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
      dplyr::filter(time_diff < -90 & time_diff > -366) %>%
      dplyr::select(-time_diff) %>%
      dplyr::compute()

    overlapPasc[[i]] <- overlapPasc[[i]] %>%
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "pasc_cohorts",
        targetCohortId = i,
        window = list(c(-180,-1)), 
        nameStyle = "event") %>%
      dplyr::compute()

    if("event" %in% colnames(overlapPasc[[i]])) {
      overlapPasc[[i]] <- overlapPasc[[i]] %>%
        dplyr::filter(event == 0) %>%
        dplyr::select(-c(event)) %>%
        dplyr::compute()
    }

    overlapPasc[[i]] <- overlapPasc[[i]] %>%
      dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
      dplyr::rename("cohort_start_date" = "outcome_date") %>%
      dplyr::rename("cohort_end_date" = "outcome_end") %>%
      dplyr::distinct() %>%
      dplyr::compute()
  }

  overlapPascFinal <- overlapPasc[[1]]
  for(i in 2:numPasc) {
    overlapPascFinal <- dplyr::union_all(
      overlapPascFinal,
      overlapPasc[[i]]
    )
  }

  overlapPasc <- overlapPascFinal %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::compute()

  overlapPascOne <- overlapPasc %>%
    dplyr::group_by(subject_id) %>%
    dbplyr::window_order(cohort_start_date) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::mutate(cohort_definition_id = 4) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapPascTwo <- overlapPascOne %>%
    dplyr::inner_join(overlapPasc %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_start_date) %>%
                 dplyr::filter(row_number() > 1) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 5) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapPascThree <- overlapPascOne %>%
    dplyr::inner_join(overlapPasc %>%
                 dplyr::group_by(subject_id) %>%
                 dbplyr::window_order(cohort_start_date) %>%
                 dplyr::filter(row_number() > 2) %>%
                 dplyr::select(subject_id) %>%
                 dplyr::ungroup() %>%
                 dplyr::distinct() %>%
                 dplyr::compute(),
               by = "subject_id") %>%
    dplyr::mutate(cohort_definition_id = 6) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  overlapCohorts <- overlapLcOne %>%
    dplyr::union_all(overlapLcTwo) %>%
    dplyr::union_all(overlapLcThree) %>%
    dplyr::union_all(overlapPascOne) %>%
    dplyr::union_all(overlapPascTwo) %>%
    dplyr::union_all(overlapPascThree)

  # Check here cohort_attrition and cohort_set and all (K)
  attr(overlapCohorts, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1:6),
    cohort_name = c("long_covid_one_symptom",
                    "long_covid_two_symptoms",
                    "long_covid_three_symptoms",
                    "pasc_one_event",
                    "pasc_two_events",
                    "pasc_three_events")
  )
  attr(overlapCohorts, "cohort_count") <- getCohortCount(overlapCohorts)
  attr(overlapCohorts, "cohort_attrition") <- attr(overlapCohorts, "cohort_count") %>%
    dplyr::mutate(number_subjects = number_records,
                  reason_id = 0L,
                  reason = "Qualifying events",
                  excluded_records = 0L,
                  excluded_subjects = 0L)
  attr(overlapCohorts, "tbl_name") <- "overlap_cohorts"

  cdm[["overlap_cohorts"]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(overlapCohorts, "overlap_cohorts", FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = attr(overlapCohorts, "cohort_set"),
    cohortAttritionRef = attr(overlapCohorts, "cohort_attrition")
  )
  
  # ------------------------------------------------------------------------------
  # Print counts of all cohorts (if >5)
  ParallelLogger::logInfo("- Getting cohort counts")
  
  cohortNames <- c("chronic_cohorts", "hu_cohorts",
                   "acute_cohorts", "overlap_cohorts",
                   "pasc_cohorts")
                 #  "acute_prognosis_cohorts") 

  finalCounts <- list()
  for(i in 1:length(cohortNames)) {
    finalCounts[[i]] <- omopgenerics::cohortCount(cdm[[cohortNames[i]]]) %>%
      dplyr::inner_join(attr(cdm[[cohortNames[i]]], "cohort_set"), 
                        by = "cohort_definition_id",
                        copy = TRUE)
  }

  finalCounts <- dplyr::bind_rows(finalCounts) %>%
    dplyr::mutate(
      number_records = dplyr::if_else(number_records < 5 & number_records > 0, NA, number_records),
      number_subjects = dplyr::if_else(number_subjects < 5 & number_subjects > 0, NA, number_subjects)
    ) %>%
    dplyr::select(-"cohort_definition_id")

  # Export csv
  write.csv(finalCounts,
            file = file.path(tempDir,
                             paste0("finalcounts.csv")
            )
  )

}
