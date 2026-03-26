# From initial cohorts, get all cohorts for the study
# Get attrition too

# observation period + death table
observation_death <- cdm$observation_period %>%
  dplyr::select("subject_id" = "person_id", "observation_period_end_date") %>%
  left_join(cdm$death %>% dplyr::select("subject_id" = "person_id", "death_date"),
            by = "subject_id") %>%
  mutate(death = ifelse(!(is.na(death_date)), 1,0)) %>%
  compute()

# Output folder for Attrition
output_at <- file.path(tempDir,"Attrition")
if (!file.exists(output_at)){
  dir.create(output_at, recursive = TRUE)}

# ------------------------------------------------------------------------------
# INFECTION COHORT FOR COVID RELATED CONDITIONS
message("Getting covid related outcome cohorts")
info(logger, '-- Getting covid related outcome cohorts')

newinf_init <- cdm[[AcuteCohortsName]] %>%
  dplyr::filter(.data$cohort_definition_id == 2) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute()

attrition <- dplyr::tibble(
  number_records = newinf_init %>% dplyr::tally() %>% dplyr::pull(),
  reason = "Initial events"
)

# No COVID infection previous 42 days
newinf_init <- newinf_init %>%
  addCohortIntersectDays(targetCohortTable = AcuteCohortsName,
                         targetCohortId = 2, window = list(c(-Inf,-1)),
                         order = "last", nameStyle = "date_previous") %>%
  filter(is.na(.data$date_previous) | .data$date_previous < -42) %>%
  select(-"date_previous") %>%
  compute()

attrition <- dplyr::union_all(
  attrition,
  dplyr::tibble(
    number_records = newinf_init %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Event washout"
  )
)

# Check the individuals are in observation at cohort entry
newinf_init <- newinf_init %>%
  addInObservation() %>%
  filter(.data$in_observation == 1) %>%
  select(-"in_observation") %>%
  compute()

attrition <- dplyr::union_all(
  attrition,
  dplyr::tibble(
    number_records = newinf_init %>% dplyr::tally() %>% dplyr::pull(),
    reason = "In observation at cohort entry"
  )
)

# Prior history 365 days
newinf_init <- newinf_init %>%
  addPriorObservation() %>%
  filter(.data$prior_observation >= 365) %>%
  select(-c("prior_observation")) %>%
  compute()

attrition <- dplyr::union_all(
  attrition,
  dplyr::tibble(
    number_records = newinf_init %>% dplyr::tally() %>% dplyr::pull(),
    reason = "365 days of prior history"
  )
)

# Historical influenza 90 days
newinf_init <- newinf_init %>%
  addCohortIntersectDays(targetCohortTable = AcuteCohortsName,
                         targetCohortId = 4, window = list(c(-90,-1)),
                         order = "last", nameStyle = "last_flu") %>%
  filter(is.na(last_flu)) %>%
  select(-c("last_flu")) %>%
  compute()

attrition <- dplyr::union_all(
  attrition,
  dplyr::tibble(
    number_records = newinf_init %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Historical influenza"
  )
)

# keep only people starting after study_start_date and 1 year before latest data availability
one_year_lda <- latest_data_availability - 365
newinf_init <- newinf_init %>%
  dplyr::filter(.data$cohort_start_date > study_start_date &
                  .data$cohort_start_date < .env$one_year_lda) %>%
  compute()

attrition <- dplyr::union_all(
  attrition,
  dplyr::tibble(
    number_records = newinf_init %>% dplyr::tally() %>% dplyr::pull(),
    reason = paste0("Entry after ",
                    study_start_date,
                    " & before ",
                    one_year_lda)
  )
)

# censor on observation_end, death, end of covid testing or study end date
newinf_init <- newinf_init %>%
  addCohortIntersectDate(targetCohortTable = AcuteCohortsName,
                         targetCohortId = 2, window = list(c(1, 365)),
                         order = "first", nameStyle = "next_covid") %>%
  compute()
# censor on observation_end, death, study end date, or covid (re)infection
newinf_init <- newinf_init %>%
  dplyr::left_join(observation_death, by = c("subject_id")) %>%
  compute()

newinf_init <- newinf_init %>%
  dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
    !(is.na(.data$death_date)) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
  dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
    !(is.na(.data$next_covid)) & .data$cohort_end_date > .data$next_covid, .data$next_covid, .data$cohort_end_date))) %>%
    dplyr::mutate(follow_up_days = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date")) %>%
  dplyr::mutate(reason_censoring = ifelse(!(is.na(.data$death_date)) & cohort_end_date == .data$death_date, "death",
                                          ifelse(cohort_end_date == .data$observation_period_end_date,
                                                               "end of data collection or exit from database",
                                                               ifelse(cohort_end_date == .data$next_covid, "next covid infection", NA )))) %>%
                  compute()

# exclude if follow-up < 120 days
excluded_followup <- newinf_init %>%
  dplyr::filter(.data$follow_up_days < 120) %>%
  compute()
reason_exclusion <- excluded_followup %>%
  dplyr::group_by(.data$reason_censoring) %>%
  tally() %>%
  collect()
newinf_init <- newinf_init %>%
  dplyr::filter(.data$follow_up_days >= 120) %>%
  dplyr::select(-follow_up_days, -reason_censoring, -next_covid, -death_date, -death, -observation_period_end_date) %>%
  compute()

attrition <- dplyr::union_all(
  attrition,
  dplyr::tibble(
    number_records = newinf_init %>% dplyr::tally() %>% dplyr::pull(),
    reason = "> 120 days follow-up"
  )
)

newinf_init <- newinf_init %>%
  dplyr::mutate(cohort_definition_id = 1) %>%
  compute()

# get first or subsequent events
#newinf_init <- newinf_init %>%
#  dplyr::group_by(subject_id) %>%
#  dbplyr::window_order(.data$cohort_start_date) %>%
#  dplyr::mutate(seq = row_number()) %>%
#  distinct() %>% ungroup() %>%
#  compute()

#newinf_init <- newinf_init %>%
#  dplyr::select(subject_id, cohort_definition_id,
#                                   cohort_start_date, cohort_end_date, seq) %>%
#  compute()

#first_event <- newinf_init %>%
#  dplyr::filter(seq == 1) %>%
#  dplyr::select(-seq) %>%
#  compute()
#subs_events <- newinf_init %>%
#  dplyr::filter(seq != 1) %>%
#  dplyr::select(-seq) %>%
#  compute()

#CDMConnector::recordCohortAttrition(newinf_init, reason = "First event only")

# Not for any "re-event" cohort
# No historical covid-19 infection
#first_event <- first_event %>%
#  addCohortIntersectDate(cdm, targetCohortTable = AcuteCohortsName,
#                         targetCohortId = 1, window = list(c(-Inf, -1)),
#                         order = "last", nameStyle = "event") %>%
#  compute()

#first_event <- first_event %>%
#  dplyr::filter(is.na(.data$event)) %>%
#  compute()

#CDMConnector::recordCohortAttrition(newinf_init, reason = "Historical COVID-19")

#first_event <- first_event %>%
#  dplyr::select(-c(event)) %>%
#  compute()

write.csv(
  attrition,
  file = here::here(output_at, "attrition_infection.csv")
)

# ------------------------------------------------------------------------------
# GETTING FINAL OUTCOME COHORTS
num_lc_symp <- as.integer(cdm[[LongCovidCohortsName]] %>%
  dplyr::select("cohort_definition_id") %>% distinct() %>% tally() %>% pull())
num_pasc <- as.integer(cdm[[PascCohortsName]] %>%
  dplyr::select("cohort_definition_id") %>% distinct() %>% tally() %>% pull())

overlap <- list()
# Right now I am treating the "LC code" as another symptom, can do it as its own thing (K)
for(i in 1:num_lc_symp) {
  outcome <- cdm[[LongCovidCohortsName]] %>%
    dplyr::filter(cohort_definition_id == i) %>%
    compute()

  overlap[[i]] <- newinf_init %>%
    dplyr::inner_join(
      outcome %>%
        dplyr::select(subject_id, outcome_date = cohort_start_date,
                      outcome_end = cohort_end_date),
      by = "subject_id"
    ) %>% distinct() %>% compute()

  attrition_outcome <- dplyr::tibble(
    number_records = overlap[[i]] %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Initial events"
  )

  overlap[[i]] <- overlap[[i]] %>%
    dplyr::mutate(cohort_definition_id = i) %>%
    dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
    dplyr::filter(time_diff < -90 & time_diff > -366) %>%
    dplyr::select(-time_diff) %>%
    compute()

  attrition_outcome <- dplyr::union_all(
    attrition_outcome,
    dplyr::tibble(
    number_records = overlap[[i]] %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Outcome in window (90,365)"
  ))

  overlap[[i]] <- overlap[[i]] %>%
    addCohortIntersectFlag(
      targetCohortTable = LongCovidCohortsName,
      targetCohortId = i,
      window = list(c(-180,-1)), nameStyle = "event") %>%
    compute()

  if("event" %in% colnames(overlap[[i]])) {
    overlap[[i]] <- overlap[[i]] %>%
      dplyr::filter(event == 0) %>%
      dplyr::select(-c(event)) %>%
      compute()
  }

  attrition_outcome <- dplyr::union_all(
    attrition_outcome,
    dplyr::tibble(
      number_records = overlap[[i]] %>% dplyr::tally() %>% dplyr::pull(),
      reason = "180 days of washout for the outcome"
    ))

  overlap[[i]] <- overlap[[i]] %>%
    dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
    dplyr::rename("cohort_start_date" = "outcome_date") %>%
    dplyr::rename("cohort_end_date" = "outcome_end") %>%
    distinct() %>%
    compute()

  write.csv(
    attrition_outcome,
    file = here::here(output_at, paste0("attrition_",
                      attr(cdm[[LongCovidCohortsName]], "cohort_set") %>%
                        dplyr::filter(cohort_definition_id == i) %>%
                        dplyr::pull("cohort_name")
                      ,".csv"))
  )
}

overlap_final <- overlap[[1]]
for(i in 2:num_lc_symp) {
  overlap_final <- dplyr::union_all(
    overlap_final,
    overlap[[i]]
  )
}

overlap <- overlap_final %>%
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::compute()

overlap_lc_one <- overlap %>%
  dplyr::group_by(subject_id) %>%
  dbplyr::window_order(cohort_definition_id) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::ungroup() %>%
  dplyr::compute()

overlap_lc_two <- overlap_lc_one %>%
  inner_join(overlap %>%
              dplyr::group_by(subject_id) %>%
              dbplyr::window_order(cohort_definition_id) %>%
              dplyr::filter(row_number() > 1) %>%
              dplyr::select(subject_id) %>%
              dplyr::ungroup() %>%
              dplyr::distinct() %>%
              dplyr::compute(),
            by = "subject_id") %>%
  dplyr::mutate(cohort_definition_id = 2) %>%
  dplyr::ungroup() %>%
  dplyr::compute()

overlap_lc_three <- overlap_lc_one %>%
  inner_join(overlap %>%
               dplyr::group_by(subject_id) %>%
               dbplyr::window_order(cohort_definition_id) %>%
               dplyr::filter(row_number() > 2) %>%
               dplyr::select(subject_id) %>%
               dplyr::ungroup() %>%
               dplyr::distinct() %>%
               dplyr::compute(),
             by = "subject_id") %>%
  dplyr::mutate(cohort_definition_id = 3) %>%
  dplyr::ungroup() %>%
  dplyr::compute()

overlap_pasc <- list()
for(i in 1:num_pasc) {
  outcome <- cdm[[PascCohortsName]] %>%
    dplyr::filter(cohort_definition_id == i) %>%
    compute()

  overlap_pasc[[i]] <- newinf_init %>%
    dplyr::inner_join(
      outcome %>%
        dplyr::select(subject_id, outcome_date = cohort_start_date,
                      outcome_end = cohort_end_date),
      by = "subject_id"
    ) %>% distinct() %>% compute()

  attrition_outcome <- dplyr::tibble(
      number_records = overlap_pasc[[i]] %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Initial events"
    )

  overlap_pasc[[i]] <- overlap_pasc[[i]] %>%
    dplyr::mutate(cohort_definition_id = i) %>%
    dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
    dplyr::filter(time_diff < -90 & time_diff > -366) %>%
    dplyr::select(-time_diff) %>%
    compute()

  attrition_outcome <- dplyr::union_all(
    attrition_outcome,
    dplyr::tibble(
      number_records = overlap_pasc[[i]] %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Outcome in window (90,365)"
    ))

  overlap_pasc[[i]] <- overlap_pasc[[i]] %>%
    addCohortIntersectFlag(
      targetCohortTable = PascCohortsName,
      targetCohortId = i,
      window = list(c(-180,-1)), nameStyle = "event") %>%
    compute()

  if("event" %in% colnames(overlap_pasc[[i]])) {
    overlap_pasc[[i]] <- overlap_pasc[[i]] %>%
      dplyr::filter(event == 0) %>%
      dplyr::select(-c(event)) %>%
      compute()
  }

  attrition_outcome <- dplyr::union_all(
    attrition_outcome,
    dplyr::tibble(
      number_records = overlap_pasc[[i]] %>% dplyr::tally() %>% dplyr::pull(),
      reason = "180 days of washout for the outcome"
    ))

  overlap_pasc[[i]] <- overlap_pasc[[i]] %>%
    dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
    dplyr::rename("cohort_start_date" = "outcome_date") %>%
    dplyr::rename("cohort_end_date" = "outcome_end") %>%
    distinct() %>%
    compute()

  write.csv(
    attrition_outcome,
    file = here::here(output_at, paste0("attrition_",
                      attr(cdm[[PascCohortsName]], "cohort_set") %>%
                        dplyr::filter(cohort_definition_id == i) %>%
                        dplyr::pull("cohort_name")
                      ,".csv"))
  )
}

overlap_pasc_final <- overlap_pasc[[1]]
for(i in 2:num_pasc) {
  overlap_pasc_final <- dplyr::union_all(
    overlap_pasc_final,
    overlap_pasc[[i]]
  )
}

overlap_pasc <- overlap_pasc_final %>%
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::compute()

overlap_pasc_one <- overlap_pasc %>%
  dplyr::group_by(subject_id) %>%
  dbplyr::window_order(cohort_definition_id) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(cohort_definition_id = 4) %>%
  dplyr::ungroup() %>%
  dplyr::compute()

overlap_pasc_two <- overlap_pasc_one %>%
  inner_join(overlap_pasc %>%
               dplyr::group_by(subject_id) %>%
               dbplyr::window_order(cohort_definition_id) %>%
               dplyr::filter(row_number() > 1) %>%
               dplyr::select(subject_id) %>%
               dplyr::ungroup() %>%
               dplyr::distinct() %>%
               dplyr::compute(),
             by = "subject_id") %>%
  dplyr::mutate(cohort_definition_id = 5) %>%
  dplyr::ungroup() %>%
  dplyr::compute()

overlap_pasc_three <- overlap_pasc_one %>%
  inner_join(overlap_pasc %>%
               dplyr::group_by(subject_id) %>%
               dbplyr::window_order(cohort_definition_id) %>%
               dplyr::filter(row_number() > 2) %>%
               dplyr::select(subject_id) %>%
               dplyr::ungroup() %>%
               dplyr::distinct() %>%
               dplyr::compute(),
             by = "subject_id") %>%
  dplyr::mutate(cohort_definition_id = 3) %>%
  dplyr::ungroup() %>%
  dplyr::compute()

overlap_cohorts <- overlap_lc_one %>%
  dplyr::union_all(overlap_lc_two) %>%
  dplyr::union_all(overlap_lc_three) %>%
  dplyr::union_all(overlap_pasc_one) %>%
  dplyr::union_all(overlap_pasc_two) %>%
  dplyr::union_all(overlap_pasc_three)

# Check here cohort_attrition and cohort_set and all (K)
attr(overlap_cohorts, "cohort_set") <- dplyr::tibble(
  cohort_definition_id = c(1:6),
  cohort_name = c("long_covid_one_symptom",
                  "long_covid_two_symptoms",
                  "long_covid_three_symptoms",
                  "pasc_one_event",
                  "pasc_two_events",
                  "pasc_three_events")
)
attr(overlap_cohorts, "cohort_count") <- getCohortCount(overlap_cohorts)
attr(overlap_cohorts, "cohort_attrition") <- attr(overlap_cohorts, "cohort_count") %>%
  dplyr::mutate(number_subjects = number_records,
                reason_id = 0L,
                reason = "Qualifying events",
                excluded_records = 0L,
                excluded_subjects = 0L)
attr(overlap_cohorts, "tbl_name") <- OverlapCohortsName

cdm[[OverlapCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(overlap_cohorts, OverlapCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = attr(overlap_cohorts, "cohort_set"),
  cohortAttritionRef = attr(overlap_cohorts, "cohort_attrition")
)

# GET PASC OUTCOMES AS ACUTE OUTCOMES ON THEIR OWN TOO
acute_cohorts_all <- cdm[[AcuteCohortsName]] %>%
  dplyr::union_all(cdm[[PascCohortsName]] %>%
                     dplyr::mutate(cohort_definition_id = cohort_definition_id + 5)) %>%
  compute()
attr(acute_cohorts_all, "cohort_set") <- attr(cdm[[AcuteCohortsName]], "cohort_set") %>%
  dplyr::union_all(attr(cdm[[PascCohortsName]], "cohort_set") %>%
                     dplyr::mutate(cohort_definition_id = cohort_definition_id + 5)) %>%
  compute()
attr(acute_cohorts_all, "cohort_count") <- getCohortCount(acute_cohorts_all)
attr(acute_cohorts_all, "cohort_attrition") <- attr(acute_cohorts_all, "cohort_count") %>%
  dplyr::mutate(number_subjects = number_records,
                reason_id = 0L,
                reason = "Qualifying events",
                excluded_records = 0L,
                excluded_subjects = 0L)
attr(acute_cohorts_all, "tbl_name") <- AcuteCohortsName

cdm[[AcuteCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(acute_cohorts_all, AcuteCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = attr(acute_cohorts_all, "cohort_set"),
  cohortAttritionRef = attr(acute_cohorts_all, "cohort_attrition")
)

# ------------------------------------------------------------------------------
# Print counts of all cohorts (if >5)

message("Getting cohort counts")
info(logger, '-- Getting cohort counts')

finalCounts <- list()
for(i in 1:length(CohortNames)) {
  finalCounts[[i]] <- attr(cdm[[CohortNames[i]]], "cohort_attrition") %>%
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::filter(reason_id == max(reason_id)) %>%
    dplyr::ungroup() %>%
    dplyr::select("cohort_definition_id", "number_records", "number_subjects") %>%
    dplyr::left_join(attr(cdm[[CohortNames[i]]], "cohort_set"), by = "cohort_definition_id") %>%
    dplyr::collect() %>%
    dplyr::mutate(table_name = CohortNames[i]) %>%
    dplyr::select(c("table_name", "cohort_name", "number_records", "number_subjects"))
}

finalCounts <- dplyr::bind_rows(finalCounts)

# Export csv
write.csv(finalCounts,
          file = file.path(tempDir,
                           paste0(db.name,"_finalcounts.csv")
          )
)
