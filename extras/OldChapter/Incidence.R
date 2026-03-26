# Incidence calculations

# Output folder for Incidence
output_ip <- file.path(tempDir,"Incidence")
if (!file.exists(output_ip)){
  dir.create(output_ip, recursive = TRUE)}

# Output folder for TableOnes
output_to <- file.path(tempDir,"TableOne")
if (!file.exists(output_to)){
  dir.create(output_to, recursive = TRUE)}

cdm <- cdmFromCon(db, cdm_database_schema,  writeSchema = c(schema = results_database_schema,
                                                            prefix = table_stem),
                  cohortTables = CohortNames)

######################################################
message("Getting table ones")
info(logger, "-- Getting table ones")

for(tabname in CohortNames) {
  result <- PatientProfiles::summariseCharacteristics(
    cohort = cdm[[tabname]],
    ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)) # or what
  )

  write.csv(result, file = here::here(output_to, paste0("tableOne_",tabname,".csv")))
}

# Any other variable of interest for tableOne? (K)

######################################################
message("Performing healthcare utilisation analyses")
info(logger, "-- Performing healthcare utilisation analyses")

# Is this necessary? Or do we stick to incidence of the HU cohorts? (K)

# addCohortIntersectFlag for all GP/consultation/hospital HU related cohorts
# do a summarise Result kinda thing with the three interval periods (or whichever)
# that for all cohorts

# probs one run per cohort, can add flag inside summariseCharacteristics and window
# Will have to change cohort_start-date to start study for everyone to define windows of interest
# Probably do HU cohorts with codes and intersectConcept instead of jsons

# proper creation of some cohorts (HU, covid related) must be done separately

######################################################
message("Getting incidence denomiantor")
info(logger, "-- Getting incidence denomiantor")

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("2018-01-01"), as.Date(latest_data_availability)),
  daysPriorObservation = 365,
  sex = c("Male", "Female", "Both"),
  ageGroup = list(c(0,39),c(40,65),c(66,150), c(0,150))
)

# Think how to split these
result <- PatientProfiles::summariseCharacteristics(
  cohort = cdm[["denominator"]],
  ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)) # or what
)

write.csv(result, file = here::here(output_to, "tableOne_allpop.csv"))

message("Calculating incidence of acute conditions")
info(logger, "-- Calculating incidence of acute conditions")

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = AcuteCohortsName,
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,
  minCellCount = 5)

# Just to check easily for now, later do a separate visualisation part (plot, gtTables)
# and also a nice ShinyApp (K)
l
png(here(output_ip,"Acute_diagnosis.png"), unit = "in", height = 10, width = 15, res = 300)
plotIncidence(inc, facet = c("denominator_age_group", "denominator_sex"), colour = "outcome_cohort_name")
dev.off()

write.csv(inc, file = here::here(output_ip, paste0("Acute_diagnosis.csv")))

# info(logger, "-- Calculating incidence of acute conditions care")
#
# inc <- IncidencePrevalence::estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = AcuteCohortsCareName,
#   interval = c("years","months","overall"),
#   completeDatabaseIntervals = FALSE,
#   minCellCount = 5)
#
# write.csv(inc, file = here::here(output_ip, paste0("Acute_care.csv")))
#
# info(logger, "-- Calculating incidence of acute conditions prognosis")
#
# inc <- IncidencePrevalence::estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = AcuteCohortsPrognosisName,
#   interval = c("years","months","overall"),
#   completeDatabaseIntervals = FALSE,
#   minCellCount = 5)
#
# write.csv(inc, file = here::here(output_ip, paste0("Acute_prognosis.csv")))

message("Calculating incidence of chronic conditions")
info(logger, "-- Calculating incidence of chronic conditions")

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = ChronicCohortsName,
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,
  minCellCount = 5)

# Just to check easily for now
png("Chronic_diagnosis.png", unit = "in", height = "10", width = "15", resolution = "300")
plotIncidence(inc, facet = c("denominator_age_group", "denominator_sex"), colour = "outcome_cohort_name")
dev.off()

write.csv(inc, file = here::here(output_ip, paste0("Chronic_diagnosis.csv")))

# info(logger, "-- Calculating incidence of chronic conditions care")
#
# inc <- IncidencePrevalence::estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = ChronicCohortsCareName,
#   interval = c("years","months","overall"),
#   completeDatabaseIntervals = FALSE,
#   minCellCount = 5)
#
# write.csv(inc, file = here::here(output_ip, paste0("Chronic_care.csv")))
#
# info(logger, "-- Calculating incidence of chronic conditions prognosis")
#
# inc <- IncidencePrevalence::estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = ChronicCohortsPrognosisName,
#   interval = c("years","months","overall"),
#   completeDatabaseIntervals = FALSE,
#   minCellCount = 5)
#
# write.csv(inc, file = here::here(output_ip, paste0("Chronic_prognosis.csv")))

message("Calculating incidence of COVID related conditions")
info(logger, "-- Calculating incidence of COVID related conditions")

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = OverlapCohortsName,
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,
  minCellCount = 5)

# Just to check easily for now
png("COVID_related.png", unit = "in", height = "10", width = "15", resolution = "300")
plotIncidence(inc, facet = c("denominator_age_group", "denominator_sex"), colour = "outcome_cohort_name")
dev.off()

write.csv(inc, file = here::here(output_ip, paste0("COVID_related.csv")))

message("Calculating incidence of Healthcare Utilisation")
info(logger, "-- Calculating incidence of Healthcare Utilisation")

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = HUCohortsName,
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,
  minCellCount = 5)

# Just to check easily for now
png("HU.png", unit = "in", height = "10", width = "15", resolution = "300")
plotIncidence(inc, facet = c("denominator_age_group", "denominator_sex"), colour = "outcome_cohort_name")
dev.off()

write.csv(inc, file = here::here(output_ip, paste0("HU.csv")))
