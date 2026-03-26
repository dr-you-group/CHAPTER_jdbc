# Create zip file
zipName <- paste0(db.name,"_Results")
tempDir <- zipName
tempDirCreated <- FALSE
if (!dir.exists(tempDir)) {
  dir.create(tempDir)
  tempDirCreated <- TRUE
}

start <- Sys.time()

# Start log
log_file <- paste0(tempDir, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Create table names to use throughout the study
LongCovidCohortsName <- paste0("lccohorts")
PascCohortsName <- paste0("pasccohorts")
AcuteCohortsName <- paste0("accohorts")
AcuteCohortsCareName <- paste0("acccohorts")
AcuteCohortsPrognosisName <- paste0("accpohorts")
ChronicCohortsName <- paste0("ccohorts")
ChronicCohortsCareName <- paste0("cccohorts")
ChronicCohortsPrognosisName <- paste0("ccpohorts")
OverlapCohortsName <- paste0("covidcohorts")
HUCohortsName <- paste0("hucohorts")

# Create vector with all names
InitialCohortNames <- c(LongCovidCohortsName, PascCohortsName, AcuteCohortsName,
#                        AcuteCohortsCareName, AcuteCohortsPrognosisName,
                        ChronicCohortsName,
#                        ChronicCohortsCareName, ChronicCohortsPrognosisName,
                        HUCohortsName)

CohortNames <- c(AcuteCohortsName,
                 #                        AcuteCohortsCareName, AcuteCohortsPrognosisName,
                 ChronicCohortsName,
                 #                        ChronicCohortsCareName, ChronicCohortsPrognosisName,
                 HUCohortsName, OverlapCohortsName)

# Read functions needed throughout the study
source(here::here("functions.R"))

# Read initial cohorts
if (readInitialCohorts){
  info(logger, 'INSTANTIATING INITIAL COHORTS')
  cdm <- cdmFromCon(db, cdm_database_schema,
                    writeSchema = c(results_database_schema, prefix = table_stem))
  source(here("1_InitialCohorts", "InstantiateStudyCohorts.R"), local=TRUE)
  info(logger, 'GOT INITIAL COHORTS')
} else {
  info(logger, 'INITIAL COHORTS ALREADY INSTANTIATED')
  cdm <- cdmFromCon(db, cdm_database_schema,
                    writeSchema = c(results_database_schema, prefix = table_stem),
    cohortTables = InitialCohortNames)
  info(logger, 'INITIAL COHORTS READ')
}

# Get cdm snapshot
snapshot <- CDMConnector::snapshot(cdm)
write.csv(snapshot, file = here::here(tempDir, paste0(attr(cdm, "cdm_name"),"_snapshot.csv")))

# Get study cohorts (long covid and pasc related)
if(getStudyCohorts) {
  info(logger, 'GETTING LONG COVID STUDY COHORTS')
    source(here("2_StudyCohorts","getStudyCohorts.R"), local = TRUE)
  info(logger, 'GOT LONG COVID STUDY COHORTS')
}

# Objective 1: Incidence and Prevalence
if(doIncidencePrevalence) {
  info(logger, 'GETTING INCIDENCE AND PREVALENCE')
    source(here("3_IncidencePrevalence","incidence.R"), local = TRUE)
  info(logger, 'GOT INCIDENCE AND PREVALENCE')
}

# Objective 2: Time Series
# (This will probably be done manually as a post-processing of incidence results)
#if(doTimeSeries) {
#  info(logger, 'PERFORMING TIME SERIES ANALYSIS')
#    source(here("4_TimeSeries","TimeSeries_code.R"), local = TRUE)
#  info(logger, 'FINISHED TIME SERIES ANALYSIS')
#}

zip::zip(zipfile = file.path(output.folder, paste0(zipName, ".zip")),
         files = list.files(tempDir, full.names = TRUE))
if (tempDirCreated) {
  unlink(tempDir, recursive = TRUE)
}
info(logger, 'SAVED RESULTS IN THE OUTPUT FOLDER')

print("Done!")
print("If all has worked, there should now be a zip file with your results
      in the output folder to share")
print("Thank you for running the study!")
Sys.time() - start
readLines(log_file)
