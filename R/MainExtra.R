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

#' Execute the CHAPTER study
#' 
#' @details 
#' This function executes the incidence analyses of CHAPTER study. 
#' 
#' @param dbConnection         An object of type \code{dbConnection} as created using the
#'                             \code{\link[DBI]{dbConnect}} function in the
#'                             DBI package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write privileges in this schema. For PostgreSQL, c(schema_name, prefix = table_stem),
#'                             for SQLServer, c(catalog = catalog_name, schema = schema_name, prefix = table_stem).
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param readCohorts          A boolean to choose whether to read and instantiate the initial cohorts
#'                             from the available jsons.
#' @param createCovidCohorts   A boolean to choose whether to create the COVID related cohorts.
#' 
#' @importFrom dplyr "%>%"
#' @export
executeIncidencePrevalenceExtra <- function(dbConnection,
                                            cdmDatabaseSchema,
                                            cohortDatabaseSchema,
                                            outputFolder,
                                            databaseId,
                                            readCohorts = TRUE){
  
  # Create zip file for results
  zipName <- paste0(databaseId,"_ResultsExtra")
  tempDir <- file.path(outputFolder, zipName)
  if (!file.exists(tempDir))
    dir.create(tempDir, recursive = TRUE)
  tempDirCreated <- TRUE
  
  # Start log
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  
  cdm <- cdmFromCon(dbConnection, cdmDatabaseSchema,
                    writeSchema = cohortDatabaseSchema,
                    cdmName = databaseId)
  
  # Get the cdm snapshot
  snapshot <- CDMConnector::snapshot(cdm)
  write.csv(snapshot, 
            file = here::here(
              tempDir, 
              paste0(attr(cdm, "cdm_name"),"_snapshot.csv")))
  ParallelLogger::logInfo("CDM snapshot retrieved")
  
  # Retrieve the cohorts from the json files
  if(readCohorts) {
    cohortsToCreate <- system.file("settings",
                                   "CohortsToCreateExtra.csv",
                                   package = "CHAPTER") %>% read.csv()
    
    tableNames <- cohortsToCreate %>%
      dplyr::pull("table_name") %>%
      unique()
    
    for(tn in tableNames) {
      folderName <- sub('^(\\w?)', '\\U\\1', tn, perl=T)
      folderName <- gsub('\\_(\\w?)', '\\U\\1', folderName, perl=T)
      cohortsToCreateWorking <- CDMConnector::readCohortSet(
        system.file("additional_cohorts_extra",folderName, package = "CHAPTER"))
      
      cdm <- CDMConnector::generateCohortSet(cdm, 
                                             cohortsToCreateWorking,
                                             name = tn,
                                             overwrite = TRUE)
    }
  } else {
    cdm <- cdmFromCon(dbConnection, cdmDatabaseSchema,
                      writeSchema = cohortDatabaseSchema,
                      cdmName = databaseId,
                      cohortTables = c("chronic_cohorts_extra",
                                       "acute_cohorts_extra"))
  }
  ParallelLogger::logInfo("Initial cohorts read and instantiated")
  
  latestDataAvailability <- cdm$observation_period %>%
    dplyr::select(observation_period_end_date) %>%
    dplyr::filter(observation_period_end_date == max(observation_period_end_date)) %>%
    dplyr::pull() %>% unique() %>% as.Date()
  
  cohortNames <- c("chronic_cohorts_extra",
                   "acute_cohorts_extra")

  cdm <- cdmFromCon(dbConnection, cdmDatabaseSchema,
                    writeSchema = cohortDatabaseSchema,
                    cdmName = databaseId,
                    cohortTables = cohortNames)
  
  # Retrieve incidence analyses to perform
  analysesToDo <- system.file("settings",
                              "AnalysesToPerformExtra.csv",
                              package = "CHAPTER") %>% read.csv()
  
  # Do the incidence calculations
  getIncidenceResults(cdm = cdm,
                      analyses = analysesToDo,
                      cohortNames = cohortNames,
                      tempDir = tempDir,
                      latestDataAvailability = latestDataAvailability)
  ParallelLogger::logInfo("Incidence results calculated")
  
  # Zip the final results
  zip::zip(zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
           files = list.files(tempDir, full.names = TRUE))
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }
  ParallelLogger::logInfo("Saved all results")
  
  print("Done!")
  print("If all has worked, there should now be a zip file with your results
         in the output folder to share")
  print("Thank you for running the study!")
}
