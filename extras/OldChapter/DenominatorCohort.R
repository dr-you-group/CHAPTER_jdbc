# Copyright 2023 Observational Health Data Sciences and Informatics
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


#' create the denominator population settings
#'
#' @details
#' Takes as input the inputs to create denominator population
#' @param washoutPeriod                The mininum required continuous observation time (in days) prior to index
#'                                     date for a person to be included in the cohort.
#' @export
createDenominatorSettings <- function(washoutPeriod = 0, # in days
                                     sampleSize = 0, #It would be highly unusual to set sample size in denominator
                                     useObservationPeriod = FALSE, 
                                     cohortId = NULL, 
                                     startDate = "", #character
                                     endDate = "", #character
                                     firstExposureOnly = F,
                                     requireDenominatorPeriod = F,
                                     minDenominatorPeriod = 0, # in days
                                     denominatorDescription = "" #optional
){
  if(is.null(cohortId)){
    if(!useObservationPeriod)
      stop('Please specify denominator cohort ID, or use Observation Period as a denominator cohort')
  }
  if(sampleSize != 0){
    warning('it would be unusual to set sample size in denominator')
  }
  
  checkIsClass(washoutPeriod, c("numeric", "integer"))
  checkNotNull(washoutPeriod)
  checkHigherEqual(washoutPeriod, 0)
  
  # checkIsClass(priorOutcomeLookback, c("numeric", "integer"))
  # checkNotNull(priorOutcomeLookback)
  # checkHigherEqual(priorOutcomeLookback, 0)
  
  checkIsClass(firstExposureOnly, "logical")
  checkNotNull(firstExposureOnly)
  
  checkIsClass(requireDenominatorPeriod, "logical")
  checkNotNull(requireDenominatorPeriod)
  
  if(requireDenominatorPeriod){
    checkIsClass(minDenominatorPeriod, c("numeric", "integer"))
    checkNotNull(minDenominatorPeriod)
    checkHigherEqual(minDenominatorPeriod, 0)
  }
  
  if(class(startDate)!="Date") startDate = as.Date(startDate)
  if(class(endDate)!="Date") endDate = as.Date(endDate)
  
  checkIsClass(startDate, c("Date"))
  checkNotNull(startDate)
  
  checkIsClass(endDate, c("Date"))
  checkNotNull(endDate)
  
  checkLaterEqual(endDate, startDate)
  
  if(requireDenominatorPeriod){
    if(minDenominatorPeriod>(difftime(endDate,startDate, units = "days"))){
      warning('issue: minDenominatorPeriod is greater than max possible time-at-risk (end date - start date')
    }
  }
  
  if(is.null(denominatorDescription)){
    denominatorDescription = ""
  }else{
    checkIsClass(denominatorDescription, c("character"))
  }
  
  result <- list(washoutPeriod = washoutPeriod,
                 sampleSize = sampleSize,
                 useObservationPeriod = useObservationPeriod,
                 cohortId = cohortId,
                 startDate = startDate,
                 endDate = endDate,
                 firstExposureOnly = firstExposureOnly,
                 requireDenominatorPeriod = requireDenominatorPeriod,
                 minDenominatorPeriod = minDenominatorPeriod, # in days
                 denominatorDescription = denominatorDescription)
  
  class (result) <- "denominatorSettings"
  return(result)
}

#' Create a denominator population
#'
#' @details
#' Create a denominator population by enf0orcing certain inclusion and exclusion criteria
#' @export
createDenominatorPopulation <- function(connectionDetails,
                                        cdmDatabaseSchema,
                                        targetDatabaseSchema,
                                        targetTable,
                                        oracleTempSchema = NULL,
                                        denominatorSettings){
  
  checkIsClass(denominatorSettings, 'denominatorSettings')
  #ToDo: add other checks the inputs are valid

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  dbms <- connectionDetails$dbms

  # ParallelLogger::logDebug(paste0('binary: ', binary))
  # ParallelLogger::logDebug(paste0('includeAllOutcomes: ', includeAllOutcomes))
  # ParallelLogger::logDebug(paste0('firstExposureOnly: ', firstExposureOnly))
  # ParallelLogger::logDebug(paste0('washoutPeriod: ', washoutPeriod))
  
  ParallelLogger::logTrace("\nConstructing the denominatorcohort")
  # if(!is.null(createDenominatorSettings$sampleSize))  writeLines(paste("\n Sampling ",createDenominatorSettings$sampleSize, " people"))

  renderedSql <- SqlRender::loadRenderTranslateSql(
    "CreateCohorts.sql",
    packageName = "CHAPTER",
    dbms = dbms,
    tempEmulationSchema = databaseDetails$tempEmulationSchema, 
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = targetDatabaseSchema,
    cohort_table = targetTable,
    cdm_version = databaseDetails$cdmVersion,
    cohort_id = denominatorSettings$cohortId, ##should be revised!! connectionDetails
    study_start_date = denominatorSettings$startDate,
    study_end_date = denominatorSettings$endDate,
    first_only = denominatorSettings$firstExposureOnly,
    washout_period = denominatorSettings$washoutPeriod,
    use_sample = !(denominatorSettings$sampleSize==0),
    use_observation_period = denominatorSettings$useObservationPeriod, 
    sample_number = denominatorSettings$sampleSize
  )
  DatabaseConnector::executeSql(connection, renderedSql)
  
  ParallelLogger::logTrace("Fetching denominator cohorts from server")
  start <- Sys.time()
  cohortSql <- SqlRender::loadRenderTranslateSql(
    "GetCohorts.sql",
    packageName = "CHAPTER",
    dbms = dbms, 
    tempEmulationSchema = databaseDetails$tempEmulationSchema,
    target_table = targetTable,
    target_database_schema = targetDatabaseSchema,
    cdm_version = databaseDetails$cdmVersion
  )
  
  # Should be removed
  # if(!is.null(denominatorCohortId)){
  #   ParallelLogger::logTrace("Fetching denominator cohorts from server")
  #   start <- Sys.time()
  #   cohortSql <- SqlRender::loadRenderTranslateSql(
  #     "GetCohorts.sql",
  #     packageName = "CHAPTER",
  #     dbms = dbms, 
  #     tempEmulationSchema = databaseDetails$tempEmulationSchema,
  #     target_table = targetTable,
  #     target_database_schema = targetDatabaseSchema,
  #     cdm_version = databaseDetails$cdmVersion
  #   )
  # }else{
  #   ParallelLogger::logTrace("Fetching observation period as denominator cohorts from server.")
  #   start <- Sys.time()
  #   cohortSql <- SqlRender::loadRenderTranslateSql(
  #     "GetCohorts.sql",
  #     packageName = "CHAPTER",
  #     dbms = dbms, 
  #     tempEmulationSchema = databaseDetails$tempEmulationSchema,
  #     use_observation_period = TRUE,
  #     target_database_schema = targetDatabaseSchema,
  #     target_cohort_table = "observation_period",
  #     cdm_version = databaseDetails$cdmVersion
  #   )
  # }
  
  
  denominator <- DatabaseConnector::querySql(connection, cohortSql)
  colnames(denominator) <- SqlRender::snakeCaseToCamelCase(colnames(denominator))
  
  denominator <- as.data.frame(denominator)
  
  # attr(denominator, "metaData") <- metaData
  return(denominator)
  
}