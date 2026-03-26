# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of examplePackage
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

#' Execute the cohort diagnostics
#'
#' @details
#' This function executes the cohort diagnostics.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the exposure and outcome cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param databaseName         The full name of the database (e.g. 'Medicare Claims
#'                             Synthetic Public Use Files (SynPUFs)').
#' @param databaseDescription  A short description (several sentences) of the database.
#' @param createCohorts        Create the cohortTable table with the exposure and outcome cohorts?
#' @param runInclusionStatistics      Generate and export statistic on the cohort incusion rules?
#' @param runIncludedSourceConcepts   Generate and export the source concepts included in the cohorts?
#' @param runOrphanConcepts           Generate and export potential orphan concepts?
#' @param runTimeDistributions        Generate and export cohort time distributions?
#' @param runBreakdownIndexEvents     Generate and export the breakdown of index events?
#' @param runIncidenceRates      Generate and export the cohort incidence rates?
#' @param runCohortOverlap            Generate and export the cohort overlap?
#' @param runCohortCharacterization   Generate and export the cohort characterization?
#' @param runTemporalCohortCharacterization Generate and export the temporal cohort characterization? 
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#' @importFrom dplyr %>%
#'
#' @export
runCohortDiagnostics <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema = cdmDatabaseSchema,
                                 cohortTable = "cohort",
                                 oracleTempSchema = NULL,
                                 outputFolder,
                                 databaseId = "Unknown",
                                 databaseName = "Unknown",
                                 databaseDescription = "Unknown",
                                 createCohorts = TRUE,
                                 runInclusionStatistics = TRUE,
                                 runIncludedSourceConcepts = TRUE,
                                 runOrphanConcepts = TRUE,
                                 runBreakdownIndexEvents = TRUE,
                                 runIncidenceRate = TRUE,
                                 # runCohortCharacterization = TRUE,
                                 # runTemporalCohortCharacterization = TRUE,
                                 minCellCount = 5) {
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)

  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  
  if (createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    connection <- DatabaseConnector::connect(connectionDetails)
    .createCohorts(connection = connection,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTable = cohortTable,
                   oracleTempSchema = oracleTempSchema,
                   outputFolder = outputFolder)
    DatabaseConnector::disconnect(connection)
  }
  
  ParallelLogger::logInfo("Running study diagnostics")
  CohortDiagnostics::runCohortDiagnostics(packageName = "CHAPTER",
                                          connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          oracleTempSchema = oracleTempSchema,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          cohortTable = cohortTable,
                                          inclusionStatisticsFolder = outputFolder,
                                          exportFolder = file.path(outputFolder, "diagnosticsExport"),
                                          databaseId = databaseId,
                                          databaseName = databaseName,
                                          databaseDescription = databaseDescription,
                                          runInclusionStatistics = runInclusionStatistics,
                                          runIncludedSourceConcepts = runIncludedSourceConcepts,
                                          runOrphanConcepts = runOrphanConcepts,
                                          runTimeDistributions = FALSE,
                                          runBreakdownIndexEvents = runBreakdownIndexEvents,
                                          runIncidenceRate = runIncidenceRate,
                                          runCohortOverlap = FALSE,
                                          runCohortCharacterization = FALSE,
                                          runTemporalCohortCharacterization = FALSE,
                                          minCellCount = minCellCount)
  
  # if(runTimeDistributions){
  #   ###Re-define getTimeDistributions#####################
  #   getTimeDistributions <- function(connectionDetails = NULL,
  #                                    connection = NULL,
  #                                    cdmDatabaseSchema,
  #                                    oracleTempSchema = NULL,
  #                                    cohortDatabaseSchema = cdmDatabaseSchema,
  #                                    cohortTable = "cohort",
  #                                    cohortIds,
  #                                    cdmVersion = 5) {
  #     
  #     start <- Sys.time()
  #     
  #     if (is.null(connection)) {
  #       connection <- DatabaseConnector::connect(connectionDetails)
  #       on.exit(DatabaseConnector::disconnect(connection))
  #     }
  #     
  #     covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsPriorObservationTime = TRUE,
  #                                                                     useDemographicsPostObservationTime = TRUE,
  #                                                                     useDemographicsTimeInCohort = TRUE)
  #     
  #     data <- FeatureExtraction::getDbCovariateData(connection = connection,
  #                                                   oracleTempSchema = oracleTempSchema,
  #                                                   cdmDatabaseSchema = cdmDatabaseSchema,
  #                                                   cohortDatabaseSchema = cohortDatabaseSchema,
  #                                                   cohortTable = cohortTable,
  #                                                   cohortId = cohortIds,
  #                                                   covariateSettings = covariateSettings,
  #                                                   cdmVersion = cdmVersion,
  #                                                   aggregated = TRUE)
  #     
  #     if (is.null(data$covariatesContinuous)) {
  #       result <- tidyr::tibble()
  #     } else {
  #       result <- data$covariatesContinuous %>%
  #         dplyr::inner_join(data$covariateRef, by = "covariateId") %>%
  #         # dplyr::select(-.data$conceptId, -.data$analysisId, -.data$covariateId, -.data$result$countValue) %>% #error in CohortDiagnostics v2.0.0 (.data$results$countValue -> .data$countValue)
  #         dplyr::select(-.data$conceptId, -.data$analysisId, -.data$covariateId, -.data$countValue) %>%
  #         dplyr::rename(timeMetric = .data$covariateName,
  #                       cohortId = .data$cohortDefinitionId) %>%
  #         dplyr::collect()
  #     }
  #     attr(result, "cohortSize") <- data$metaData$populationSize
  #     delta <- Sys.time() - start
  #     ParallelLogger::logInfo("Computing time distributions took ", signif(delta, 3), " ", attr(delta, "units"))
  #     return(result)
  #   }
  #   
  #   subsetToRequiredCohorts <- function(cohorts, task, incremental, recordKeepingFile) {
  #     if (incremental) {
  #       tasks <- getRequiredTasks(cohortId = cohorts$cohortId,
  #                                 task = task,
  #                                 checksum = cohorts$checksum,
  #                                 recordKeepingFile = recordKeepingFile)
  #       return(cohorts[cohorts$cohortId %in% tasks$cohortId, ])
  #     } else {
  #       return(cohorts)
  #     }
  #   }
  #   
  #   writeToCsv <- function(data, fileName, incremental = FALSE, ...) {
  #     colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  #     if (incremental) {
  #       params <- list(...)
  #       names(params) <- SqlRender::camelCaseToSnakeCase(names(params))
  #       params$data = data
  #       params$fileName = fileName
  #       do.call(saveIncremental, params)
  #       ParallelLogger::logDebug("appending records to ", fileName)
  #     } else {
  #       if (file.exists(fileName)) {
  #         ParallelLogger::logDebug("Overwriting and replacing previous ",fileName, " with new.")
  #       } else {
  #         ParallelLogger::logDebug("creating ",fileName)
  #       }
  #       readr::write_excel_csv(x = data, 
  #                              file = fileName, 
  #                              na = "", 
  #                              append = FALSE,
  #                              delim = ",")
  #     }
  #   }
  #   
  #   recordTasksDone <- function(..., checksum, recordKeepingFile, incremental = TRUE) {
  #     if (!incremental) {
  #       return()
  #     }
  #     if (length(list(...)[[1]]) == 0) {
  #       return()
  #     }
  #     if (file.exists(recordKeepingFile)) {
  #       recordKeeping <-  readr::read_csv(recordKeepingFile, 
  #                                         col_types = readr::cols(),
  #                                         guess_max = min(1e7))
  #       recordKeeping$timeStamp <- as.character(recordKeeping$timeStamp)
  #       if ('cohortId' %in% colnames(recordKeeping)) {
  #         recordKeeping <- recordKeeping %>% 
  #           dplyr::mutate(cohortId = as.double(.data$cohortId))
  #       }
  #       if ('comparatorId' %in% colnames(recordKeeping)) {
  #         recordKeeping <- recordKeeping %>% 
  #           dplyr::mutate(comparatorId = as.double(.data$comparatorId))
  #       }
  #       idx <- getKeyIndex(list(...), recordKeeping)
  #       if (length(idx) > 0) {
  #         recordKeeping <- recordKeeping[-idx, ]
  #       }
  #     } else {
  #       recordKeeping <- dplyr::tibble()
  #     }
  #     newRow <- dplyr::as_tibble(list(...))
  #     newRow$checksum <- checksum
  #     newRow$timeStamp <-  as.character(Sys.time())
  #     recordKeeping <- dplyr::bind_rows(recordKeeping, newRow)
  #     readr::write_csv(recordKeeping, recordKeepingFile)
  #   }
  #   
  #   ################################
  #   
  #   
  #   ParallelLogger::logInfo("Creating time distributions")
  #   
  #   cohorts <- getCohortsJsonAndSql(packagename = "CHAPTER",
  #                                   cohortToCreatFIle = ,
  #                                   baseUrl = ,
  #                                   cohortSetReference = ,
  #                                   cohortIds = cohortIds)
  #   
  #   subset <- subsetToRequiredCohorts(cohorts = cohorts %>%
  #                                       dplyr::filter(.data$cohortId %in% instantiatedCohorts),
  #                                     task = "runTimeDistributions",
  #                                     incremental = FALSE,
  #                                     recordKeepingFile = FALSE)
  #   
  #   if (incremental && (length(instantiatedCohorts) - nrow(subset)) > 0) {
  #     ParallelLogger::logInfo(sprintf("Skipping %s cohorts in incremental mode.", 
  #                                     length(instantiatedCohorts) - nrow(subset)))
  #   }
  #   if (nrow(subset) > 0) {
  #     data <- getTimeDistributions(connection = connection,
  #                                  oracleTempSchema = oracleTempSchema,
  #                                  cdmDatabaseSchema = cdmDatabaseSchema,
  #                                  cohortDatabaseSchema = cohortDatabaseSchema,
  #                                  cohortTable = cohortTable,
  #                                  cdmVersion = cdmVersion,
  #                                  cohortIds = subset$cohortId)
  #     if (nrow(data) > 0) {
  #       data <- data %>%
  #         dplyr::mutate(databaseId = !!databaseId)
  #       writeToCsv(data = data,
  #                  fileName = file.path(exportFolder, "time_distribution.csv"),
  #                  incremental = incremental,
  #                  cohortId = subset$cohortId)
  #     }
  #     recordTasksDone(cohortId = subset$cohortId,
  #                     task = "runTimeDistributions",
  #                     checksum = subset$checksum,
  #                     recordKeepingFile = recordKeepingFile,
  #                     incremental = incremental)
  #   }
  # }
  # 
  # CohortDiagnostics::runCohortDiagnostics(packageName = "CHAPTER",
  #                                         connectionDetails = connectionDetails,
  #                                         cdmDatabaseSchema = cdmDatabaseSchema,
  #                                         oracleTempSchema = oracleTempSchema,
  #                                         cohortDatabaseSchema = cohortDatabaseSchema,
  #                                         cohortTable = cohortTable,
  #                                         inclusionStatisticsFolder = outputFolder,
  #                                         exportFolder = file.path(outputFolder, "diagnosticsExport"),
  #                                         databaseId = databaseId,
  #                                         databaseName = databaseName,
  #                                         databaseDescription = databaseDescription,
  #                                         runInclusionStatistics = FALSE,
  #                                         runIncludedSourceConcepts = FALSE,
  #                                         runOrphanConcepts = FALSE,
  #                                         runTimeDistributions = FALSE,
  #                                         runBreakdownIndexEvents = runBreakdownIndexEvents,
  #                                         runIncidenceRate = runIncidenceRates,
  #                                         runCohortOverlap = runCohortOverlap,
  #                                         runCohortCharacterization = runCohortCharacterization,
  #                                         runTemporalCohortCharacterization = runTemporalCohortCharacterization,
  #                                         minCellCount = minCellCount)
  
}
