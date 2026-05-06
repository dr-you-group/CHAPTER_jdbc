#' Execute the CHAPTER study (ATLAS-compatible version)
#'
#' @details
#' This function executes the incidence analyses of the CHAPTER study using only
#' the standard ATLAS stack: DatabaseConnector, SqlRender, CohortGenerator, etc.
#'
#' @param connectionDetails    An object as created by
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}}.
#' @param cdmDatabaseSchema    Schema name where OMOP CDM resides (e.g. 'cdm.dbo').
#' @param cohortDatabaseSchema Schema name where intermediate cohorts will be stored.
#' @param writePrefix          Optional prefix for tables to be created in
#'                             \code{cohortDatabaseSchema}.
#' @param outputFolder         Local folder for results (no network drive).
#' @param databaseId           Short identifier of the database (e.g. 'YUHS').
#' @param readCohorts          If TRUE, instantiate cohorts from JSONs.
#'                             If FALSE, assume they already exist.
#' @param createAllergyCohorts If TRUE, create allergy cohorts.
#' @param createCancerCohorts  If TRUE, create cancer cohorts.
#' @param createCardioCohorts  If TRUE, create cardiovascular cohorts.
#' @param createGeneralCohorts If TRUE, create healthcare utilization and mortality cohorts.
#' @param createPulmoCohorts   If TRUE, create pulmonary cohorts.
#' @param createDenominator    If TRUE, create the denominator cohort table.
#' @param calculateIncidence   If TRUE, calculate incidence.
#' @param continueOnIncidenceError If TRUE, continue when one outcome incidence query fails.
#' @param minCellCount         Minimum allowed count to report; smaller cells are suppressed.
#'
#' @importFrom dplyr %>%
#' @export
executeIncidencePrevalenceFinal <- function(connectionDetails,
                                            cdmDatabaseSchema,
                                            cohortDatabaseSchema,
                                            writePrefix = NULL,
                                            outputFolder,
                                            databaseId,
                                            readCohorts = TRUE,
                                            createAllergyCohorts = TRUE,
                                            createCancerCohorts = TRUE,
                                            createCardioCohorts = TRUE,
                                            createGeneralCohorts = TRUE,
                                            createPulmoCohorts = TRUE,
                                            createDenominator = TRUE,
                                            calculateIncidence = TRUE,
                                            continueOnIncidenceError = TRUE,
                                            minCellCount = 5) {
  
  #------------------------------
  # 0. Prepare result folder, loggers & list cohorts to create
  #------------------------------
  zipName   <- paste0(databaseId, "_ResultsFinal")
  resultsDir <- file.path(outputFolder, zipName)
  if (!file.exists(resultsDir)) {
    dir.create(resultsDir, recursive = TRUE, showWarnings = FALSE)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  
  ParallelLogger::logInfo("Starting CHAPTER executeIncidencePrevalenceFinal (ATLAS-compatible)")
  
  cohortGroupFlags <- c(
    allergy_cohorts = createAllergyCohorts,
    cancer_cohorts  = createCancerCohorts,
    cardio_cohorts  = createCardioCohorts,
    general_cohorts = createGeneralCohorts,
    pulmo_cohorts   = createPulmoCohorts
  )
  
  cohortTablesToCreate <- names(cohortGroupFlags)[cohortGroupFlags]
  
  ParallelLogger::logInfo(
    paste0(
      "Cohort groups requested for creation in this run: ",
      if (length(cohortTablesToCreate) == 0) {
        "<none>"
      } else {
        paste(cohortTablesToCreate, collapse = ", ")
      }
    )
  )
  
  
  #------------------------------
  # 1. Connect to the database
  #------------------------------
  ParallelLogger::logInfo("Connecting to database")
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  
  #------------------------------
  # 2. CDM snapshot (replaces OmopSketch)
  #------------------------------
  ParallelLogger::logInfo("Creating CDM snapshot")
  snapshot <- createCdmSnapshot(
    connection        = connection,
    cdmDatabaseSchema = cdmDatabaseSchema
  )
  
  exportResults(
    x            = snapshot,
    minCellCount = minCellCount,
    fileName     = "snapshot_cdm.csv",
    path         = resultsDir
  )
  
  ParallelLogger::logInfo("CDM snapshot saved")
  
  #------------------------------
  # 3. Instantiate cohorts (via CohortGenerator)
  #------------------------------
  if (readCohorts) {
    ParallelLogger::logInfo("Reading CohortsToCreateFinal.csv")
    cohortsToCreate <- utils::read.csv(
      system.file("settings", "CohortsToCreateFinal.csv", package = "CHAPTER"),
      stringsAsFactors = FALSE
    )
    
    tableNames <- unique(cohortsToCreate$table_name)
    tableNames <- intersect(tableNames, cohortTablesToCreate)
    
    if (length(tableNames) == 0) {
      ParallelLogger::logInfo("No cohort groups selected for creation in this run.")
    }
    
    for (tn in tableNames) {
      ParallelLogger::logInfo(paste0("Instantiating cohorts for table: ", tn))
      
      # Map 'allergy_cohorts' -> 'AllergyCohorts'
      folderName <- sub('^(\\w?)', '\\U\\1', tn, perl = TRUE)
      folderName <- gsub('\\_(\\w?)', '\\U\\1', folderName, perl = TRUE)
      
      # Folder where JSONs live, e.g. inst/cohorts_final/AllergyCohorts
      folderPath <- system.file("cohorts_final", folderName, package = "CHAPTER")
      
      if (folderPath == "") {
        stop("Could not find folder for cohort group: ", folderName)
      }
      
      # Subset settings for this table (allergy_cohorts, cancer_cohorts, ...)
      subsetSettings <- cohortsToCreate[cohortsToCreate$table_name == tn,
                                        c("cohortId", "cohortName", "fileRoot")]
      if (nrow(subsetSettings) == 0) {
        ParallelLogger::logInfo(paste0("  No cohorts found in CohortsToCreateFinal for table: ", tn))
        next
      }
      
      # Write a temporary settings CSV (do NOT write into Program Files)
      settingsTmp <- file.path(
        tempdir(),
        paste0("CohortsToCreate_", tn, ".csv")
      )
      utils::write.csv(subsetSettings, settingsTmp, row.names = FALSE)
      
      ParallelLogger::logInfo(paste0("  Loading cohortDefinitionSet from ", settingsTmp))
      
      # Build cohortDefinitionSet using JSONs like chapter_<cohortName>.json
      cohortDefSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName     = settingsTmp,
        jsonFolder           = folderPath,
        sqlFolder            = folderPath,    
        cohortFileNameFormat = "%s",
        cohortFileNameValue  = c("fileRoot"),
        verbose              = TRUE
      )
      
      # Name of cohort table in DB
      cohortTableName <- if (is.null(writePrefix)) tn else paste0(writePrefix, "_", tn)
      
      cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableName)
      
      # 1) Create cohort tables
      CohortGenerator::createCohortTables(
        connectionDetails    = connectionDetails,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTableNames     = cohortTableNames
      )
      
      # 2) Generate cohorts
      CohortGenerator::generateCohortSet(
        connectionDetails    = connectionDetails,
        cdmDatabaseSchema    = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTableNames     = cohortTableNames,
        cohortDefinitionSet  = cohortDefSet,
        incremental          = FALSE
      )
    }
    
    ParallelLogger::logInfo("All initial cohorts instantiated")
  } else {
    ParallelLogger::logInfo("readCohorts = FALSE, assuming cohorts already exist in cohortDatabaseSchema")
  }
  
  allPossibleCohortTables <- c(
    "allergy_cohorts",
    "cancer_cohorts",
    "cardio_cohorts",
    "general_cohorts",
    "pulmo_cohorts"
  )
  
  possibleCohortTables <- if (is.null(writePrefix)) {
    allPossibleCohortTables
  } else {
    paste0(writePrefix, "_", allPossibleCohortTables)
  }
  
  existingCohortTables <- possibleCohortTables[
    vapply(
      possibleCohortTables,
      function(x) {
        cohortTableExists(
          connection = connection,
          cohortDatabaseSchema = cohortDatabaseSchema,
          cohortTable = x
        )
      },
      logical(1)
    )
  ]
  
  #------------------------------
  # 4. Determine latest data availability
  #   (from observation_period; no CDMConnector)
  #------------------------------
  ParallelLogger::logInfo("Retrieving latest observation_period_end_date")
  latestDataAvailability <- getLatestObservationEndDate(
    connection        = connection,
    cdmDatabaseSchema = cdmDatabaseSchema
  )
  ParallelLogger::logInfo(paste0("Latest data availability: ", latestDataAvailability))
  
  #------------------------------
  # 5. Retrieve incidence analyses settings
  #------------------------------
  analysesToDo <- utils::read.csv(
    system.file("settings", "AnalysesToPerformFinal.csv", package = "CHAPTER"),
    stringsAsFactors = FALSE
  )
  
  # Cohort group names (keep same conceptual grouping as before)
  cohortNames <- c("allergy_cohorts",
                   "cancer_cohorts",
                   "cardio_cohorts",
                   "general_cohorts",
                   "pulmo_cohorts")
  
  # Apply writePrefix if used
  cohortTables <- if (is.null(writePrefix)) {
    cohortNames
  } else {
    paste0(writePrefix, "_", cohortNames)
  }
  
  #------------------------------
  # 6. Run incidence + characteristics analysis
  #------------------------------
  ParallelLogger::logInfo("Starting incidence and characteristics analysis")
  getIncidenceResults(
    connection            = connection,
    cdmDatabaseSchema     = cdmDatabaseSchema,
    cohortDatabaseSchema  = cohortDatabaseSchema,
    cohortTables          = existingCohortTables,
    analyses              = analysesToDo,
    latestDataAvailability= latestDataAvailability,
    resultsDir            = resultsDir,
    createDenominator = createDenominator,
    calculateIncidence = calculateIncidence,
    continueOnIncidenceError = continueOnIncidenceError,
    minCellCount          = minCellCount
  )
  ParallelLogger::logInfo("Incidence and characteristics analysis completed")
  
  #------------------------------
  # 7. Zip all results
  #------------------------------

  ParallelLogger::logInfo("Exporting results as zip")
  
  zipFile <- file.path(outputFolder, paste0("results-", databaseId, ".zip"))
  
  oldWd <- getwd()
  setwd(resultsDir)
  on.exit(setwd(oldWd), add = TRUE)
  
  filesToZip <- list.files(".", recursive = TRUE, full.names = TRUE)
  
  # Optional: if something is still weird, this tryCatch will let the
  # analysis finish and just warn about the zip.
  tryCatch(
    {
      zip::zip(
        zipfile = zipFile,
        files   = filesToZip
      )
      ParallelLogger::logInfo(paste0("Saved all results to ", zipFile))
    },
    error = function(e) {
      ParallelLogger::logError(paste0("Zipping results failed: ", e$message))
      message("Zipping results failed, but CSV files are in: ", resultsDir)
    }
  )
  
  
  ParallelLogger::logInfo("Saved all results")
  message("Done! A zip file with your results should now be in the output folder.")
}
