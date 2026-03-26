#' Get the incidence results of the CHAPTER study (ATLAS-compatible)
#'
#' @details
#' This function performs the incidence calculations and basic cohort
#' characterisation using only SQL + DatabaseConnector, without CDMConnector
#' or IncidencePrevalence.
#'
#' @param connection            A connection object from DatabaseConnector::connect()
#' @param cdmDatabaseSchema     CDM schema name
#' @param cohortDatabaseSchema  Cohort schema name
#' @param cohortTables          Character vector of cohort table names (per group)
#' @param analyses              Tibble/data.frame specifying incidence analyses
#' @param latestDataAvailability Date of latest data availability (Date)
#' @param resultsDir            Directory to save results
#' @param minCellCount          Minimum allowed count to report
#'
#' @importFrom dplyr %>%
#' @export
getIncidenceResults <- function(connection,
                                cdmDatabaseSchema,
                                cohortDatabaseSchema,
                                cohortTables,
                                analyses,
                                latestDataAvailability,
                                resultsDir,
                                createDenominator = TRUE,
                                calculateIncidence = TRUE,
                                continueOnIncidenceError = TRUE,
                                minCellCount) {
  
  
  
  if (calculateIncidence && !createDenominator) {
    stop("calculateIncidence = TRUE requires createDenominator = TRUE.")
  }
  
  #--------------------------------------------
  # 1. Cohort characteristics (Table 1-ish)
  #--------------------------------------------
  ParallelLogger::logInfo("- Getting cohort characteristics")
  
  characteristics_list <- list()
  
  if (length(cohortTables) > 0) {
    for (tn in cohortTables) {
      ParallelLogger::logInfo(paste0("  * Summarising characteristics for table: ", tn))
      
      characteristics_list[[tn]] <- tryCatch(
        {
          summariseCohortCharacteristicsSql(
            connection           = connection,
            cdmDatabaseSchema    = cdmDatabaseSchema,
            cohortDatabaseSchema = cohortDatabaseSchema,
            cohortTable          = tn
          )
        },
        error = function(e) {
          ParallelLogger::logWarn(
            paste0(
              "[CHARACTERISTICS_FAILED] table=", tn,
              "; message=", conditionMessage(e)
            )
          )
          dplyr::tibble()
        }
      )
    }
    
    characteristics <- dplyr::bind_rows(characteristics_list)
  } else {
    ParallelLogger::logInfo("No cohort tables provided for characteristics.")
    characteristics <- dplyr::tibble()
  }
  
  #--------------------------------------------
  # 2. Create denominator cohort table
  #--------------------------------------------
  denomTableName <- "CHAPTER_denominator"
  characteristics_denom <- dplyr::tibble()
  
  if (createDenominator) {
    ParallelLogger::logInfo("- Creating denominator cohort")
    
    createDenominatorCohort(
      connection            = connection,
      cdmDatabaseSchema     = cdmDatabaseSchema,
      cohortDatabaseSchema  = cohortDatabaseSchema,
      denominatorTable      = denomTableName,
      startDate             = as.Date("2018-01-01"),
      endDate               = latestDataAvailability,
      minPriorObservation   = 365
    )
    
    # characteristics of denominator
    ParallelLogger::logInfo("- Summarising denominator characteristics")
    characteristics_denom <- summariseCohortCharacteristicsSql(
      connection           = connection,
      cdmDatabaseSchema    = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable          = denomTableName
    )
  } else {
    ParallelLogger::logInfo("Skipping denominator creation")
  }
  
  #--------------------------------------------
  # 3. Incidence for all diagnoses in the population
  #--------------------------------------------
  incidence <- dplyr::tibble()
  failed_incidence <- list()
  
  if (calculateIncidence) {
    ParallelLogger::logInfo("- Estimating incidence")
    
    # Filter analyses that use "all_population" denominator
    outcomesAllPop <- analyses %>%
      dplyr::filter(.data$denominator_table_name == "all_population") %>%
      dplyr::select(.data$outcome_id, .data$outcome_table_name)
    
    incidence_list <- list()
    idx <- 1L
    
    for (tn in unique(outcomesAllPop$outcome_table_name)) {
      ParallelLogger::logInfo(paste0("  * Estimating incidence for outcome table: ", tn))
      
      # outcome_id in AnalysesToPerformFinal.csv corresponds to cohort_definition_id
      idsForTable <- outcomesAllPop %>%
        dplyr::filter(.data$outcome_table_name == tn) %>%
        dplyr::pull(.data$outcome_id) %>%
        unique()
      
      for (oid in idsForTable) {
        ParallelLogger::logInfo(paste0("    - outcome cohort id: ", oid))
        
        one_result <- tryCatch(
          {
            tmp <- estimateIncidenceSql(
              connection            = connection,
              cdmDatabaseSchema     = cdmDatabaseSchema,
              cohortDatabaseSchema  = cohortDatabaseSchema,
              denominatorTable      = denomTableName,
              outcomeTable          = tn,
              outcomeIds            = oid,   # single ID per call
              minCellCount          = minCellCount
            )
            
            list(
              success = TRUE,
              result  = tmp,
              error   = NA_character_
            )
          },
          error = function(e) {
            list(
              success = FALSE,
              result  = NULL,
              error   = conditionMessage(e)
            )
          }
        )
        
        if (isTRUE(one_result$success)) {
          incidence_list[[idx]] <- one_result$result
          idx <- idx + 1L
        } else {
          ParallelLogger::logWarn(
            paste0(
              "[INCIDENCE_FAILED] table=", tn,
              "; outcome_id=", oid,
              "; message=", one_result$error
            )
          )
          
          failed_incidence[[length(failed_incidence) + 1L]] <- dplyr::tibble(
            outcome_table_name = tn,
            outcome_id         = oid,
            error_message      = one_result$error
          )
          
          if (!continueOnIncidenceError) {
            stop(
              paste0(
                "Incidence failed for table=", tn,
                ", outcome_id=", oid,
                ". Error: ", one_result$error
              )
            )
          }
        }
      }
    }
    
    if (length(incidence_list) > 0) {
      incidence <- dplyr::bind_rows(incidence_list)
    } else {
      incidence <- dplyr::tibble()
    }
  } else {
    ParallelLogger::logInfo("- Skipping incidence estimation")
  }
  
  failedIncidence <- if (length(failed_incidence) > 0) {
    dplyr::bind_rows(failed_incidence)
  } else {
    dplyr::tibble(
      outcome_table_name = character(),
      outcome_id         = numeric(),
      error_message      = character()
    )
  }
  
  readr::write_csv(
    failedIncidence,
    file.path(resultsDir, "failed_incidence_log.csv")
  )
  
  # --- Normalise column names for joins ---
  # Make all column names lower-case to avoid case/camelCase issues
  
  summarised_incidence <- dplyr::tibble()
  
  if (nrow(incidence) > 0) {
    # --- Normalise column names for joins ---
    names(incidence) <- tolower(names(incidence))
    
    if (!"outcome_id" %in% names(incidence)) {
      if ("cohort_definition_id" %in% names(incidence)) {
        incidence <- dplyr::rename(incidence, outcome_id = cohort_definition_id)
      } else if ("outcomeid" %in% names(incidence)) {
        incidence <- dplyr::rename(incidence, outcome_id = outcomeid)
      } else {
        stop(
          "Could not find an outcome_id-like column in incidence. Available columns: ",
          paste(names(incidence), collapse = ", ")
        )
      }
    }
    
    cohortsToCreate <- utils::read.csv(
      system.file("settings", "CohortsToCreateFinal.csv", package = "CHAPTER"),
      stringsAsFactors = FALSE
    )
    
    names(cohortsToCreate) <- tolower(names(cohortsToCreate))
    
    cohortIdCol   <- grep("^cohortid$",   names(cohortsToCreate), ignore.case = TRUE, value = TRUE)[1]
    cohortNameCol <- grep("^cohortname$", names(cohortsToCreate), ignore.case = TRUE, value = TRUE)[1]
    tableNameCol  <- grep("^table_name$", names(cohortsToCreate), ignore.case = TRUE, value = TRUE)[1]
    
    if (is.na(cohortIdCol) || is.na(cohortNameCol) || is.na(tableNameCol)) {
      stop(
        "Could not find cohortId/cohortName/table_name columns in CohortsToCreateFinal.csv.\n",
        "Columns present: ", paste(names(cohortsToCreate), collapse = ", ")
      )
    }
    
    outcomeLabels <- cohortsToCreate %>%
      dplyr::select(
        cohort_id   = dplyr::all_of(cohortIdCol),
        cohort_name = dplyr::all_of(cohortNameCol),
        table_name  = dplyr::all_of(tableNameCol)
      )
    
    names(incidence) <- tolower(names(incidence))
    if (!"outcome_id" %in% names(incidence)) {
      if ("cohort_definition_id" %in% names(incidence)) {
        incidence <- dplyr::rename(incidence, outcome_id = cohort_definition_id)
      } else if ("outcomeid" %in% names(incidence)) {
        incidence <- dplyr::rename(incidence, outcome_id = outcomeid)
      } else {
        stop("Could not find an outcome_id-like column in incidence. Columns: ",
             paste(names(incidence), collapse = ", "))
      }
    }
    if (!"outcome_table" %in% names(incidence)) {
      stop("Expected 'outcome_table' column in incidence, but it is missing.")
    }
    
    incidence <- incidence %>%
      dplyr::left_join(
        outcomeLabels,
        by = c("outcome_id" = "cohort_id", "outcome_table" = "table_name")
      )
    
    if ("cohort_name" %in% names(incidence)) {
      cohName <- incidence$cohort_name
      incidence$outcome_name <- ifelse(
        !is.na(cohName) & cohName != "",
        cohName,
        paste0(incidence$outcome_table, "_", incidence$outcome_id)
      )
    } else {
      incidence$outcome_name <- paste0(incidence$outcome_table, "_", incidence$outcome_id)
    }
    
    if (!"denominator_cohort" %in% names(incidence)) {
      denomCol <- if ("denom_id" %in% names(incidence)) "denom_id" else if ("DENOM_ID" %in% names(incidence)) "DENOM_ID" else NULL
      if (!is.null(denomCol)) {
        incidence$denominator_cohort <- paste0("denominator_cohort_", incidence[[denomCol]])
      } else {
        incidence$denominator_cohort <- NA_character_
      }
    }
    
    summarised_incidence <- formatIncidenceToSummarisedResult(
      incidence    = incidence,
      startDate    = as.Date("2018-01-01"),
      endDate      = latestDataAvailability,
      minCellCount = minCellCount
    )
    
    readr::write_csv(
      summarised_incidence,
      file.path(resultsDir, "main_incidence_results.csv")
    )
  } else {
    ParallelLogger::logInfo("No successful incidence results to format.")
  }
  
  
  
  #--------------------------------------------
  # 4. Combine and export results
  #--------------------------------------------
  final_parts <- list(
    characteristics       = characteristics,
    characteristics_denom = characteristics_denom,
    incidence             = incidence
  )
  
  final_parts <- final_parts[vapply(final_parts, nrow, integer(1)) > 0]
  
  if (length(final_parts) > 0) {
    final_results <- dplyr::bind_rows(final_parts)
  } else {
    final_results <- dplyr::tibble()
  }
  
  exportResults(
    x            = final_results,
    minCellCount = minCellCount,
    fileName     = "incidence_and_characteristics.csv",
    path         = resultsDir
  )
  
  ParallelLogger::logInfo("Incidence results calculated and exported")
}