# helper functions

.getDbms <- function(connection) {
  dbms <- attr(connection, "dbms")
  if (is.null(dbms) || is.na(dbms) || dbms == "") {
    stop("Connection attribute 'dbms' is missing. DatabaseConnector must set attr(connection, 'dbms').")
  }
  tolower(dbms)
}

.isDatabricks <- function(dbms) {
  dbms <- tolower(dbms)
  dbms %in% c("databricks", "spark", "spark sql", "spark_sql")
}

.getColumn <- function(x, columnName) {
  idx <- match(tolower(columnName), tolower(names(x)))
  if (is.na(idx)) {
    stop("Column '", columnName, "' not found. Available columns: ", paste(names(x), collapse = ", "))
  }
  x[[idx]]
}

.splitSchemaName <- function(schema) {
  parts <- strsplit(schema, "\\.")[[1]]
  if (length(parts) == 1) {
    list(catalog = NULL, schema = parts[1])
  } else {
    list(
      catalog = paste(parts[-length(parts)], collapse = "."),
      schema = parts[length(parts)]
    )
  }
}



getCohortCount <- function(cohort) {
  cohort %>%
    group_by(cohort_definition_id) %>%
    summarise(
      number_records = dplyr::n() %>% as.integer(),
      number_subjects = dplyr::n_distinct(.data$subject_id) %>% as.integer(),
      .groups = "drop"
    ) %>%
    collect() %>%
    dplyr::right_join(attr(cohort, "cohort_set") %>% dplyr::select("cohort_definition_id") %>%
                        dplyr::collect(),
                     by = "cohort_definition_id") %>%
    dplyr::mutate(number_records = dplyr::if_else(is.na(number_records), 0, number_records),
                  number_subjects = dplyr::if_else(is.na(number_subjects), 0, number_subjects)) %>%
    arrange(cohort_definition_id) %>%
    dplyr::distinct()
}


cohortTableExists <- function(connection, cohortDatabaseSchema, cohortTable) {
  dbms <- .getDbms(connection)
  
  if (dbms == "oracle") {
    schemaParts <- strsplit(cohortDatabaseSchema, "\\.")[[1]]
    owner <- toupper(schemaParts[length(schemaParts)])
    
    sql <- "
      SELECT COUNT(*) AS n
      FROM all_tables
      WHERE owner = '@owner'
        AND table_name = '@tableName'
    "
    
    sql <- SqlRender::render(
      sql,
      owner = owner,
      tableName = toupper(cohortTable)
    )
  } else if (.isDatabricks(dbms)) {
    schemaParts <- .splitSchemaName(cohortDatabaseSchema)
    informationSchema <- if (is.null(schemaParts$catalog)) {
      "information_schema.tables"
    } else {
      paste0(schemaParts$catalog, ".information_schema.tables")
    }
    
    sql <- "
      SELECT COUNT(*) AS n
      FROM @informationSchema
      WHERE lower(table_schema) = lower('@schemaName')
        AND lower(table_name) = lower('@tableName')
    "
    
    sql <- SqlRender::render(
      sql,
      informationSchema = informationSchema,
      schemaName = schemaParts$schema,
      tableName = cohortTable
    )
  } else {
    stop("cohortTableExists(): unsupported dbms = ", dbms)
  }

  if (!.isDatabricks(dbms)) {
    sql <- SqlRender::translate(sql, targetDialect = attr(connection, 'dbms'))
  }
  
  res <- DatabaseConnector::querySql(connection, sql)
  as.numeric(res[[1]][1]) > 0
}


# HelperFunctions.R
# Internal utility functions for ATLAS-compatible CHAPTER package
# These replace CDMConnector, IncidencePrevalence, OmopSketch, omopgenerics,
# CohortCharacteristics, and similar functionality.

# -------------------------------------------------------------------------
# 1. CDM Snapshot ----------------------------------------------------------
# -------------------------------------------------------------------------

#' @noRd
#' @keywords internal
# -------------------------------------------------------------------------
# Improved CDM snapshot (OmopSketch-like)
# -------------------------------------------------------------------------

createCdmSnapshot <- function(connection, cdmDatabaseSchema) {
  ParallelLogger::logInfo("Running improved CDM snapshot query")
  
  # Snapshot date
  snapshotDate <- Sys.Date()
  
  #-----------------------------
  # 1. Person count
  #-----------------------------
  sqlPerson <- "
    SELECT COUNT(*) AS person_count
    FROM @cdm.person;
  "
  sqlPerson <- SqlRender::render(sqlPerson, cdm = cdmDatabaseSchema)
  sqlPerson <- SqlRender::translate(sqlPerson, targetDialect = attr(connection, "dbms"))
  personRes <- DatabaseConnector::querySql(connection, sqlPerson)
  personCount <- as.character(personRes[[1]][1])
  
  #-----------------------------
  # 2. Observation period summary
  #-----------------------------
  sqlObs <- "
    SELECT
      COUNT(*)                           AS op_count,
      MIN(observation_period_start_date) AS op_start_date,
      MAX(observation_period_end_date)   AS op_end_date
    FROM @cdm.observation_period;
  "
  sqlObs <- SqlRender::render(sqlObs, cdm = cdmDatabaseSchema)
  sqlObs <- SqlRender::translate(sqlObs, targetDialect = attr(connection, "dbms"))
  obsRes <- DatabaseConnector::querySql(connection, sqlObs)
  
  opCount  <- as.character(.getColumn(obsRes, "op_count")[1])
  opStart  <- as.character(as.Date(.getColumn(obsRes, "op_start_date")[1]))
  opEnd    <- as.character(as.Date(.getColumn(obsRes, "op_end_date")[1]))
  
  #-----------------------------
  # 3. CDM source metadata
  #-----------------------------
  sqlCdmSource <- "
    SELECT
      cdm_source_name,
      cdm_version,
      cdm_holder,
      cdm_release_date,
      source_description,
      source_documentation_reference,
      vocabulary_version
    FROM @cdm.cdm_source;
  "
  sqlCdmSource <- SqlRender::render(sqlCdmSource, cdm = cdmDatabaseSchema)
  sqlCdmSource <- SqlRender::translate(sqlCdmSource, targetDialect = attr(connection, "dbms"))
  
  cdmSrc <- tryCatch(
    DatabaseConnector::querySql(connection, sqlCdmSource),
    error = function(e) {
      ParallelLogger::logWarn(paste0("cdm_source not available: ", e$message))
      NULL
    }
  )
  
  if (!is.null(cdmSrc) && nrow(cdmSrc) > 0) {
    row1 <- cdmSrc[1, ]
    sourceName    <- as.character(.getColumn(row1, "cdm_source_name"))
    cdmVersion    <- as.character(.getColumn(row1, "cdm_version"))
    holderName    <- as.character(.getColumn(row1, "cdm_holder"))
    releaseDateRaw <- .getColumn(row1, "cdm_release_date")
    releaseDate   <- if (!is.null(releaseDateRaw)) as.character(as.Date(releaseDateRaw)) else NA_character_
    description   <- as.character(.getColumn(row1, "source_description"))
    docRef        <- as.character(.getColumn(row1, "source_documentation_reference"))
    vocabVersion  <- as.character(.getColumn(row1, "vocabulary_version"))
  } else {
    sourceName   <- NA_character_
    cdmVersion   <- NA_character_
    holderName   <- NA_character_
    releaseDate  <- NA_character_
    description  <- NA_character_
    docRef       <- NA_character_
    vocabVersion <- NA_character_
  }
  
  # If vocabulary_version missing in cdm_source, fall back to vocabulary table
  if (is.na(vocabVersion) || vocabVersion == "") {
    sqlVocab <- "
    SELECT MAX(vocabulary_version) AS vocabulary_version
    FROM @cdm.vocabulary;
  "
    sqlVocab <- SqlRender::render(sqlVocab, cdm = cdmDatabaseSchema)
    sqlVocab <- SqlRender::translate(sqlVocab, targetDialect = attr(connection, "dbms"))
    vocabRes <- tryCatch(DatabaseConnector::querySql(connection, sqlVocab),
                         error = function(e) NULL)
    if (!is.null(vocabRes) && nrow(vocabRes) > 0) {
      vocabVersion <- as.character(vocabRes[[1]][1])
    }
  }
  
  
  #-----------------------------
  # 4. cdm_source object meta (type, package, etc.)
  #-----------------------------
  dbmsType <- attr(connection, "dbms")
  if (is.null(dbmsType)) dbmsType <- "unknown"
  
  # We mimic what OmopSketch reports:
  cdmSourceType   <- tolower(dbmsType)   # e.g. "sql server"
  cdmSourcePkg    <- "DatabaseConnector" # or "Unknown"
  cdmSourceLength <- "0"
  cdmSourceClass1 <- "cdm_source"
  cdmSourceClass2 <- "db_cdm"
  cdmSourceMode   <- "list"
  
  #-----------------------------
  # 5. Build snapshot in summarisedResult-like format
  #-----------------------------
  make_row <- function(variable_name, variable_level,
                       estimate_name, estimate_type, estimate_value) {
    dplyr::tibble(
      result_id       = 1L,
      cdm_name        = NA_character_,
      group_name      = "overall",
      group_level     = "overall",
      strata_name     = "overall",
      strata_level    = "overall",
      variable_name   = variable_name,
      variable_level  = variable_level,
      estimate_name   = estimate_name,
      estimate_type   = estimate_type,
      estimate_value  = as.character(estimate_value),
      additional_name = "overall",
      additional_level= "overall"
    )
  }
  
  # general
  general_rows <- dplyr::bind_rows(
    make_row("general", NA_character_, "snapshot_date",      "date",      snapshotDate),
    make_row("general", NA_character_, "person_count",       "integer",   personCount),
    make_row("general", NA_character_, "vocabulary_version", "character", vocabVersion)
  )
  
  # cdm (from cdm_source)
  cdm_rows <- dplyr::bind_rows(
    make_row("cdm", NA_character_, "source_name",             "character", sourceName),
    make_row("cdm", NA_character_, "version",                 "character", cdmVersion),
    make_row("cdm", NA_character_, "holder_name",             "character", holderName),
    make_row("cdm", NA_character_, "release_date",            "character", releaseDate),
    make_row("cdm", NA_character_, "description",             "character", description),
    make_row("cdm", NA_character_, "documentation_reference", "character", docRef)
  )
  
  # observation_period
  obs_rows <- dplyr::bind_rows(
    make_row("observation_period", NA_character_, "count",      "integer", opCount),
    make_row("observation_period", NA_character_, "start_date", "date",    opStart),
    make_row("observation_period", NA_character_, "end_date",   "date",    opEnd)
  )
  
  # cdm_source object meta
  cdm_source_rows <- dplyr::bind_rows(
    make_row("cdm_source", NA_character_, "type",   "character", cdmSourceType),
    make_row("cdm_source", NA_character_, "package","character", cdmSourcePkg),
    make_row("cdm_source", NA_character_, "Length", "character", cdmSourceLength),
    make_row("cdm_source", NA_character_, "Class1", "character", cdmSourceClass1),
    make_row("cdm_source", NA_character_, "Class2", "character", cdmSourceClass2),
    make_row("cdm_source", NA_character_, "Mode",   "character", cdmSourceMode)
  )
  
  # settings block (simple version for snapshot)
  settings_rows <- dplyr::tibble(
    result_id       = 1L,
    cdm_name        = NA_character_,
    group_name      = "overall",
    group_level     = "overall",
    strata_name     = "overall",
    strata_level    = "overall",
    variable_name   = "settings",
    variable_level  = NA_character_,
    estimate_name   = c(
      "result_type",
      "package_name",
      "package_version",
      "group",
      "strata",
      "additional"
    ),
    estimate_type   = "character",
    estimate_value  = c(
      "summarise_omop_snapshot_sql",
      "CHAPTER_ATLAS_SQL",
      as.character(utils::packageVersion("CHAPTER")),
      NA_character_,
      NA_character_,
      NA_character_
    ),
    additional_name  = "overall",
    additional_level = "overall"
  )
  
  snapshot <- dplyr::bind_rows(
    general_rows,
    cdm_rows,
    obs_rows,
    cdm_source_rows,
    settings_rows
  )
  
  snapshot
}


# -------------------------------------------------------------------------
# 2. Latest observation end date ------------------------------------------
# -------------------------------------------------------------------------

#' @noRd
#' @keywords internal
getLatestObservationEndDate <- function(connection, cdmDatabaseSchema) {
  ParallelLogger::logInfo("Querying latest observation_period_end_date")
  
  sql <- "
    SELECT MAX(observation_period_end_date) AS max_end_date
    FROM @cdm.observation_period;
  "
  
  sql <- SqlRender::render(sql, cdm = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  
  # Let DatabaseConnector do its usual thing; don't rely on column name
  res <- DatabaseConnector::querySql(connection, sql)
  
  if (nrow(res) == 0 || all(is.na(res[[1]]))) {
    stop("No observation_period_end_date found in ", cdmDatabaseSchema,
         ". Check that observation_period has data.")
  }
  
  latestRaw <- res[[1]][1]  # first column, first row
  
  # Try direct Date conversion
  latestDate <- suppressWarnings(as.Date(latestRaw))
  # If there is a time component (e.g. '2023-12-31 00:00:00'), strip to first 10 chars
  if (is.na(latestDate)) {
    latestDate <- as.Date(substr(as.character(latestRaw), 1, 10))
  }
  
  ParallelLogger::logInfo(paste0("Latest observation_period_end_date: ", latestDate))
  latestDate
}



# -------------------------------------------------------------------------
# 3. Cohort Characteristics (legacy-level descriptive output)
# -------------------------------------------------------------------------

#' @noRd
#' @keywords internal
summariseCohortCharacteristicsSql <- function(connection,
                                              cdmDatabaseSchema,
                                              cohortDatabaseSchema,
                                              cohortTable) {
  
  ParallelLogger::logInfo(paste0("Summarising characteristics for ", cohortTable))
  
  dbms <- .getDbms(connection)
  
  if (dbms == "oracle") {
    sql <- getCohortCharacteristicsSql_oracle()
  } else if (.isDatabricks(dbms)) {
    sql <- getCohortCharacteristicsSql_databricks()
  } else {
    stop("summariseCohortCharacteristicsSql(): unsupported dbms = ", dbms)
  }
  
  sql <- SqlRender::render(
    sql,
    cdm          = cdmDatabaseSchema,
    cohortSchema = cohortDatabaseSchema,
    cohortTable  = cohortTable
  )
  
  if (!.isDatabricks(dbms)) {
    sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  }
  
  res <- DatabaseConnector::querySql(connection, sql)
  res <- dplyr::as_tibble(res)
  
  if (nrow(res) == 0) {
    ParallelLogger::logInfo(
      paste0("summariseCohortCharacteristicsSql: 0 rows for ", cohortTable)
    )
    return(res)
  }
  
  # Track origin cohort table (useful for debugging)
  res$cohort_table <- cohortTable
  
  res
}


# -------------------------------------------------------------------------
# Oracle SQL template for legacy-style cohort characteristics
# -------------------------------------------------------------------------
getCohortCharacteristicsSql_oracle <- function() {
  "
WITH cohort AS (
  SELECT
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date
  FROM @cohortSchema.@cohortTable
),
p AS (
  SELECT
    person_id,
    gender_concept_id,
    year_of_birth
  FROM @cdm.person
),
op AS (
  SELECT
    person_id,
    observation_period_start_date,
    observation_period_end_date
  FROM @cdm.observation_period
),
base AS (
  SELECT
    c.cohort_definition_id,
    c.subject_id,
    c.cohort_start_date,
    c.cohort_end_date,
    (c.cohort_end_date - c.cohort_start_date + 1) AS days_in_cohort,
    (EXTRACT(YEAR FROM c.cohort_start_date) - p.year_of_birth) AS age_years,
    CASE
      WHEN p.gender_concept_id = 8532 THEN 'Female'
      WHEN p.gender_concept_id = 8507 THEN 'Male'
      ELSE 'Other/Unknown'
    END AS sex,
    (c.cohort_start_date - o.observation_period_start_date) AS prior_observation,
    (o.observation_period_end_date - c.cohort_start_date) AS future_observation
  FROM cohort c
  JOIN p
    ON p.person_id = c.subject_id
  LEFT JOIN op o
    ON o.person_id = c.subject_id
   AND c.cohort_start_date BETWEEN o.observation_period_start_date AND o.observation_period_end_date
),
subj AS (
  -- Subject-level de-duplication for sex/age group distributions
  SELECT DISTINCT
    cohort_definition_id,
    subject_id,
    age_years,
    sex
  FROM base
),
agg AS (
  SELECT
    cohort_definition_id,
    COUNT(*) AS n_records,
    COUNT(DISTINCT subject_id) AS n_subjects,

    MIN(cohort_start_date) AS cs_min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY cohort_start_date) AS cs_q25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY cohort_start_date) AS cs_median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY cohort_start_date) AS cs_q75,
    MAX(cohort_start_date) AS cs_max,

    MIN(cohort_end_date) AS ce_min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY cohort_end_date) AS ce_q25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY cohort_end_date) AS ce_median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY cohort_end_date) AS ce_q75,
    MAX(cohort_end_date) AS ce_max,

    MIN(age_years) AS age_min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY age_years) AS age_q25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY age_years) AS age_median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY age_years) AS age_q75,
    MAX(age_years) AS age_max,
    AVG(age_years) AS age_mean,
    STDDEV(age_years) AS age_sd,

    MIN(prior_observation) AS prior_min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY prior_observation) AS prior_q25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY prior_observation) AS prior_median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY prior_observation) AS prior_q75,
    MAX(prior_observation) AS prior_max,
    AVG(prior_observation) AS prior_mean,
    STDDEV(prior_observation) AS prior_sd,

    MIN(future_observation) AS fut_min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY future_observation) AS fut_q25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY future_observation) AS fut_median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY future_observation) AS fut_q75,
    MAX(future_observation) AS fut_max,
    AVG(future_observation) AS fut_mean,
    STDDEV(future_observation) AS fut_sd,

    MIN(days_in_cohort) AS dic_min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY days_in_cohort) AS dic_q25,
    PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY days_in_cohort) AS dic_median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY days_in_cohort) AS dic_q75,
    MAX(days_in_cohort) AS dic_max,
    AVG(days_in_cohort) AS dic_mean,
    STDDEV(days_in_cohort) AS dic_sd
  FROM base
  GROUP BY cohort_definition_id
),
sex_counts AS (
  SELECT
    cohort_definition_id,
    SUM(CASE WHEN sex = 'Female' THEN 1 ELSE 0 END) AS n_female,
    SUM(CASE WHEN sex = 'Male' THEN 1 ELSE 0 END) AS n_male
  FROM subj
  GROUP BY cohort_definition_id
),
age_counts AS (
  SELECT
    cohort_definition_id,
    SUM(CASE WHEN age_years BETWEEN 0 AND 19 THEN 1 ELSE 0 END) AS n_0_19,
    SUM(CASE WHEN age_years BETWEEN 20 AND 39 THEN 1 ELSE 0 END) AS n_20_39,
    SUM(CASE WHEN age_years BETWEEN 40 AND 59 THEN 1 ELSE 0 END) AS n_40_59,
    SUM(CASE WHEN age_years BETWEEN 60 AND 79 THEN 1 ELSE 0 END) AS n_60_79,
    SUM(CASE WHEN age_years BETWEEN 80 AND 150 THEN 1 ELSE 0 END) AS n_80_150
  FROM subj
  GROUP BY cohort_definition_id
),
long_rows AS (
  -- Every SELECT explicitly aliases the 6 long columns:
  -- (cohort_definition_id, variable_name, variable_level, estimate_name, estimate_type, estimate_value)

  SELECT
    a.cohort_definition_id AS cohort_definition_id,
    'Number records'       AS variable_name,
    'NA'                   AS variable_level,
    'count'                AS estimate_name,
    'integer'              AS estimate_type,
    TO_CHAR(a.n_records)   AS estimate_value
  FROM agg a

  UNION ALL
  SELECT a.cohort_definition_id, 'Number subjects', 'NA', 'count', 'integer', TO_CHAR(a.n_subjects)
  FROM agg a

  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'min',    'date', TO_CHAR(a.cs_min,    'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'q25',    'date', TO_CHAR(a.cs_q25,    'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'median', 'date', TO_CHAR(a.cs_median, 'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'q75',    'date', TO_CHAR(a.cs_q75,    'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'max',    'date', TO_CHAR(a.cs_max,    'YYYY-MM-DD') FROM agg a

  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'min',    'date', TO_CHAR(a.ce_min,    'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'q25',    'date', TO_CHAR(a.ce_q25,    'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'median', 'date', TO_CHAR(a.ce_median, 'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'q75',    'date', TO_CHAR(a.ce_q75,    'YYYY-MM-DD') FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'max',    'date', TO_CHAR(a.ce_max,    'YYYY-MM-DD') FROM agg a

  UNION ALL
  SELECT a.cohort_definition_id, 'Age', 'NA', 'min',    'integer', TO_CHAR(a.age_min)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Age', 'NA', 'q25',    'integer', TO_CHAR(a.age_q25)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Age', 'NA', 'median', 'integer', TO_CHAR(a.age_median) FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Age', 'NA', 'q75',    'integer', TO_CHAR(a.age_q75)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Age', 'NA', 'max',    'integer', TO_CHAR(a.age_max)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Age', 'NA', 'mean',   'numeric', TO_CHAR(a.age_mean)   FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Age', 'NA', 'sd',     'numeric', TO_CHAR(a.age_sd)     FROM agg a

  UNION ALL
  SELECT s.cohort_definition_id, 'Sex', 'Female', 'count', 'integer', TO_CHAR(s.n_female)
  FROM sex_counts s
  UNION ALL
  SELECT s.cohort_definition_id, 'Sex', 'Female', 'percentage', 'percentage',
         TO_CHAR(CASE WHEN a.n_subjects = 0 THEN 0 ELSE (s.n_female * 100.0 / a.n_subjects) END)
  FROM sex_counts s JOIN agg a ON a.cohort_definition_id = s.cohort_definition_id

  UNION ALL
  SELECT s.cohort_definition_id, 'Sex', 'Male', 'count', 'integer', TO_CHAR(s.n_male)
  FROM sex_counts s
  UNION ALL
  SELECT s.cohort_definition_id, 'Sex', 'Male', 'percentage', 'percentage',
         TO_CHAR(CASE WHEN a.n_subjects = 0 THEN 0 ELSE (s.n_male * 100.0 / a.n_subjects) END)
  FROM sex_counts s JOIN agg a ON a.cohort_definition_id = s.cohort_definition_id

  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '0 to 19', 'count', 'integer', TO_CHAR(ac.n_0_19)
  FROM age_counts ac
  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '0 to 19', 'percentage', 'percentage',
         TO_CHAR(CASE WHEN a.n_subjects = 0 THEN 0 ELSE (ac.n_0_19 * 100.0 / a.n_subjects) END)
  FROM age_counts ac JOIN agg a ON a.cohort_definition_id = ac.cohort_definition_id

  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '20 to 39', 'count', 'integer', TO_CHAR(ac.n_20_39)
  FROM age_counts ac
  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '20 to 39', 'percentage', 'percentage',
         TO_CHAR(CASE WHEN a.n_subjects = 0 THEN 0 ELSE (ac.n_20_39 * 100.0 / a.n_subjects) END)
  FROM age_counts ac JOIN agg a ON a.cohort_definition_id = ac.cohort_definition_id

  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '40 to 59', 'count', 'integer', TO_CHAR(ac.n_40_59)
  FROM age_counts ac
  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '40 to 59', 'percentage', 'percentage',
         TO_CHAR(CASE WHEN a.n_subjects = 0 THEN 0 ELSE (ac.n_40_59 * 100.0 / a.n_subjects) END)
  FROM age_counts ac JOIN agg a ON a.cohort_definition_id = ac.cohort_definition_id

  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '60 to 79', 'count', 'integer', TO_CHAR(ac.n_60_79)
  FROM age_counts ac
  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '60 to 79', 'percentage', 'percentage',
         TO_CHAR(CASE WHEN a.n_subjects = 0 THEN 0 ELSE (ac.n_60_79 * 100.0 / a.n_subjects) END)
  FROM age_counts ac JOIN agg a ON a.cohort_definition_id = ac.cohort_definition_id

  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '80 to 150', 'count', 'integer', TO_CHAR(ac.n_80_150)
  FROM age_counts ac
  UNION ALL
  SELECT ac.cohort_definition_id, 'Age group', '80 to 150', 'percentage', 'percentage',
         TO_CHAR(CASE WHEN a.n_subjects = 0 THEN 0 ELSE (ac.n_80_150 * 100.0 / a.n_subjects) END)
  FROM age_counts ac JOIN agg a ON a.cohort_definition_id = ac.cohort_definition_id

  UNION ALL
  SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'min',    'integer', TO_CHAR(a.prior_min)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'q25',    'integer', TO_CHAR(a.prior_q25)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'median', 'integer', TO_CHAR(a.prior_median) FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'q75',    'integer', TO_CHAR(a.prior_q75)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'max',    'integer', TO_CHAR(a.prior_max)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'mean',   'numeric', TO_CHAR(a.prior_mean)   FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'sd',     'numeric', TO_CHAR(a.prior_sd)     FROM agg a

  UNION ALL
  SELECT a.cohort_definition_id, 'Future observation', 'NA', 'min',    'integer', TO_CHAR(a.fut_min)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Future observation', 'NA', 'q25',    'integer', TO_CHAR(a.fut_q25)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Future observation', 'NA', 'median', 'integer', TO_CHAR(a.fut_median) FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Future observation', 'NA', 'q75',    'integer', TO_CHAR(a.fut_q75)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Future observation', 'NA', 'max',    'integer', TO_CHAR(a.fut_max)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Future observation', 'NA', 'mean',   'numeric', TO_CHAR(a.fut_mean)   FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Future observation', 'NA', 'sd',     'numeric', TO_CHAR(a.fut_sd)     FROM agg a

  UNION ALL
  SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'min',    'integer', TO_CHAR(a.dic_min)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'q25',    'integer', TO_CHAR(a.dic_q25)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'median', 'integer', TO_CHAR(a.dic_median) FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'q75',    'integer', TO_CHAR(a.dic_q75)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'max',    'integer', TO_CHAR(a.dic_max)    FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'mean',   'numeric', TO_CHAR(a.dic_mean)   FROM agg a
  UNION ALL
  SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'sd',     'numeric', TO_CHAR(a.dic_sd)     FROM agg a
)
SELECT
  lr.cohort_definition_id,
  'cohort_name' AS group_name,
  TO_CHAR(lr.cohort_definition_id) AS group_level,
  'overall' AS strata_name,
  'overall' AS strata_level,
  lr.variable_name,
  lr.variable_level,
  lr.estimate_name,
  lr.estimate_type,
  lr.estimate_value,
  'overall' AS additional_name,
  'overall' AS additional_level
FROM long_rows lr
"
}

getCohortCharacteristicsSql_databricks <- function() {
  "
WITH cohort AS (
  SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
  FROM @cohortSchema.@cohortTable
),
p AS (
  SELECT person_id, gender_concept_id, year_of_birth
  FROM @cdm.person
),
op AS (
  SELECT person_id, observation_period_start_date, observation_period_end_date
  FROM @cdm.observation_period
),
base AS (
  SELECT
    c.cohort_definition_id,
    c.subject_id,
    c.cohort_start_date,
    c.cohort_end_date,
    datediff(c.cohort_end_date, c.cohort_start_date) + 1 AS days_in_cohort,
    year(c.cohort_start_date) - p.year_of_birth AS age_years,
    CASE
      WHEN p.gender_concept_id = 8532 THEN 'Female'
      WHEN p.gender_concept_id = 8507 THEN 'Male'
      ELSE 'Other/Unknown'
    END AS sex,
    datediff(c.cohort_start_date, o.observation_period_start_date) AS prior_observation,
    datediff(o.observation_period_end_date, c.cohort_start_date) AS future_observation
  FROM cohort c
  JOIN p ON p.person_id = c.subject_id
  LEFT JOIN op o
    ON o.person_id = c.subject_id
   AND c.cohort_start_date BETWEEN o.observation_period_start_date AND o.observation_period_end_date
),
subj AS (
  SELECT DISTINCT cohort_definition_id, subject_id, age_years, sex
  FROM base
),
agg AS (
  SELECT
    cohort_definition_id,
    COUNT(*) AS n_records,
    COUNT(DISTINCT subject_id) AS n_subjects,
    MIN(cohort_start_date) AS cs_min,
    date_add(to_date('1970-01-01'), CAST(percentile_approx(datediff(cohort_start_date, to_date('1970-01-01')), 0.25) AS INT)) AS cs_q25,
    date_add(to_date('1970-01-01'), CAST(percentile_approx(datediff(cohort_start_date, to_date('1970-01-01')), 0.50) AS INT)) AS cs_median,
    date_add(to_date('1970-01-01'), CAST(percentile_approx(datediff(cohort_start_date, to_date('1970-01-01')), 0.75) AS INT)) AS cs_q75,
    MAX(cohort_start_date) AS cs_max,
    MIN(cohort_end_date) AS ce_min,
    date_add(to_date('1970-01-01'), CAST(percentile_approx(datediff(cohort_end_date, to_date('1970-01-01')), 0.25) AS INT)) AS ce_q25,
    date_add(to_date('1970-01-01'), CAST(percentile_approx(datediff(cohort_end_date, to_date('1970-01-01')), 0.50) AS INT)) AS ce_median,
    date_add(to_date('1970-01-01'), CAST(percentile_approx(datediff(cohort_end_date, to_date('1970-01-01')), 0.75) AS INT)) AS ce_q75,
    MAX(cohort_end_date) AS ce_max,
    MIN(age_years) AS age_min,
    percentile_approx(age_years, 0.25) AS age_q25,
    percentile_approx(age_years, 0.50) AS age_median,
    percentile_approx(age_years, 0.75) AS age_q75,
    MAX(age_years) AS age_max,
    AVG(age_years) AS age_mean,
    STDDEV(age_years) AS age_sd,
    MIN(prior_observation) AS prior_min,
    percentile_approx(prior_observation, 0.25) AS prior_q25,
    percentile_approx(prior_observation, 0.50) AS prior_median,
    percentile_approx(prior_observation, 0.75) AS prior_q75,
    MAX(prior_observation) AS prior_max,
    AVG(prior_observation) AS prior_mean,
    STDDEV(prior_observation) AS prior_sd,
    MIN(future_observation) AS fut_min,
    percentile_approx(future_observation, 0.25) AS fut_q25,
    percentile_approx(future_observation, 0.50) AS fut_median,
    percentile_approx(future_observation, 0.75) AS fut_q75,
    MAX(future_observation) AS fut_max,
    AVG(future_observation) AS fut_mean,
    STDDEV(future_observation) AS fut_sd,
    MIN(days_in_cohort) AS dic_min,
    percentile_approx(days_in_cohort, 0.25) AS dic_q25,
    percentile_approx(days_in_cohort, 0.50) AS dic_median,
    percentile_approx(days_in_cohort, 0.75) AS dic_q75,
    MAX(days_in_cohort) AS dic_max,
    AVG(days_in_cohort) AS dic_mean,
    STDDEV(days_in_cohort) AS dic_sd
  FROM base
  GROUP BY cohort_definition_id
),
sex_counts AS (
  SELECT
    cohort_definition_id,
    SUM(CASE WHEN sex = 'Female' THEN 1 ELSE 0 END) AS n_female,
    SUM(CASE WHEN sex = 'Male' THEN 1 ELSE 0 END) AS n_male
  FROM subj
  GROUP BY cohort_definition_id
),
age_counts AS (
  SELECT
    cohort_definition_id,
    SUM(CASE WHEN age_years BETWEEN 0 AND 19 THEN 1 ELSE 0 END) AS n_0_19,
    SUM(CASE WHEN age_years BETWEEN 20 AND 39 THEN 1 ELSE 0 END) AS n_20_39,
    SUM(CASE WHEN age_years BETWEEN 40 AND 59 THEN 1 ELSE 0 END) AS n_40_59,
    SUM(CASE WHEN age_years BETWEEN 60 AND 79 THEN 1 ELSE 0 END) AS n_60_79,
    SUM(CASE WHEN age_years BETWEEN 80 AND 150 THEN 1 ELSE 0 END) AS n_80_150
  FROM subj
  GROUP BY cohort_definition_id
),
long_rows AS (
  SELECT a.cohort_definition_id, 'Number records' AS variable_name, 'NA' AS variable_level, 'count' AS estimate_name, 'integer' AS estimate_type, CAST(a.n_records AS STRING) AS estimate_value FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Number subjects', 'NA', 'count', 'integer', CAST(a.n_subjects AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'min', 'date', date_format(a.cs_min, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'q25', 'date', date_format(a.cs_q25, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'median', 'date', date_format(a.cs_median, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'q75', 'date', date_format(a.cs_q75, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort start date', 'NA', 'max', 'date', date_format(a.cs_max, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'min', 'date', date_format(a.ce_min, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'q25', 'date', date_format(a.ce_q25, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'median', 'date', date_format(a.ce_median, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'q75', 'date', date_format(a.ce_q75, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Cohort end date', 'NA', 'max', 'date', date_format(a.ce_max, 'yyyy-MM-dd') FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Age', 'NA', 'min', 'integer', CAST(a.age_min AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Age', 'NA', 'q25', 'integer', CAST(a.age_q25 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Age', 'NA', 'median', 'integer', CAST(a.age_median AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Age', 'NA', 'q75', 'integer', CAST(a.age_q75 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Age', 'NA', 'max', 'integer', CAST(a.age_max AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Age', 'NA', 'mean', 'numeric', CAST(a.age_mean AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Age', 'NA', 'sd', 'numeric', CAST(a.age_sd AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Sex', 'Female', 'count', 'integer', CAST(COALESCE(s.n_female, 0) AS STRING) FROM agg a LEFT JOIN sex_counts s ON a.cohort_definition_id = s.cohort_definition_id
  UNION ALL SELECT a.cohort_definition_id, 'Sex', 'Male', 'count', 'integer', CAST(COALESCE(s.n_male, 0) AS STRING) FROM agg a LEFT JOIN sex_counts s ON a.cohort_definition_id = s.cohort_definition_id
  UNION ALL SELECT a.cohort_definition_id, 'Age group', '0 to 19', 'count', 'integer', CAST(COALESCE(ac.n_0_19, 0) AS STRING) FROM agg a LEFT JOIN age_counts ac ON a.cohort_definition_id = ac.cohort_definition_id
  UNION ALL SELECT a.cohort_definition_id, 'Age group', '20 to 39', 'count', 'integer', CAST(COALESCE(ac.n_20_39, 0) AS STRING) FROM agg a LEFT JOIN age_counts ac ON a.cohort_definition_id = ac.cohort_definition_id
  UNION ALL SELECT a.cohort_definition_id, 'Age group', '40 to 59', 'count', 'integer', CAST(COALESCE(ac.n_40_59, 0) AS STRING) FROM agg a LEFT JOIN age_counts ac ON a.cohort_definition_id = ac.cohort_definition_id
  UNION ALL SELECT a.cohort_definition_id, 'Age group', '60 to 79', 'count', 'integer', CAST(COALESCE(ac.n_60_79, 0) AS STRING) FROM agg a LEFT JOIN age_counts ac ON a.cohort_definition_id = ac.cohort_definition_id
  UNION ALL SELECT a.cohort_definition_id, 'Age group', '80 to 150', 'count', 'integer', CAST(COALESCE(ac.n_80_150, 0) AS STRING) FROM agg a LEFT JOIN age_counts ac ON a.cohort_definition_id = ac.cohort_definition_id
  UNION ALL SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'min', 'integer', CAST(a.prior_min AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'q25', 'integer', CAST(a.prior_q25 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'median', 'integer', CAST(a.prior_median AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'q75', 'integer', CAST(a.prior_q75 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'max', 'integer', CAST(a.prior_max AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'mean', 'numeric', CAST(a.prior_mean AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Prior observation', 'NA', 'sd', 'numeric', CAST(a.prior_sd AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Future observation', 'NA', 'min', 'integer', CAST(a.fut_min AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Future observation', 'NA', 'q25', 'integer', CAST(a.fut_q25 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Future observation', 'NA', 'median', 'integer', CAST(a.fut_median AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Future observation', 'NA', 'q75', 'integer', CAST(a.fut_q75 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Future observation', 'NA', 'max', 'integer', CAST(a.fut_max AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Future observation', 'NA', 'mean', 'numeric', CAST(a.fut_mean AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Future observation', 'NA', 'sd', 'numeric', CAST(a.fut_sd AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'min', 'integer', CAST(a.dic_min AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'q25', 'integer', CAST(a.dic_q25 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'median', 'integer', CAST(a.dic_median AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'q75', 'integer', CAST(a.dic_q75 AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'max', 'integer', CAST(a.dic_max AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'mean', 'numeric', CAST(a.dic_mean AS STRING) FROM agg a
  UNION ALL SELECT a.cohort_definition_id, 'Days in cohort', 'NA', 'sd', 'numeric', CAST(a.dic_sd AS STRING) FROM agg a
)
SELECT
  cohort_definition_id,
  'cohort_name' AS group_name,
  CAST(cohort_definition_id AS STRING) AS group_level,
  'overall' AS strata_name,
  'overall' AS strata_level,
  variable_name,
  variable_level,
  estimate_name,
  estimate_type,
  estimate_value,
  'overall' AS additional_name,
  'overall' AS additional_level
FROM long_rows
  "
}

# -------------------------------------------------------------------------
# 4. Create Denominator Cohort ---------------------------------------------
# -------------------------------------------------------------------------

#' @noRd
#' @keywords internal
createDenominatorCohort <- function(connection,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    denominatorTable,
                                    startDate,
                                    endDate,
                                    minPriorObservation = 365) {
  
  ParallelLogger::logInfo(paste0(
    "Creating stratified denominator cohort table '", denominatorTable,
    "' from ", startDate, " to ", endDate,
    " with min prior observation = ", minPriorObservation, " days"
  ))
  
  dbms <- .getDbms(connection)
  if (.isDatabricks(dbms)) {
    createDenominatorCohortDatabricks(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      denominatorTable = denominatorTable,
      startDate = startDate,
      endDate = endDate,
      minPriorObservation = minPriorObservation
    )
    return(invisible(TRUE))
  }
  if (dbms != "oracle") {
    stop("createDenominatorCohort(): unsupported dbms = ", dbms)
  }
  
  startDateStr <- format(as.Date(startDate), "%Y-%m-%d")
  endDateStr   <- format(as.Date(endDate), "%Y-%m-%d")
  
  # 1) Drop table (Oracle-safe)
  dropSql <- "
  BEGIN
    EXECUTE IMMEDIATE 'DROP TABLE @cohortSchema.@denom PURGE';
  EXCEPTION
    WHEN OTHERS THEN
      IF SQLCODE != -942 THEN RAISE; END IF;
  END;
  "
  
  dropSql <- SqlRender::render(
    dropSql,
    cohortSchema = cohortDatabaseSchema,
    denom        = denominatorTable
  )
  dropSql <- SqlRender::translate(dropSql, targetDialect = attr(connection, "dbms"))
  DatabaseConnector::executeSql(connection, dropSql)
  
  # 2) Create table as select (CTAS)
  sql <- "
  CREATE TABLE @cohortSchema.@denom NOLOGGING AS
  WITH person_base AS (
    SELECT
      p.person_id,
      p.gender_concept_id,
      TO_DATE(
        p.year_of_birth || '-' ||
        LPAD(NVL(NULLIF(p.month_of_birth, 0), 1), 2, '0') || '-' ||
        LPAD(NVL(NULLIF(p.day_of_birth,   0), 1), 2, '0'),
        'YYYY-MM-DD'
      ) AS dob
    FROM @cdm.person p
    WHERE p.year_of_birth IS NOT NULL
      AND p.gender_concept_id IN (8507, 8532)
  ),
  obs AS (
    SELECT
      op.person_id,
      op.observation_period_start_date,
      op.observation_period_end_date
    FROM @cdm.observation_period op
    WHERE op.observation_period_end_date   >= TO_DATE('@startDate','YYYY-MM-DD')
      AND op.observation_period_start_date <= TO_DATE('@endDate','YYYY-MM-DD')
  ),
  base AS (
    SELECT
      pb.person_id,
      pb.gender_concept_id,
      pb.dob,
      CASE
        WHEN (o.observation_period_start_date + @minPrior) > TO_DATE('@startDate','YYYY-MM-DD')
          THEN (o.observation_period_start_date + @minPrior)
        ELSE TO_DATE('@startDate','YYYY-MM-DD')
      END AS risk_start_date,
      CASE
        WHEN o.observation_period_end_date < TO_DATE('@endDate','YYYY-MM-DD')
          THEN o.observation_period_end_date
        ELSE TO_DATE('@endDate','YYYY-MM-DD')
      END AS risk_end_date
    FROM person_base pb
    JOIN obs o
      ON o.person_id = pb.person_id
  ),
  base_clean AS (
    SELECT DISTINCT
      person_id,
      gender_concept_id,
      dob,
      risk_start_date,
      risk_end_date
    FROM base
    WHERE risk_start_date <= risk_end_date
  ),
  age_band_intervals AS (
    SELECT
      b.person_id,
      b.gender_concept_id,

      /* 0-18 */
      GREATEST(ADD_MONTHS(b.dob, 12*0),  b.risk_start_date) AS start_0_18,
      LEAST(ADD_MONTHS(b.dob, 12*19) - 1, b.risk_end_date) AS end_0_18,

      /* 19-39 */
      GREATEST(ADD_MONTHS(b.dob, 12*19), b.risk_start_date) AS start_19_39,
      LEAST(ADD_MONTHS(b.dob, 12*40) - 1, b.risk_end_date) AS end_19_39,

      /* 40-65 */
      GREATEST(ADD_MONTHS(b.dob, 12*40), b.risk_start_date) AS start_40_65,
      LEAST(ADD_MONTHS(b.dob, 12*66) - 1, b.risk_end_date) AS end_40_65,

      /* 66-150 */
      GREATEST(ADD_MONTHS(b.dob, 12*66), b.risk_start_date) AS start_66_150,
      LEAST(ADD_MONTHS(b.dob, 12*151) - 1, b.risk_end_date) AS end_66_150,

      /* 0-150 */
      b.risk_start_date AS start_0_150,
      LEAST(ADD_MONTHS(b.dob, 12*151) - 1, b.risk_end_date) AS end_0_150

    FROM base_clean b
  )
  SELECT
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date
  FROM (
    SELECT 1  AS cohort_definition_id, a.person_id AS subject_id, a.start_0_18   AS cohort_start_date, a.end_0_18   AS cohort_end_date
    FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_0_18   <= a.end_0_18
    UNION ALL
    SELECT 2, a.person_id, a.start_0_18,   a.end_0_18   FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_0_18 <= a.end_0_18
    UNION ALL
    SELECT 3, a.person_id, a.start_0_18,   a.end_0_18   FROM age_band_intervals a WHERE a.gender_concept_id IN (8507,8532) AND a.start_0_18 <= a.end_0_18

    UNION ALL
    SELECT 4, a.person_id, a.start_19_39,  a.end_19_39  FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_19_39 <= a.end_19_39
    UNION ALL
    SELECT 5, a.person_id, a.start_19_39,  a.end_19_39  FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_19_39 <= a.end_19_39
    UNION ALL
    SELECT 6, a.person_id, a.start_19_39,  a.end_19_39  FROM age_band_intervals a WHERE a.gender_concept_id IN (8507,8532) AND a.start_19_39 <= a.end_19_39

    UNION ALL
    SELECT 7, a.person_id, a.start_40_65,  a.end_40_65  FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_40_65 <= a.end_40_65
    UNION ALL
    SELECT 8, a.person_id, a.start_40_65,  a.end_40_65  FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_40_65 <= a.end_40_65
    UNION ALL
    SELECT 9, a.person_id, a.start_40_65,  a.end_40_65  FROM age_band_intervals a WHERE a.gender_concept_id IN (8507,8532) AND a.start_40_65 <= a.end_40_65

    UNION ALL
    SELECT 10, a.person_id, a.start_66_150, a.end_66_150 FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_66_150 <= a.end_66_150
    UNION ALL
    SELECT 11, a.person_id, a.start_66_150, a.end_66_150 FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_66_150 <= a.end_66_150
    UNION ALL
    SELECT 12, a.person_id, a.start_66_150, a.end_66_150 FROM age_band_intervals a WHERE a.gender_concept_id IN (8507,8532) AND a.start_66_150 <= a.end_66_150

    UNION ALL
    SELECT 13, a.person_id, a.start_0_150, a.end_0_150   FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_0_150 <= a.end_0_150
    UNION ALL
    SELECT 14, a.person_id, a.start_0_150, a.end_0_150   FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_0_150 <= a.end_0_150
    UNION ALL
    SELECT 15, a.person_id, a.start_0_150, a.end_0_150   FROM age_band_intervals a WHERE a.gender_concept_id IN (8507,8532) AND a.start_0_150 <= a.end_0_150
  )
  "
  
  sql <- SqlRender::render(
    sql,
    cdm          = cdmDatabaseSchema,
    cohortSchema = cohortDatabaseSchema,
    denom        = denominatorTable,
    startDate    = startDateStr,
    endDate      = endDateStr,
    minPrior     = minPriorObservation
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  
  DatabaseConnector::executeSql(connection, sql)
  ParallelLogger::logInfo("Stratified denominator cohorts created (Oracle CTAS).")
}

createDenominatorCohortDatabricks <- function(connection,
                                              cdmDatabaseSchema,
                                              cohortDatabaseSchema,
                                              denominatorTable,
                                              startDate,
                                              endDate,
                                              minPriorObservation = 365) {
  startDateStr <- format(as.Date(startDate), "%Y-%m-%d")
  endDateStr   <- format(as.Date(endDate), "%Y-%m-%d")
  
  sql <- "
  CREATE OR REPLACE TABLE @cohortSchema.@denom AS
  WITH person_base AS (
    SELECT
      p.person_id,
      p.gender_concept_id,
      make_date(
        p.year_of_birth,
        COALESCE(NULLIF(p.month_of_birth, 0), 1),
        COALESCE(NULLIF(p.day_of_birth, 0), 1)
      ) AS dob
    FROM @cdm.person p
    WHERE p.year_of_birth IS NOT NULL
      AND p.gender_concept_id IN (8507, 8532)
  ),
  obs AS (
    SELECT
      op.person_id,
      op.observation_period_start_date,
      op.observation_period_end_date
    FROM @cdm.observation_period op
    WHERE op.observation_period_end_date   >= to_date('@startDate')
      AND op.observation_period_start_date <= to_date('@endDate')
  ),
  base AS (
    SELECT
      pb.person_id,
      pb.gender_concept_id,
      pb.dob,
      CASE
        WHEN date_add(o.observation_period_start_date, @minPrior) > to_date('@startDate')
          THEN date_add(o.observation_period_start_date, @minPrior)
        ELSE to_date('@startDate')
      END AS risk_start_date,
      CASE
        WHEN o.observation_period_end_date < to_date('@endDate')
          THEN o.observation_period_end_date
        ELSE to_date('@endDate')
      END AS risk_end_date
    FROM person_base pb
    JOIN obs o
      ON o.person_id = pb.person_id
  ),
  base_clean AS (
    SELECT DISTINCT
      person_id,
      gender_concept_id,
      dob,
      risk_start_date,
      risk_end_date
    FROM base
    WHERE risk_start_date <= risk_end_date
  ),
  age_band_intervals AS (
    SELECT
      b.person_id,
      b.gender_concept_id,
      greatest(add_months(b.dob, 12 * 0), b.risk_start_date) AS start_0_18,
      least(date_add(add_months(b.dob, 12 * 19), -1), b.risk_end_date) AS end_0_18,
      greatest(add_months(b.dob, 12 * 19), b.risk_start_date) AS start_19_39,
      least(date_add(add_months(b.dob, 12 * 40), -1), b.risk_end_date) AS end_19_39,
      greatest(add_months(b.dob, 12 * 40), b.risk_start_date) AS start_40_65,
      least(date_add(add_months(b.dob, 12 * 66), -1), b.risk_end_date) AS end_40_65,
      greatest(add_months(b.dob, 12 * 66), b.risk_start_date) AS start_66_150,
      least(date_add(add_months(b.dob, 12 * 151), -1), b.risk_end_date) AS end_66_150,
      b.risk_start_date AS start_0_150,
      least(date_add(add_months(b.dob, 12 * 151), -1), b.risk_end_date) AS end_0_150
    FROM base_clean b
  )
  SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
  FROM (
    SELECT 1 AS cohort_definition_id, a.person_id AS subject_id, a.start_0_18 AS cohort_start_date, a.end_0_18 AS cohort_end_date FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_0_18 <= a.end_0_18
    UNION ALL SELECT 2, a.person_id, a.start_0_18, a.end_0_18 FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_0_18 <= a.end_0_18
    UNION ALL SELECT 3, a.person_id, a.start_0_18, a.end_0_18 FROM age_band_intervals a WHERE a.gender_concept_id IN (8507, 8532) AND a.start_0_18 <= a.end_0_18
    UNION ALL SELECT 4, a.person_id, a.start_19_39, a.end_19_39 FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_19_39 <= a.end_19_39
    UNION ALL SELECT 5, a.person_id, a.start_19_39, a.end_19_39 FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_19_39 <= a.end_19_39
    UNION ALL SELECT 6, a.person_id, a.start_19_39, a.end_19_39 FROM age_band_intervals a WHERE a.gender_concept_id IN (8507, 8532) AND a.start_19_39 <= a.end_19_39
    UNION ALL SELECT 7, a.person_id, a.start_40_65, a.end_40_65 FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_40_65 <= a.end_40_65
    UNION ALL SELECT 8, a.person_id, a.start_40_65, a.end_40_65 FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_40_65 <= a.end_40_65
    UNION ALL SELECT 9, a.person_id, a.start_40_65, a.end_40_65 FROM age_band_intervals a WHERE a.gender_concept_id IN (8507, 8532) AND a.start_40_65 <= a.end_40_65
    UNION ALL SELECT 10, a.person_id, a.start_66_150, a.end_66_150 FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_66_150 <= a.end_66_150
    UNION ALL SELECT 11, a.person_id, a.start_66_150, a.end_66_150 FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_66_150 <= a.end_66_150
    UNION ALL SELECT 12, a.person_id, a.start_66_150, a.end_66_150 FROM age_band_intervals a WHERE a.gender_concept_id IN (8507, 8532) AND a.start_66_150 <= a.end_66_150
    UNION ALL SELECT 13, a.person_id, a.start_0_150, a.end_0_150 FROM age_band_intervals a WHERE a.gender_concept_id = 8507 AND a.start_0_150 <= a.end_0_150
    UNION ALL SELECT 14, a.person_id, a.start_0_150, a.end_0_150 FROM age_band_intervals a WHERE a.gender_concept_id = 8532 AND a.start_0_150 <= a.end_0_150
    UNION ALL SELECT 15, a.person_id, a.start_0_150, a.end_0_150 FROM age_band_intervals a WHERE a.gender_concept_id IN (8507, 8532) AND a.start_0_150 <= a.end_0_150
  ) denominator_rows
  "
  
  sql <- SqlRender::render(
    sql,
    cdm = cdmDatabaseSchema,
    cohortSchema = cohortDatabaseSchema,
    denom = denominatorTable,
    startDate = startDateStr,
    endDate = endDateStr,
    minPrior = minPriorObservation
  )
  DatabaseConnector::executeSql(connection, sql)
  ParallelLogger::logInfo("Stratified denominator cohorts created (Databricks CTAS).")
}

# -------------------------------------------------------------------------
# 5. Incidence Estimation ---------------------------------------------------
# -------------------------------------------------------------------------

#' @noRd
#' @keywords internal
estimateIncidenceSql <- function(connection,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 denominatorTable,
                                 outcomeTable,
                                 outcomeIds,
                                 minCellCount) {
  
  ParallelLogger::logInfo(paste0("Estimating incidence for outcome table: ", outcomeTable))
  
  dbms <- .getDbms(connection)
  if (.isDatabricks(dbms)) {
    return(estimateIncidenceSqlDatabricks(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      denominatorTable = denominatorTable,
      outcomeTable = outcomeTable,
      outcomeIds = outcomeIds,
      minCellCount = minCellCount
    ))
  }
  if (dbms != "oracle") {
    stop("estimateIncidenceSql(): unsupported dbms = ", dbms)
  }
  
  outcomeIds <- outcomeIds[!is.na(outcomeIds)]
  if (length(outcomeIds) == 0) {
    ParallelLogger::logInfo("  - No outcome IDs provided; returning empty result.")
    return(dplyr::tibble())
  }
  
  sql <- "
  WITH date_bounds AS (
    SELECT MIN(cohort_start_date) AS start_date,
           MAX(cohort_end_date)   AS end_date
    FROM @cohortSchema.@denomTable
  ),
  months AS (
    SELECT
      ADD_MONTHS(TRUNC(start_date, 'MM'), LEVEL - 1) AS month_start,
      (LAST_DAY(ADD_MONTHS(TRUNC(start_date, 'MM'), LEVEL - 1)) + 1 - (1/86400)) AS month_end
    FROM date_bounds
    CONNECT BY ADD_MONTHS(TRUNC(start_date, 'MM'), LEVEL - 1) <= TRUNC(end_date, 'MM')
  ),
  outcome_ids AS (
    SELECT DISTINCT cohort_definition_id AS outcome_id
    FROM @cohortSchema.@outcomeTable
    WHERE cohort_definition_id IN (@outcomeIds)
  ),
  outcome_all AS (
    SELECT
      o.subject_id,
      o.cohort_definition_id AS outcome_id,
      o.cohort_start_date    AS event_date
    FROM @cohortSchema.@outcomeTable o
    JOIN outcome_ids oi
      ON o.cohort_definition_id = oi.outcome_id
  ),
  outcome_first_subject AS (
    SELECT
      subject_id,
      outcome_id,
      MIN(event_date) AS first_event_date
    FROM outcome_all
    GROUP BY subject_id, outcome_id
  ),
  denom_with_first AS (
    SELECT
      d.cohort_definition_id AS denom_id,
      oi.outcome_id,
      d.subject_id,
      d.cohort_start_date,
      d.cohort_end_date,
      ofirst.first_event_date
    FROM @cohortSchema.@denomTable d
    CROSS JOIN outcome_ids oi
    LEFT JOIN outcome_first_subject ofirst
      ON d.subject_id  = ofirst.subject_id
     AND oi.outcome_id = ofirst.outcome_id
    WHERE d.cohort_start_date <= d.cohort_end_date
  ),
  denom_at_risk AS (
    SELECT
      denom_id,
      outcome_id,
      subject_id,
      cohort_start_date,
      cohort_end_date,
      CASE
        WHEN first_event_date IS NOT NULL
             AND first_event_date >= cohort_start_date
             AND first_event_date <= cohort_end_date
          THEN first_event_date
        ELSE cohort_end_date
      END AS at_risk_end_date
    FROM denom_with_first
    WHERE first_event_date IS NULL
       OR first_event_date >= cohort_start_date
  ),
  denom_by_month AS (
    SELECT
      da.denom_id,
      da.outcome_id,
      da.subject_id,
      m.month_start,
      m.month_end,
      CASE WHEN da.cohort_start_date > m.month_start THEN da.cohort_start_date ELSE m.month_start END AS start_in_month,
      CASE WHEN da.at_risk_end_date < m.month_end THEN da.at_risk_end_date ELSE m.month_end END AS end_in_month
    FROM denom_at_risk da
    JOIN months m
      ON da.cohort_start_date <= m.month_end
     AND da.at_risk_end_date   >= m.month_start
  ),
  denom_summary AS (
    SELECT
      denom_id,
      outcome_id,
      EXTRACT(YEAR FROM month_start)  AS cal_year,
      EXTRACT(MONTH FROM month_start) AS cal_month,
      COUNT(DISTINCT subject_id) AS denominator_count,
      SUM(TRUNC(end_in_month) - TRUNC(start_in_month) + 1) AS person_days
    FROM denom_by_month
    GROUP BY denom_id, outcome_id,
             EXTRACT(YEAR FROM month_start),
             EXTRACT(MONTH FROM month_start)
  ),
  incidence_events AS (
    SELECT
      denom_id,
      outcome_id,
      subject_id,
      first_event_date AS event_date
    FROM denom_with_first
    WHERE first_event_date IS NOT NULL
      AND first_event_date >= cohort_start_date
      AND first_event_date <= cohort_end_date
  ),
  outcome_counts AS (
    SELECT
      denom_id,
      outcome_id,
      EXTRACT(YEAR FROM event_date)  AS cal_year,
      EXTRACT(MONTH FROM event_date) AS cal_month,
      COUNT(*) AS outcome_count
    FROM incidence_events
    GROUP BY denom_id, outcome_id,
             EXTRACT(YEAR FROM event_date),
             EXTRACT(MONTH FROM event_date)
  )
  SELECT
    ds.denom_id,
    ds.outcome_id,
    ds.cal_year,
    ds.cal_month,
    NVL(oc.outcome_count, 0) AS outcome_count,
    ds.denominator_count     AS denom_count,
    ds.person_days,
    (ds.person_days / 365.25) AS person_years,
    CASE
      WHEN ds.person_days = 0 THEN NULL
      ELSE (NVL(oc.outcome_count, 0) / (ds.person_days / 365.25)) * 100000
    END AS incidence_100000_pys
  FROM denom_summary ds
  LEFT JOIN outcome_counts oc
    ON oc.denom_id   = ds.denom_id
   AND oc.outcome_id = ds.outcome_id
   AND oc.cal_year   = ds.cal_year
   AND oc.cal_month  = ds.cal_month
  ORDER BY ds.denom_id, ds.outcome_id, ds.cal_year, ds.cal_month
  "
  
  sql <- SqlRender::render(
    sql,
    cohortSchema = cohortDatabaseSchema,
    outcomeTable = outcomeTable,
    denomTable   = denominatorTable,
    outcomeIds   = outcomeIds
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  
  res <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
  
  res$outcome_table <- outcomeTable
  
  denomCol <- if ("denom_id" %in% names(res)) "denom_id" else if ("DENOM_ID" %in% names(res)) "DENOM_ID" else NULL
  if (!is.null(denomCol)) {
    res$denominator_cohort <- paste0("denominator_cohort_", res[[denomCol]])
  }
  
  # small cell suppression
  if ("outcome_count" %in% names(res)) {
    res$outcome_count[res$outcome_count > 0 & res$outcome_count < minCellCount] <- NA
  } else if ("OUTCOME_COUNT" %in% names(res)) {
    res$OUTCOME_COUNT[res$OUTCOME_COUNT > 0 & res$OUTCOME_COUNT < minCellCount] <- NA
  }
  if ("denom_count" %in% names(res)) {
    res$denom_count[res$denom_count > 0 & res$denom_count < minCellCount] <- NA
  } else if ("DENOM_COUNT" %in% names(res)) {
    res$DENOM_COUNT[res$DENOM_COUNT > 0 & res$DENOM_COUNT < minCellCount] <- NA
  }
  
  res
}


estimateIncidenceSqlDatabricks <- function(connection,
                                           cdmDatabaseSchema,
                                           cohortDatabaseSchema,
                                           denominatorTable,
                                           outcomeTable,
                                           outcomeIds,
                                           minCellCount) {
  outcomeIds <- outcomeIds[!is.na(outcomeIds)]
  if (length(outcomeIds) == 0) {
    ParallelLogger::logInfo("  - No outcome IDs provided; returning empty result.")
    return(dplyr::tibble())
  }
  
  sql <- "
  WITH date_bounds AS (
    SELECT MIN(cohort_start_date) AS start_date,
           MAX(cohort_end_date) AS end_date
    FROM @cohortSchema.@denomTable
  ),
  month_starts AS (
    SELECT explode(sequence(
      CAST(date_trunc('month', start_date) AS DATE),
      CAST(date_trunc('month', end_date) AS DATE),
      interval 1 month
    )) AS month_start
    FROM date_bounds
  ),
  months AS (
    SELECT
      month_start,
      last_day(month_start) AS month_end
    FROM month_starts
  ),
  outcome_ids AS (
    SELECT DISTINCT cohort_definition_id AS outcome_id
    FROM @cohortSchema.@outcomeTable
    WHERE cohort_definition_id IN (@outcomeIds)
  ),
  outcome_all AS (
    SELECT
      o.subject_id,
      o.cohort_definition_id AS outcome_id,
      o.cohort_start_date AS event_date
    FROM @cohortSchema.@outcomeTable o
    JOIN outcome_ids oi
      ON o.cohort_definition_id = oi.outcome_id
  ),
  outcome_first_subject AS (
    SELECT
      subject_id,
      outcome_id,
      MIN(event_date) AS first_event_date
    FROM outcome_all
    GROUP BY subject_id, outcome_id
  ),
  denom_with_first AS (
    SELECT
      d.cohort_definition_id AS denom_id,
      oi.outcome_id,
      d.subject_id,
      d.cohort_start_date,
      d.cohort_end_date,
      ofirst.first_event_date
    FROM @cohortSchema.@denomTable d
    CROSS JOIN outcome_ids oi
    LEFT JOIN outcome_first_subject ofirst
      ON d.subject_id = ofirst.subject_id
     AND oi.outcome_id = ofirst.outcome_id
    WHERE d.cohort_start_date <= d.cohort_end_date
  ),
  denom_at_risk AS (
    SELECT
      denom_id,
      outcome_id,
      subject_id,
      cohort_start_date,
      cohort_end_date,
      CASE
        WHEN first_event_date IS NOT NULL
             AND first_event_date >= cohort_start_date
             AND first_event_date <= cohort_end_date
          THEN first_event_date
        ELSE cohort_end_date
      END AS at_risk_end_date
    FROM denom_with_first
    WHERE first_event_date IS NULL
       OR first_event_date >= cohort_start_date
  ),
  denom_by_month AS (
    SELECT
      da.denom_id,
      da.outcome_id,
      da.subject_id,
      m.month_start,
      m.month_end,
      CASE WHEN da.cohort_start_date > m.month_start THEN da.cohort_start_date ELSE m.month_start END AS start_in_month,
      CASE WHEN da.at_risk_end_date < m.month_end THEN da.at_risk_end_date ELSE m.month_end END AS end_in_month
    FROM denom_at_risk da
    JOIN months m
      ON da.cohort_start_date <= m.month_end
     AND da.at_risk_end_date >= m.month_start
  ),
  denom_summary AS (
    SELECT
      denom_id,
      outcome_id,
      year(month_start) AS cal_year,
      month(month_start) AS cal_month,
      COUNT(DISTINCT subject_id) AS denominator_count,
      SUM(datediff(end_in_month, start_in_month) + 1) AS person_days
    FROM denom_by_month
    GROUP BY denom_id, outcome_id, year(month_start), month(month_start)
  ),
  incidence_events AS (
    SELECT
      denom_id,
      outcome_id,
      subject_id,
      first_event_date AS event_date
    FROM denom_with_first
    WHERE first_event_date IS NOT NULL
      AND first_event_date >= cohort_start_date
      AND first_event_date <= cohort_end_date
  ),
  outcome_counts AS (
    SELECT
      denom_id,
      outcome_id,
      year(event_date) AS cal_year,
      month(event_date) AS cal_month,
      COUNT(*) AS outcome_count
    FROM incidence_events
    GROUP BY denom_id, outcome_id, year(event_date), month(event_date)
  )
  SELECT
    ds.denom_id,
    ds.outcome_id,
    ds.cal_year,
    ds.cal_month,
    COALESCE(oc.outcome_count, 0) AS outcome_count,
    ds.denominator_count AS denom_count,
    ds.person_days,
    (ds.person_days / 365.25) AS person_years,
    CASE
      WHEN ds.person_days = 0 THEN NULL
      ELSE (COALESCE(oc.outcome_count, 0) / (ds.person_days / 365.25)) * 100000
    END AS incidence_100000_pys
  FROM denom_summary ds
  LEFT JOIN outcome_counts oc
    ON oc.denom_id = ds.denom_id
   AND oc.outcome_id = ds.outcome_id
   AND oc.cal_year = ds.cal_year
   AND oc.cal_month = ds.cal_month
  ORDER BY ds.denom_id, ds.outcome_id, ds.cal_year, ds.cal_month
  "
  
  sql <- SqlRender::render(
    sql,
    cohortSchema = cohortDatabaseSchema,
    outcomeTable = outcomeTable,
    denomTable = denominatorTable,
    outcomeIds = outcomeIds
  )
  
  res <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = FALSE)
  names(res) <- tolower(names(res))
  res$outcome_table <- outcomeTable
  res$denominator_cohort <- paste0("denominator_cohort_", res$denom_id)
  
  res$outcome_count[res$outcome_count > 0 & res$outcome_count < minCellCount] <- NA
  res$denom_count[res$denom_count > 0 & res$denom_count < minCellCount] <- NA
  
  res
}



#' Convert SQL incidence output to omopgenerics-style summarisedResult
#' (incidence part: months, years, overall, with CIs)
#' @noRd
#' @keywords internal
formatIncidenceToSummarisedResult <- function(incidence,
                                              startDate,
                                              endDate,
                                              minCellCount) {
  if (nrow(incidence) == 0) {
    return(dplyr::tibble())
  }
  
  startDate <- as.Date(startDate)
  endDate   <- as.Date(endDate)
  
  #-----------------------------------------------------------------
  # 0. Normalise incidence column names to lower-case
  #-----------------------------------------------------------------
  names(incidence) <- tolower(names(incidence))
  
  # Make sure we have outcome_id and outcome_table
  if (!"outcome_id" %in% names(incidence)) {
    if ("cohort_definition_id" %in% names(incidence)) {
      incidence <- dplyr::rename(incidence, outcome_id = cohort_definition_id)
    } else if ("outcomeid" %in% names(incidence)) {
      incidence <- dplyr::rename(incidence, outcome_id = outcomeid)
    } else {
      stop("Could not find an outcome_id column in incidence. Columns: ",
           paste(names(incidence), collapse = ", "))
    }
  }
  if (!"outcome_table" %in% names(incidence)) {
    stop("Expected 'outcome_table' column in incidence, but it is missing.")
  }

  #-----------------------------------------------------------------
  # 1. Attach cohort names (chapter_*) using CohortsToCreateFinal.csv
  #-----------------------------------------------------------------
  cohortsToCreate <- utils::read.csv(
    system.file("settings", "CohortsToCreateFinal.csv", package = "CHAPTER"),
    stringsAsFactors = FALSE
  )
  
  # Normalise *label* table names to lower-case
  names(cohortsToCreate) <- tolower(names(cohortsToCreate))
  
  # Work out the actual column names in the CSV (robust to case)
  cohortIdCol   <- grep("^cohortid$",   names(cohortsToCreate), ignore.case = TRUE, value = TRUE)[1]
  cohortNameCol <- grep("^cohortname$", names(cohortsToCreate), ignore.case = TRUE, value = TRUE)[1]
  tableNameCol  <- grep("^table_name$", names(cohortsToCreate), ignore.case = TRUE, value = TRUE)[1]
  
  if (is.na(cohortIdCol) || is.na(cohortNameCol) || is.na(tableNameCol)) {
    stop(
      "Could not find cohortId/cohortName/table_name columns in CohortsToCreateFinal.csv.\n",
      "Columns present: ", paste(names(cohortsToCreate), collapse = ", ")
    )
  }
  
  # Build a clean look-up table with standard names
  outcomeLabels <- cohortsToCreate %>%
    dplyr::select(
      cohort_id   = dplyr::all_of(cohortIdCol),
      cohort_name = dplyr::all_of(cohortNameCol),
      table_name  = dplyr::all_of(tableNameCol)
    )
  
  # Ensure incidence has lower-case names and outcome_id/outcome_table
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
  
  # Join labels
  incidence <- incidence %>%
    dplyr::left_join(
      outcomeLabels,
      by = c("outcome_id" = "cohort_id", "outcome_table" = "table_name")
    )
  
  # Now assign outcome_name in base R (no dplyr pronoun issues)
  if ("cohort_name" %in% names(incidence)) {
    cohName <- incidence$cohort_name
    incidence$outcome_name <- ifelse(
      !is.na(cohName) & cohName != "",
      cohName,
      paste0(incidence$outcome_table, "_", incidence$outcome_id)
    )
  } else {
    # Fallback if the join somehow didn't bring cohort_name
    incidence$outcome_name <- paste0(incidence$outcome_table, "_", incidence$outcome_id)
  }
  
    # Ensure we have the denominator label as in the original results
  if (!"denominator_cohort" %in% names(incidence)) {
    # fallback if estimateIncidenceSql didn't already add this
    denomCol <- if ("denom_id" %in% names(incidence)) "denom_id" else if ("DENOM_ID" %in% names(incidence)) "DENOM_ID" else NULL
    if (!is.null(denomCol)) {
      incidence$denominator_cohort <- paste0("denominator_cohort_", incidence[[denomCol]])
    } else {
      incidence$denominator_cohort <- NA_character_
    }
  }
  
  #-------------------------------------------------------------------------------
  # 2. Helper: Poisson 95% CI for incidence per 100,000 PYs
  #-------------------------------------------------------------------------------
  poisson_ci <- function(k, py) {
    if (is.na(k) || is.na(py) || py <= 0 || k == 0) {
      return(c(NA_real_, NA_real_))
    }
    alpha <- 0.05
    lower_count <- 0.5 * stats::qchisq(alpha / 2, df = 2 * k)
    upper_count <- 0.5 * stats::qchisq(1 - alpha / 2, df = 2 * (k + 1))
    lower_rate  <- (lower_count / py) * 100000
    upper_rate  <- (upper_count / py) * 100000
    c(lower_rate, upper_rate)
  }
  
  #-------------------------------------------------------------------------------
  # 3. Build interval-level summaries from monthly SQL output
  #    3.1 Months (as in original IP interval = "months")
  #-------------------------------------------------------------------------------
  # We assume each row in 'incidence' is a single {denom_id, outcome_id, year, month}
  month_df <- incidence %>%
    dplyr::mutate(
      interval_start_date = as.Date(
        paste0(.data$cal_year, "-", sprintf("%02d", .data$cal_month), "-01")
      ),
      interval_end_date = as.Date(
        format(
          .data$interval_start_date + 31, "%Y-%m-01"
        )
      ) - 1L,
      interval_label = "months"
    ) %>%
    dplyr::mutate(
      person_years = .data$person_days / 365.25
    )
  
  #-------------------------------------------------------------------------------
  #    3.2 Years (interval = "years" like original code)
  #-------------------------------------------------------------------------------
  year_df <- incidence %>%
    dplyr::group_by(.data$denom_id, .data$outcome_id, .data$denominator_cohort,
                    .data$outcome_name, .data$cal_year) %>%
    dplyr::summarise(
      outcome_count = sum(.data$outcome_count, na.rm = TRUE),
      person_days   = sum(.data$person_days, na.rm = TRUE),
      denom_count   = max(.data$denom_count, na.rm = TRUE),
      .groups       = "drop"
    ) %>%
    dplyr::mutate(
      interval_start_date = as.Date(paste0(.data$cal_year, "-01-01")),
      interval_end_date   = as.Date(paste0(.data$cal_year, "-12-31")),
      person_years        = .data$person_days / 365.25,
      interval_label      = "years"
    )
  
  #-------------------------------------------------------------------------------
  #    3.3 Overall (interval = "overall")
  #-------------------------------------------------------------------------------
  overall_df <- incidence %>%
    dplyr::group_by(.data$denom_id, .data$outcome_id, .data$denominator_cohort,
                    .data$outcome_name) %>%
    dplyr::summarise(
      outcome_count = sum(.data$outcome_count, na.rm = TRUE),
      person_days   = sum(.data$person_days, na.rm = TRUE),
      denom_count   = max(.data$denom_count, na.rm = TRUE),
      .groups       = "drop"
    ) %>%
    dplyr::mutate(
      interval_start_date = startDate,
      interval_end_date   = endDate,
      person_years        = .data$person_days / 365.25,
      interval_label      = "overall"
    )
  
  #-------------------------------------------------------------------------------
  # 4. Function to turn an interval-level df into long-format rows
  #-------------------------------------------------------------------------------
  make_long <- function(df) {
    if (nrow(df) == 0) return(dplyr::tibble())
    
    # Ensure we have outcome_name and denominator_cohort
    if (!"outcome_name" %in% names(df)) {
      df$outcome_name <- paste0(df$outcome_table, "_", df$outcome_id)
    }
    if (!"denominator_cohort" %in% names(df)) {
      denomCol <- if ("denom_id" %in% names(df)) "denom_id" else NULL
      if (!is.null(denomCol)) {
        df$denominator_cohort <- paste0("denominator_cohort_", df[[denomCol]])
      } else {
        df$denominator_cohort <- NA_character_
      }
    }
    
    # Calculate incidence and CIs
    df <- df %>%
      dplyr::mutate(
        incidence_100000_pys = dplyr::if_else(
          !is.na(.data$person_years) & .data$person_years > 0,
          (.data$outcome_count / .data$person_years) * 100000,
          NA_real_
        )
      )
    
    ci_mat <- mapply(
      poisson_ci,
      k  = df$outcome_count,
      py = df$person_years
    )
    df$incidence_100000_pys_95CI_lower <- ci_mat[1, ]
    df$incidence_100000_pys_95CI_upper <- ci_mat[2, ]
    
    df <- df %>%
      dplyr::mutate(
        group_name   = "denominator_cohort_name &&& outcome_cohort_name",
        group_level  = paste0(.data$denominator_cohort, " &&& ", .data$outcome_name),
        strata_name  = "overall",
        strata_level = "overall",
        additional_name  = "incidence_start_date &&& incidence_end_date &&& interval_name",
        additional_level = paste0(
          format(.data$interval_start_date, "%Y-%m-%d"),
          " &&& ",
          format(.data$interval_end_date, "%Y-%m-%d"),
          " &&& ",
          .data$interval_label
        )
      )
    
    # Denominator rows
    denom_count_rows <- df %>%
      dplyr::transmute(
        result_id     = 1L,
        cdm_name      = NA_character_,
        group_name,
        group_level,
        strata_name,
        strata_level,
        variable_name = "Denominator",
        variable_level = NA_character_,
        estimate_name = "denominator_count",
        estimate_type = "integer",
        estimate_value = as.numeric(.data$denom_count),
        additional_name,
        additional_level
      )
    
    person_days_rows <- df %>%
      dplyr::transmute(
        result_id     = 1L,
        cdm_name      = NA_character_,
        group_name,
        group_level,
        strata_name,
        strata_level,
        variable_name = "Denominator",
        variable_level = NA_character_,
        estimate_name = "person_days",
        estimate_type = "numeric",
        estimate_value = as.numeric(.data$person_days),
        additional_name,
        additional_level
      )
    
    person_years_rows <- df %>%
      dplyr::transmute(
        result_id     = 1L,
        cdm_name      = NA_character_,
        group_name,
        group_level,
        strata_name,
        strata_level,
        variable_name = "Denominator",
        variable_level = NA_character_,
        estimate_name = "person_years",
        estimate_type = "numeric",
        estimate_value = as.numeric(.data$person_years),
        additional_name,
        additional_level
      )
    
    # Outcome rows
    outcome_count_rows <- df %>%
      dplyr::transmute(
        result_id     = 1L,
        cdm_name      = NA_character_,
        group_name,
        group_level,
        strata_name,
        strata_level,
        variable_name = "Outcome",
        variable_level = NA_character_,
        estimate_name = "outcome_count",
        estimate_type = "integer",
        estimate_value = as.numeric(.data$outcome_count),
        additional_name,
        additional_level
      )
    
    incidence_rows <- df %>%
      dplyr::transmute(
        result_id     = 1L,
        cdm_name      = NA_character_,
        group_name,
        group_level,
        strata_name,
        strata_level,
        variable_name = "Outcome",
        variable_level = NA_character_,
        estimate_name = "incidence_100000_pys",
        estimate_type = "numeric",
        estimate_value = as.numeric(.data$incidence_100000_pys),
        additional_name,
        additional_level
      )
    
    ci_lower_rows <- df %>%
      dplyr::transmute(
        result_id     = 1L,
        cdm_name      = NA_character_,
        group_name,
        group_level,
        strata_name,
        strata_level,
        variable_name = "Outcome",
        variable_level = NA_character_,
        estimate_name = "incidence_100000_pys_95CI_lower",
        estimate_type = "numeric",
        estimate_value = as.numeric(.data$incidence_100000_pys_95CI_lower),
        additional_name,
        additional_level
      )
    
    ci_upper_rows <- df %>%
      dplyr::transmute(
        result_id     = 1L,
        cdm_name      = NA_character_,
        group_name,
        group_level,
        strata_name,
        strata_level,
        variable_name = "Outcome",
        variable_level = NA_character_,
        estimate_name = "incidence_100000_pys_95CI_upper",
        estimate_type = "numeric",
        estimate_value = as.numeric(.data$incidence_100000_pys_95CI_upper),
        additional_name,
        additional_level
      )
    
    dplyr::bind_rows(
      denom_count_rows,
      person_days_rows,
      person_years_rows,
      outcome_count_rows,
      incidence_rows,
      ci_lower_rows,
      ci_upper_rows
    )
  }
  
  inc_long <- dplyr::bind_rows(
    make_long(month_df),
    make_long(year_df),
    make_long(overall_df)
  )
  
  #-------------------------------------------------------------------------------
  # 5. Add a basic 'settings' block for result_type = 'incidence'
  #-------------------------------------------------------------------------------
  settings_block <- dplyr::tibble(
    result_id       = 1L,
    cdm_name        = NA_character_,
    group_name      = "overall",
    group_level     = "overall",
    strata_name     = "overall",
    strata_level    = "overall",
    variable_name   = "settings",
    variable_level  = NA_character_,
    estimate_name   = c(
      "result_type",
      "package_name",
      "package_version",
      "group",
      "strata",
      "additional",
      "min_cell_count"
    ),
    estimate_type   = "character",
    estimate_value  = c(
      "incidence",
      "CHAPTER_ATLAS_SQL",
      as.character(utils::packageVersion("CHAPTER")),
      "denominator_cohort_name &&& outcome_cohort_name",
      NA_character_,
      "incidence_start_date &&& incidence_end_date &&& interval_name",
      as.character(minCellCount)
    ),
    additional_name  = "overall",
    additional_level = "overall"
  )
  # Make sure estimate_value has the same type as in settings_block (character)
  inc_long <- inc_long %>%
    dplyr::mutate(estimate_value = as.character(estimate_value))
  
  dplyr::bind_rows(inc_long, settings_block)
}





# -------------------------------------------------------------------------
# 6. Exporter (replaces omopgenerics::exportSummarisedResult) --------------
# -------------------------------------------------------------------------

#' @noRd
#' @keywords internal
exportResults <- function(x, minCellCount, fileName, path) {
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Simple suppression rule
  countCols <- grep("count", names(x), value = TRUE, ignore.case = TRUE)
  for (col in countCols) {
    x[[col]][x[[col]] > 0 & x[[col]] < minCellCount] <- NA
  }
  
  readr::write_csv(x, file.path(path, fileName))
}
