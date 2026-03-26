cdmDatabaseSchema <- "CDM_IBM_CCAE_V1247.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_skeleton_ccae"


ParallelLogger::logInfo("Getting Monthly Incidence")
connection <- DatabaseConnector::connect(connectionDetails)

# startDateAnchor = '2012-01-01'
# baseCohortIds = 104
# outcomeIds = 88

##Update baseline population's start date
sql <- "UPDATE @cohort_database_schema.@cohort_table SET cohort_start_date = '@start_date_anchor' 
      WHERE cohort_start_date <= '@start_date_anchor' AND cohort_definition_id IN (@cohort_definition_ids)"
sql <- SqlRender::render(sql,
                         cohort_database_schema = cohortDatabaseSchema,
                         cohort_table = cohortTable,
                         start_date_anchor = startDateAnchor,
                         cohort_definition_ids = baseCohortIds)
sql <- SqlRender::translate(sql = sql,
                            targetDialect = attr(connection, "dbms"),
                            oracleTempSchema = oracleTempSchema)

DatabaseConnector::executeSql(connection,
                              sql)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cdmDatabaseName = 'YUHS_SC',
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortId = baseCohortIds[1],
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  outcomeIds = outcomeIds,
  cdmVersion = 5)

covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                useDemographicsAge = TRUE)

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  studyStartDate = "",
  studyEndDate = "",
  firstExposureOnly = F,
  washoutPeriod = 0,
  sampleSize = NULL)

plpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
                                            covariateSettings = covariateSettings,
                                            restrictPlpDataSettings = restrictPlpDataSettings)


PatientLevelPrediction::savePlpData(plpData, "pediatric_astma_2012")

