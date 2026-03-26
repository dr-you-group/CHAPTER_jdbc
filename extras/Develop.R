# devtools::install_github("ohdsi/Eunomia")

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

cdmDatabaseSchema <- "main"
cohortDatabaseSchema <- "main"
cohortTable <- "cohort"
targetCohortId <- 1
outcomeCohortId <- 3
broadSampleSize <- NULL
limitedSampleSize <- 50

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails,
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cdmDatabaseName = "Enumonia",
                                                                 tempEmulationSchema = NULL,
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 cohortTable = cohortTable,
                                                                 outcomeDatabaseSchema = cohortDatabaseSchema,
                                                                 outcomeTable = cohortTable,
                                                                 cohortId = targetCohortId,
                                                                 outcomeIds = outcomeCohortId,
                                                                 cdmVersion = 5)

# covSet <- createCohortCovariateSettings(
#     cohortName = 'covariateName',
#     settingId = 4,
#     cohortDatabaseSchema = 'main',
#     cohortTable = 'cohort',
#     cohortId = 2,
#     startDay = -350,
#     endDay = -2,
#     count = F,
#     ageInteraction = F,
#     lnAgeInteraction = F,
#     analysisId = 456
# )

demographCovSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = T,
    useDemographicsAge = T,
    useDemographicsAgeGroup = T
    )

limitedCovSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = T,
    useDemographicsAge = T,
    useDemographicsAgeGroup = T,
    useConditionEraLongTerm = T,
    longTermStartDays = -365,
    endDays = 0)

broadPlpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
                                              covariateSettings = demographCovSettings,
                                              restrictPlpDataSettings =
                                                  PatientLevelPrediction::createRestrictPlpDataSettings(sampleSize = broadSampleSize)
                                              )

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(sampleSize = limitedSampleSize)
restrictedPlpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
                                                        covariateSettings = limitedCovSettings,
                                                        restrictPlpDataSettings = restrictPlpDataSettings)

broadPlpData$cohorts
broadPlpData$outcomes

broadPlpData$


braodPlpData$cohorts
restrictedPlpData$cohorts


braodPlpData$outcomes

restrictedPlpData$cohorts
restrictedPlpData$outcomes

