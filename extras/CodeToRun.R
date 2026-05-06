# Check library path and required packages. Run renv::restore() for downloading all needed packages.
.libPaths("")
# renv::status()
# renv::snapshot()
# renv::deactivate()




# Press "build", then proceed.
library(CHAPTER)


# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores() -1

# The folder where the study intermediate and result files will be written:
outputFolder <- ""


# Details for connecting to the server.
# For Oracle, use dbms = "oracle".
# For HealthVerity Databricks, use dbms = "spark" and the JDBC connection
# string/driver path provided by the data partner environment.
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = "",
  user = "",
  password = "",
  pathToDriver = "" 
)
# Check connection
conn <- DatabaseConnector::connect(connectionDetails) 
DatabaseConnector::disconnect(conn)




# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- ""

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- ""
cohortTable <- ""

# Some meta-information that will be used by the export function:
databaseId <- ""
databaseName <- ""
databaseDescription <- ""

# For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
options(sqlRenderTempEmulationSchema = NULL)

CHAPTER::executeIncidencePrevalenceFinal(
  connectionDetails,
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
  minCellCount = 5
)

# CohortDiagnostics::preMergeDiagnosticsFiles(dataFolder = outputFolder)
# CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = outputFolder)


