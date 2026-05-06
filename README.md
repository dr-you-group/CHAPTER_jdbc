CHAPTER (Characterization of Health by OHDSI Asia-Pacific chapter to identify Temporal Effect of the Pandemic)
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **Characterization**
- Study type: **Clinical Application**
- Tags: **OHDSI-AP, COVID-19**
- Study lead: **Seng Chan You**
- Study lead forums tag: **[SCYou](https://forums.ohdsi.org/u/scyou)**
- Study start date: **2021. Dec.**
- Study end date: **TBD**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**

As part of the OHDSI APAC Symposium 2012, the APAC Community selected 4 studies to push into 2022. The CHATPER study is one of them. This study will assess the incidence, prevalence, and treatment pattern of diseases or healthcare utilization during pre- and post-COVID 19 era. By this we aim to identify the temporal causality between COVID-19 and epidemiogical changes in health across OHDSI, especially APAC region.

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel).
- The current execution path is intended for Oracle and Databricks environments.
- R version 4.1.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java.

2. Open your study package in RStudio. Use the following code to install all the dependencies:

	```r
	renv::restore()
	```

3. In RStudio, select 'Build' then 'Install and Restart' to build the package.

3. Once installed, execute the study by modifying `extras/CodeToRun.R`. The main execution function expects `connectionDetails`, not an already-open `dbConnection`.

	```r
	library(CHAPTER)

	# Optional: specify where the temporary files (used by the Andromeda package) will be created:
	options(andromedaTempFolder = "")

	# Maximum number of cores to be used:
	maxCores <- parallel::detectCores() - 1

	# The folder where the study intermediate and result files will be written:
	outputFolder <- ""

	# Details for connecting to the server. For Databricks, use dbms = "spark"
	# with the Spark/Databricks JDBC driver and the connection string supported
	# by your HealthVerity environment.
	connectionDetails <- DatabaseConnector::createConnectionDetails(
	  dbms = "spark",
	  connectionString = "",
	  user = "",
	  password = "",
	  pathToDriver = ""
	)

	# Optional connection check:
	conn <- DatabaseConnector::connect(connectionDetails)
	DatabaseConnector::disconnect(conn)

	# The name of the database schema where the CDM data can be found:
	cdmDatabaseSchema <- ""

	# The name of the database schema and table where the study-specific cohorts will be instantiated:
	cohortDatabaseSchema <- ""

	# Some meta-information that will be used by the export function:
	databaseId <- ""

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
	```

4. Share the file `results-<databaseId>.zip` in the output folder with the study coordinator.


License
=======
The CHAPTER package is licensed under Apache License 2.0

Development
===========
CHAPTER package is in development

### Development status

Unknown
