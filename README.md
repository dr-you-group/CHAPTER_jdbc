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

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, or Microsoft APS.
- R version 4.0.0 or newer
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

3. Once installed, you can execute the study by modifying and using the code below. For your convenience, this code is also provided under `extras/CodeToRun.R`:

	```r
	library(CHAPTER)

	# Optional: specify where the temporary files (used by the Andromeda package) will be created:
	options(andromedaTempFolder = "s:/andromedaTemp")

	# Maximum number of cores to be used:
	maxCores <- parallel::detectCores()

	# The folder where the study intermediate and result files will be written:
	outputFolder <- "c:/CHAPTER"

	# Details for connecting to the server
        db <- DBI::dbConnect(odbc::odbc(),
                             Driver   = "ODBC Driver 18 for SQL Server",
                             Server   = "10.19.10.241",
                             Database = "Synpuf",
                             UID      = "******",
                             PWD = "******",
                             TrustServerCertificate = "yes",
                             Port     = 1433)

	# The name of the database schema where the CDM data can be found:
	cdmDatabaseSchema <- "cdm_synpuf"

	# The name of the database schema and table where the study-specific cohorts will be instantiated:
	cohortDatabaseSchema <- "scratch.dbo"
	cohortTable <- "my_study_cohorts"

	# Some meta-information that will be used by the export function:
	databaseId <- "Synpuf"
	databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
	databaseDescription <- "Medicare Claims Synthetic Public Use Files (SynPUFs) were created to allow interested parties to gain familiarity using Medicare claims data while protecting beneficiary privacy. These files are intended to promote development of software and applications that utilize files in this format, train researchers on the use and complexities of Centers for Medicare and Medicaid Services (CMS) claims, and support safe data mining innovations. The SynPUFs were created by combining randomized information from multiple unique beneficiaries and changing variable values. This randomization and combining of beneficiary information ensures privacy of health information."

	# For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
	options(sqlRenderTempEmulationSchema = NULL)

        CHAPTER::executeIncidencePrevalenceFinal(dbConnection = db,
                                                 cdmDatabaseSchema,
                                                 cohortDatabaseSchema,
                                                 writePrefix = NULL,
                                                 outputFolder,
                                                 databaseId,
                                                 readCohorts = TRUE,
                                                 minCellCount = 5
                                                 )
	```

4. Share the file ```export/Results_<DatabaseId>.zip``` in the output folder to the study coordinator


License
=======
The CHAPTER package is licensed under Apache License 2.0

Development
===========
CHAPTER package is in development

### Development status

Unknown
