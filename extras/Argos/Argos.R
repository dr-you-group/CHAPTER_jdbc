# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' Execute the Argos
#'
#' @details
#' This function executes the cohort diagnostics.
#'
#' @param connectionDetails                   An object of type \code{connectionDetails} as created
#'                                            using the
#'                                            \code{\link[DatabaseConnector]{createConnectionDetails}}
#'                                            function in the DatabaseConnector package.
#' @param cdmDatabaseSchema                   Schema name where your patient-level data in OMOP CDM
#'                                            format resides. Note that for SQL Server, this should
#'                                            include both the database and schema name, for example
#'                                            'cdm_data.dbo'.
#' @param cohortDatabaseSchema                Schema name where intermediate data can be stored. You
#'                                            will need to have write privileges in this schema. Note
#'                                            that for SQL Server, this should include both the
#'                                            database and schema name, for example 'cdm_data.dbo'.
#' @param vocabularyDatabaseSchema            Schema name where your OMOP vocabulary data resides. This
#'                                            is commonly the same as cdmDatabaseSchema. Note that for
#'                                            SQL Server, this should include both the database and
#'                                            schema name, for example 'vocabulary.dbo'.
#' @param cohortTable                         The name of the table that will be created in the work
#'                                            database schema. This table will hold the exposure and
#'                                            outcome cohorts used in this study.
#' @param tempEmulationSchema                 Some database platforms like Oracle and Impala do not
#'                                            truly support temp tables. To emulate temp tables,
#'                                            provide a schema with write privileges where temp tables
#'                                            can be created.
#' @param outputFolder                        Name of local folder to place results; make sure to use
#'                                            forward slashes (/). Do not use a folder on a network
#'                                            drive since this greatly impacts performance.
#' @export
executeArgos <- function(connectionDetails,
                         cdmDatabaseSchema,
                         vocabularyDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cdmDatabaseSchema,
                         cohortTable = "cohort",
                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                         verifyDependencies = TRUE,
                         outputFolder){
    if (!file.exists(outputFolder)) {
        dir.create(outputFolder, recursive = TRUE)
    }

    ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
    ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))

    on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
    on.exit(
        ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE),
        add = TRUE
    )

    ParallelLogger::logInfo("Get incidence data")

    ####Get incidence data####
    pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "CHAPTER")
    cohortsToCreate <- read.csv(pathToCsv, stringsAsFactors = FALSE)

    ##create setting for covariates
    covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                    useDemographicsAge = TRUE
    )
    for (i in seq(cancerList$cohortId)){

    }
    incidenceData <- Argos::getIncidenceData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = cancerList$cohortId[i],
                                             minDateUnit = "year")


}


#i<-1
for (i in seq(cancerList$cohortId)){
    ##get incidence Data
    incidenceData <- Argos::getIncidenceData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = cancerList$cohortId[i],
                                             minDateUnit = "year")
    saveRDS(incidenceData,file.path(outputFolder,paste0("incidenceData_cohortId_",cancerList$cohortId[i], ".rds" )))

    ##calculate the age adjusted incidence rate
    incCal<-calculateIncidence(incidenceData = incidenceData,
                               basePopulation = basePop,
                               refPopulation = refPop,
                               standardization = "direct",
                               Agestandardization = TRUE,
                               genderStandardization = TRUE,
                               startYearStandardization = TRUE,
                               AgeSet = list(0:4,5:9,10:14,15:19,20:24,25:29,30:34,35:39,40:44,
                                             45:49,50:54,55:59,60:64,65:69,70:74,75:79,80:84,85:100),
                               genderSet = list(8507,8532),
                               startYearSet = startYearSet,
                               birthYearSet = list(1910:2005))

    ageSpecified<-agespe(incCal)
    ageadjInc<-ageadjust(ageSpecified, alpha = 0.05)

    write.csv(ageadjInc, file.path(outputFolder, paste0("ageadjustedInc_cohortId_", cancerList$cohortId[[i]], ".csv")))
    saveRDS(ageadjInc,file.path(outputFolder,paste0("ageadjustedInc_cohortId_",cancerList$cohortId[i], ".rds" )))

    ##calculate the age specified incidence rate
    incCal<-Argos::calculateIncidence(incidenceData = incidenceData,
                                      basePopulation = basePop,
                                      refPopulation = refPop,
                                      standardization = "direct",
                                      Agestandardization = TRUE,
                                      genderStandardization = TRUE,
                                      startYearStandardization = TRUE,
                                      AgeSet = list(30:39,
                                                    40:49,
                                                    50:59,
                                                    60:69,
                                                    70:79,
                                                    80:99),
                                      genderSet = list(8507,8532),
                                      startYearSet = startYearSet,
                                      birthYearSet = list(1910:1919, 1920:1929,
                                                          1930:1939, 1940:1949,
                                                          1950:1959, 1960:1964,
                                                          1965:1969, 1970:1974,
                                                          1975:1979, 1980:1989))

    ageSpecifiedIncData <- agespe(incidencePropdata = incCal)
    birthcohortIncData<-bybirth(incidencePropdata = incCal)

    saveRDS(ageSpecifiedIncData,file.path(outputFolder,paste0("ageSpecifiedIncData_cohortId_",cancerList$cohortId[i], ".rds" )))
    write.csv(ageSpecifiedIncData,file.path(outputFolder,paste0("ageSpecifiedIncData_cohortId_",cancerList$cohortId[i], ".csv" )))
    saveRDS(birthcohortIncData,file.path(outputFolder,paste0("birthcohortIncData_cohortId_",cancerList$cohortId[i], ".rds" )))
    write.csv(birthcohortIncData,file.path(outputFolder,paste0("birthcohortIncData_cohortId_",cancerList$cohortId[i], ".csv" )))

    bybirthPlot<-PlotByBirthInc(birthcohortIncData = birthcohortIncData)
    ageSpePlot<-PlotByDiagnosisIncAgeS(agespecifiedIncData = ageSpecifiedIncData)
    ageAdjPlot<-PlotByDiagnosisIncAgeAd(ageadjustIncData = ageadjInc)
    Argos::saveIncidence(outputFolder,
                         bybirthPlot,
                         ageSpePlot,
                         ageAdjPlot,
                         imageExtension = "png")
}
