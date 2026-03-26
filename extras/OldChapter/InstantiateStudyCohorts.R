# Instantiate initial cohorts

info(logger, "- getting initial cohort definitions")

cohorts_acute <- CDMConnector::readCohortSet(
  here::here("1_InitialCohorts","Jsons_acute_diagnosis"))
#cohorts_acute_care <- CDMConnector::readCohortSet(
#  here::here("1_InitialCohorts","Jsons_acute_care"))
#cohorts_acute_prognosis <- CDMConnector::readCohortSet(
#  here::here("1_InitialCohorts","Jsons_acute_prognosis"))

cohorts_chronic <- CDMConnector::readCohortSet(
  here::here("1_InitialCohorts","Jsons_chronic_diagnosis"))
#cohorts_chronic_care <- CDMConnector::readCohortSet(
#  here::here("1_InitialCohorts","Jsons_chronic_care"))
#cohorts_chronic_prognosis <- CDMConnector::readCohortSet(
#  here::here("1_InitialCohorts","Jsons_chronic_prognosis"))

cohorts_long_covid <- CDMConnector::readCohortSet(
  here::here("1_InitialCohorts","Jsons_long_covid"))
cohorts_pasc <- CDMConnector::readCohortSet(
  here::here("1_InitialCohorts","Jsons_pasc"))

cohorts_hu <- CDMConnector::readCohortSet(
  here::here("1_InitialCohorts","Jsons_hu"))

info(logger, "- instantiating initial cohorts")

cdm <- CDMConnector::generateCohortSet(cdm, cohorts_acute,
                                       name = AcuteCohortsName,
                                       overwrite = TRUE)
#cdm <- CDMConnector::generateCohortSet(cdm, cohorts_acute_care,
#                                       name = AcuteCohortsCareName,
#                                       overwrite = TRUE)
#cdm <- CDMConnector::generateCohortSet(cdm, cohorts_acute_prognosis,
#                                       name = AcuteCohortsPrognosisName,
#                                       overwrite = TRUE)

cdm <- CDMConnector::generateCohortSet(cdm, cohorts_chronic,
                                       name = ChronicCohortsName,
                                       overwrite = TRUE)
#cdm <- CDMConnector::generateCohortSet(cdm, cohorts_chronic_care,
#                                       name = ChronicCohortsCareName,
#                                       overwrite = TRUE)
#cdm <- CDMConnector::generateCohortSet(cdm, cohorts_chronic_prognosis,
#                                       name = ChronicCohortsPrognosisName,
#                                       overwrite = TRUE)

cdm <- CDMConnector::generateCohortSet(cdm, cohorts_long_covid,
                                       name = LongCovidCohortsName,
                                       overwrite = TRUE)
cdm <- CDMConnector::generateCohortSet(cdm, cohorts_pasc,
                                       name = PascCohortsName,
                                       overwrite = TRUE)

cdm <- CDMConnector::generateCohortSet(cdm, cohorts_hu,
                                       name = HUCohortsName,
                                       overwrite = TRUE)

info(logger, "- got initial cohorts")
