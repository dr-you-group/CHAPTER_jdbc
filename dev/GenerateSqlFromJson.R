# dev/GenerateSqlFromJson.R
# ------------------------------------------------------------------
# Generate .sql files from ATLAS cohort expression JSONs
# for the CHAPTER package.
#
# Run this script from the *project root* of CHAPTER, e.g.:
#   setwd("C:/Users/paul9/Rprojects/CHAPTER")
#   source("dev/GenerateSqlFromJson.R")
# ------------------------------------------------------------------

# 1. Load required packages ----------------------------------------

if (!requireNamespace("CirceR", quietly = TRUE)) {
  stop("Please install CirceR first, e.g. remotes::install_github('OHDSI/CirceR')")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Please install jsonlite first: install.packages('jsonlite')")
}

library(CirceR)
library(jsonlite)

# 2. Root folder where JSONs live ----------------------------------

# This should point to the *source* package folder, not the installed library.
# So run this script from the CHAPTER project root, where 'inst/' exists.
rootFolder <- file.path("inst", "cohorts_final")

if (!dir.exists(rootFolder)) {
  stop("Cannot find 'inst/cohorts_final'. ",
       "Make sure your working directory is the CHAPTER project root.")
}

message("Using rootFolder = ", normalizePath(rootFolder, winslash = "/"))


# 3. Helper: generate SQL for a single JSON file -------------------

generateSqlForJson <- function(jsonFile, overwrite = FALSE) {
  jsonFile <- normalizePath(jsonFile, winslash = "/")
  sqlFile  <- sub("\\.json$", ".sql", jsonFile)
  
  if (!overwrite && file.exists(sqlFile)) {
    message("  - Skipping (SQL already exists): ", basename(sqlFile))
    return(invisible(TRUE))
  }
  
  message("  - Generating SQL for: ", basename(jsonFile))
  
  # Read the JSON as text
  jsonText <- readChar(jsonFile, file.info(jsonFile)$size)
  
  # Our JSON is the expression object itself
  expr <- CirceR::cohortExpressionFromJson(jsonText)
  
  # ✅ Correct call: argument name is 'expression', not 'cohortExpression'
  sql <- CirceR::buildCohortQuery(
    expression = expr,
    options    = CirceR::createGenerateOptions(generateStats = FALSE)
  )
  
  # Write out the SQL next to the JSON
  writeLines(sql, con = sqlFile)
  
  message("    -> Wrote: ", basename(sqlFile))
  invisible(TRUE)
}


# 4. Walk all subfolders and JSON files -----------------------------

subFolders <- list.dirs(rootFolder, full.names = TRUE, recursive = FALSE)

if (length(subFolders) == 0) {
  stop("No subfolders found under ", rootFolder,
       ". Expected folders like 'AllergyCohorts', 'CancerCohorts', etc.")
}

for (sub in subFolders) {
  message("Processing folder: ", normalizePath(sub, winslash = "/"))
  
  jsonFiles <- list.files(sub, pattern = "\\.json$", full.names = TRUE)
  if (length(jsonFiles) == 0) {
    message("  (No .json files found here)")
    next
  }
  
  for (jf in jsonFiles) {
    tryCatch(
      {
        generateSqlForJson(jf, overwrite = FALSE)
      },
      error = function(e) {
        message("  !! Error generating SQL for ", basename(jf), ": ", conditionMessage(e))
      }
    )
  }
  
  message("")  # blank line for readability
}

message("Done. Check each subfolder for newly created .sql files.")
