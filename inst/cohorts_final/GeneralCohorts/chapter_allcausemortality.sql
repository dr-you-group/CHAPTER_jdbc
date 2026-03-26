-- Customized large-DB-friendly version for chapter_allcausemortality.sql
-- Semantics preserved:
--   1) death date within observation period
--   2) first qualifying death per person
--   3) cohort end = start_date (same-day cohort)

DELETE FROM @target_database_schema.@target_cohort_table
WHERE cohort_definition_id = @target_cohort_id
;

INSERT INTO @target_database_schema.@target_cohort_table
(
  cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
)
WITH candidate_deaths AS (
  SELECT
    d.person_id,
    d.death_date AS cohort_start_date,
    op.observation_period_end_date,
    ROW_NUMBER() OVER (
      PARTITION BY d.person_id
      ORDER BY d.death_date ASC, d.person_id ASC
    ) AS rn
  FROM @cdm_database_schema.DEATH d
  JOIN @cdm_database_schema.OBSERVATION_PERIOD op
    ON d.person_id = op.person_id
   AND d.death_date >= op.observation_period_start_date
   AND d.death_date <= op.observation_period_end_date
)
SELECT
  @target_cohort_id AS cohort_definition_id,
  person_id         AS subject_id,
  cohort_start_date,
  CASE
    WHEN cohort_start_date > observation_period_end_date
      THEN observation_period_end_date
    ELSE cohort_start_date
  END AS cohort_end_date
FROM candidate_deaths
WHERE rn = 1
;