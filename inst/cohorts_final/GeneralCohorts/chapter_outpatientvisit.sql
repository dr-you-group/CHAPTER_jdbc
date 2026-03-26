-- Customized large-DB-friendly version for chapter_outpatientvisit.sql
-- Semantics preserved:
--   1) outpatient visit concept 9202 + descendants
--   2) at least 365 days prior observation
--   3) first qualifying event per person
--   4) cohort end = start_date + 1 day, capped at observation_period_end_date

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
WITH outpatient_codes AS (
  SELECT c.concept_id
  FROM @vocabulary_database_schema.CONCEPT c
  WHERE c.concept_id = 9202

  UNION

  SELECT ca.descendant_concept_id AS concept_id
  FROM @vocabulary_database_schema.CONCEPT_ANCESTOR ca
  JOIN @vocabulary_database_schema.CONCEPT c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id = 9202
    AND c.invalid_reason IS NULL
),
candidate_visits AS (
  SELECT
    vo.person_id,
    vo.visit_occurrence_id,
    vo.visit_start_date AS cohort_start_date,
    op.observation_period_end_date,
    ROW_NUMBER() OVER (
      PARTITION BY vo.person_id
      ORDER BY vo.visit_start_date ASC, vo.visit_occurrence_id ASC
    ) AS rn
  FROM @cdm_database_schema.VISIT_OCCURRENCE vo
  JOIN outpatient_codes oc
    ON vo.visit_concept_id = oc.concept_id
  JOIN @cdm_database_schema.OBSERVATION_PERIOD op
    ON vo.person_id = op.person_id
   AND vo.visit_start_date >= op.observation_period_start_date
   AND vo.visit_start_date <= op.observation_period_end_date
  WHERE DATEADD(day, 365, op.observation_period_start_date) <= vo.visit_start_date
)
SELECT
  @target_cohort_id AS cohort_definition_id,
  person_id         AS subject_id,
  cohort_start_date,
  CASE
    WHEN DATEADD(day, 1, cohort_start_date) > observation_period_end_date
      THEN observation_period_end_date
    ELSE DATEADD(day, 1, cohort_start_date)
  END AS cohort_end_date
FROM candidate_visits
WHERE rn = 1
;