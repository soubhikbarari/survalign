library(testthat)
library(survalign)
library(dplyr)

# ============================================================================
# BASIC STRUCTURE AND OUTPUT TESTS
# ============================================================================

test_that("measure_alignment returns expected output structure", {
  df <- tibble::tibble(
    id = 1:3,
    group = c("A", "A", "B"),
    q1 = c(1, 2, 3),
    q2 = c(2, 2, 4)
  )
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2"),
    group_col = "group",
    id_col = "id",
    verbose = FALSE
  )
  
  expect_s3_class(res, "survalign")
  expect_s3_class(res, "list")
  expect_true(all(c("respondent_alignment", "question_pluralities", 
                    "question_cumulative_pluralities", "group_stats") %in% names(res)))
})

test_that("measure_alignment returns group_stats with correct structure", {
  df <- tibble::tibble(
    id = 1:4,
    group = rep("A", 4),
    q1 = c(1, 1, 1, 2),
    q2 = c(1, 1, 2, 2)
  )
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2"),
    group_col = "group",
    id_col = "id",
    verbose = FALSE
  )
  
  group_stats <- res$group_stats
  expect_equal(nrow(group_stats), 1) # one group
  expect_true(all(c("alignment_mean", "alignment_median", "cumulative_perfect_alignment", 
                    "cumulative_weak_alignment", "n_respondents") %in% names(group_stats)))
  expect_true(all(group_stats$alignment_mean >= 0 & group_stats$alignment_mean <= 1))
  expect_true(all(group_stats$cumulative_perfect_alignment >= 0 & group_stats$cumulative_perfect_alignment <= 1))
})

test_that("measure_alignment handles missing values with treat_na='exclude'", {
  df <- tibble::tibble(
    id = 1:4,
    group = rep("A", 4),
    q1 = c(1, 1, NA, 2),
    q2 = c(1, 1, 2, 2)
  )
  expect_no_error(
    measure_alignment(df, ques_cols = c("q1", "q2"), group_col = "group",
                      id_col = "id", treat_na = "exclude", verbose = FALSE)
  )
})

test_that("measure_alignment handles empty data gracefully", {
  df <- tibble::tibble(
    id = integer(),
    group = character(),
    q1 = double(),
    q2 = double()
  )
  expect_error(
    measure_alignment(
      data = df,
      ques_cols = c("q1", "q2"),
      group_col = "group",
      id_col = "id",
      verbose = FALSE
    )
  )
})

# ============================================================================
# NEAR-PERFECT ALIGNMENT SCENARIO: STRONG ALIGNMENT WITH MINIMAL VARIATION
# ============================================================================

test_that("near-perfect alignment: almost all respondents answer identically", {
  # 4 respondents, 1 group
  # Q1-Q3: all answer the same
  # Q4: 3 same, 1 different (to satisfy variation requirement)
  df <- tibble::tibble(
    id = 1:4,
    group = rep("All", 4),
    q1 = c(1, 1, 1, 1),
    q2 = c(2, 2, 2, 2),
    q3 = c(1, 1, 1, 1),
    q4 = c(2, 2, 2, 1)  # one respondent differs
  )
  
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2", "q3", "q4"),
    group_col = "group",
    id_col = "id",
    verbose = FALSE
  )
  
  # 3/4 respondents are perfectly aligned
  expect_equal(res$group_stats$cumulative_perfect_alignment[[1]], 0.75)
  # Average alignment should be high (most questions respondent matches plurality)
  expect_true(res$group_stats$alignment_mean[[1]] >= 0.8)
  
  # Cumulative plurality should be 100% for first 3 questions, then drop
  cum_props <- res$question_cumulative_pluralities$prop_cumulative_plurality
  expect_equal(cum_props[1], 1.0)
  expect_equal(cum_props[2], 1.0)
  expect_equal(cum_props[3], 1.0)
  expect_equal(cum_props[4], 0.75)
})

# ============================================================================
# CONTROLLED DECAY SCENARIO: PREDICTABLE DROP IN CUMULATIVE AGREEMENT
# ============================================================================

test_that("controlled decay: cumulative agreement drops predictably (25% per question)", {
  # Design: 4 respondents, 4 questions
  # Q1: all agree on 1 (100% plurality)
  # Q2: all agree on 2 (100% plurality)
  # Q3: 3 agree on 1, 1 disagrees (75% plurality)
  # Q4: 2 agree on 1, 2 disagree (50% plurality)
  
  df <- tibble::tibble(
    id = c(1, 2, 3, 4),
    group = rep("All", 4),
    q1 = c(1, 1, 1, 1),      # 4/4 aligned (100%)
    q2 = c(2, 2, 2, 2),      # 4/4 aligned (100%)
    q3 = c(1, 1, 1, 2),      # 3/4 aligned (75%)
    q4 = c(1, 1, 2, 2)       # 2/4 aligned (50%)
  )
  
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2", "q3", "q4"),
    group_col = "group",
    id_col = "id",
    verbose = FALSE
  )
  
  cum_props <- res$question_cumulative_pluralities$prop_cumulative_plurality
  
  # Cumulative proportions should decay
  expect_equal(cum_props[1], 1.0)      # Everyone aligned on Q1
  expect_equal(cum_props[2], 1.0)      # Everyone aligned on Q1 AND Q2
  expect_equal(cum_props[3], 0.75)     # 3/4 aligned on Q1, Q2, and Q3
  expect_equal(cum_props[4], 0.5)      # 2/4 aligned on all four
})

# ============================================================================
# BINARY SPLIT SCENARIO: TWO SUBGROUPS WITH OPPOSITE VIEWS
# ============================================================================

test_that("binary split: two respondents perfectly aligned, two perfectly split", {
  # Respondent 1&2: always answer 1
  # Respondent 3&4: always answer 2
  df <- tibble::tibble(
    id = c(1, 2, 3, 4),
    group = rep("All", 4),
    q1 = c(1, 1, 2, 2),
    q2 = c(1, 1, 2, 2),
    q3 = c(1, 1, 2, 2),
    q4 = c(1, 1, 2, 2)
  )
  
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2", "q3", "q4"),
    group_col = "group",
    id_col = "id",
    verbose = FALSE
  )
  
  # Half the group is perfectly aligned, half is not aligned at all
  expect_equal(res$group_stats$cumulative_perfect_alignment[[1]], 0.5)
  expect_equal(res$group_stats$alignment_mean[[1]], 0.5)
})

# ============================================================================
# MAJORITY PLURALITY SCENARIO: MAJORITY SHIFTS BETWEEN QUESTIONS
# ============================================================================

test_that("shifting majority: plurality opinion changes across questions", {
  # Q1: 3 say 1, 1 says 2 (plurality = 1)
  # Q2: 2 say 1, 2 say 2 (plurality = 1 or 2, depending on tie-breaking)
  # Q3: 1 says 1, 3 say 2 (plurality = 2)
  # Q4: all say 1 (plurality = 1)
  
  df <- tibble::tibble(
    id = c(1, 2, 3, 4),
    group = rep("All", 4),
    q1 = c(1, 1, 1, 2),
    q2 = c(1, 1, 2, 2),
    q3 = c(1, 2, 2, 2),
    q4 = c(1, 1, 1, 1)
  )
  
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2", "q3", "q4"),
    group_col = "group",
    id_col = "id",
    verbose = FALSE
  )
  
  pluralities <- res$question_pluralities$plurality_response
  
  # Check that pluralities are sensible (most common responses)
  expect_equal(length(pluralities), 4)
  expect_true(all(!is.na(pluralities)))
})

# ============================================================================
# WEIGHTED DATA SCENARIO
# ============================================================================

test_that("weighted data: weights affect plurality calculation", {
  # Two respondents with opposite answers, but second is weighted heavily
  df <- tibble::tibble(
    id = c(1, 2, 3, 4, 5),
    group = rep("All", 5),
    weight = c(1, 10, 1, 10, 10),  # respondent 2 and 4 are weighted 10x
    q1 = c(1, 2, 1, 2, 2),
    q2 = c(1, 2, 1, 2, 2),
    q3 = c(1, 2, 1, 2, 2),
    q4 = c(1, 2, 1, 2, 2)
  )
  
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2", "q3", "q4"),
    group_col = "group",
    weight_col = "weight",
    id_col = "id",
    verbose = FALSE
  )
  
  # The response 2 should be plurality for all questions
  pluralities <- res$question_pluralities$plurality_response
  expect_true(all(pluralities == 2))
  
  # Only respondents 2, 4, 5 should be aligned (weighted 10 each)
  # Respondents 1 and 3 unaligned (weighted 1 each)
  # Expected: (10 + 10 + 10) / (1 + 10 + 1 + 10 + 10) = 20/22 = 0.9375
  
  exp_align_mean <- with(df, weighted.mean(q1==2 & q2==2 & q3==2 & q4==2, w=weight))
  
  expect_true(res$group_stats$alignment_mean[[1]] == exp_align_mean)
})

# ============================================================================
# NA HANDLING SCENARIOS
# ============================================================================

test_that("treat_na='exclude': NAs are dropped from calculations", {
  # Respondent 1: all questions answered
  # Respondent 2: all questions answered
  # Respondent 3: has NAs scattered throughout
  df <- tibble::tibble(
    id = c(1, 2, 3),
    group = rep("All", 3),
    q1 = c(1, 2, NA),
    q2 = c(1, 2, 1),
    q3 = c(1, 2, NA),
    q4 = c(1, 2, 1)
  )
  
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2", "q3", "q4"),
    group_col = "group",
    id_col = "id",
    treat_na = "exclude",
    verbose = FALSE
  )
  
  # Respondent 3 should have reduced effective questions
  resp_align <- res$respondent_alignment
  expect_equal(nrow(resp_align), 3)
  
  # With treat_na='exclude', mean should only count non-NA responses
  r3_alignment <- resp_align %>% dplyr::filter(id == 3)
  expect_true(!is.na(r3_alignment$prop_questions_aligned))
})

test_that("treat_na='unaligned': NAs are treated as not aligned", {
  df <- tibble::tibble(
    id = c(1, 2, 3),
    group = rep("All", 3),
    q1 = c(1, NA, NA),
    q2 = c(1, 2, 2)
  )
  
  res <- measure_alignment(
    data = df,
    ques_cols = c("q1", "q2"),
    group_col = "group",
    id_col = "id",
    treat_na = "unaligned",
    verbose = FALSE
  )
  
  # Respondent 2 has one aligned (q2) and one unaligned (q1=NA)
  resp_align <- res$respondent_alignment %>% 
    dplyr::filter(id == 2)
  expect_equal(resp_align$prop_questions_aligned, 0.5)
})
