library(tibble)
library(testthat)

build_test_data <- function() {
  tibble(
    year = c(2000, 2000, 2001, 2001),
    pid3 = c("A", "A", "A", "B"),
    weight = c(1, 2, 1, 1),
    q1 = c(1, 0, 1, 1),
    q2 = c(0, 1, 1, 0)
  )
}

test_that("package data objects load", {
  data("gss")
  data("ces")
  expect_true(exists("gss"))
  expect_true(exists("ces"))
})

data(gss)
data(ces)

test_that("plot_coverage resolves columns via ques_stem", {
  data <- build_test_data()
  p <- plot_coverage(data, wave_col = "year", ques_stem = "q")
  expect_s3_class(p, "ggplot")
})

test_that("plot_support returns a ggplot", {
  data <- build_test_data()
  p <- plot_support(
    data,
    wave_col = "year",
    weight_col = "weight",
    group_col = "pid3",
    ques_cols = c("q1", "q2")
  )
  expect_s3_class(p, "ggplot")
})

test_that("binarize_responses returns 0/1 values", {
  data <- build_test_data()
  out <- binarize_responses(data, ques_cols = c("q1", "q2"), threshold = 0.5)
  expect_true(all(out$q1 %in% c(0, 1, NA)))
  expect_true(all(out$q2 %in% c(0, 1, NA)))
})

test_that("binarize_responses returns 0/1 values on GSS data (factors)", {
  out <- binarize_responses(gss, ques_cols = c("natarms"), threshold = 0.3)
  expect_true(all(out$natarms %in% c(0, 1, NA)))
  expect_true(mean(out$natarms, na.rm=T) > 0.66)
  
  out <- binarize_responses(gss, ques_cols = c("natarms"), threshold = 0.6)
  expect_true(mean(out$natarms, na.rm=T) < 0.33)
})

test_that("measure_pairwise_support computes weighted co-support", {
  data <- build_test_data()
  out <- measure_pairwise_support(
    data,
    ques_cols = c("q1", "q2"),
    weight_col = "weight",
    include_self = FALSE,
    treat_na = "exclude"
  )
  expect_equal(nrow(out), 1)
  expect_equal(out$percent_q1_support, weighted.mean(data$q1, data$weight))
  expect_equal(out$percent_q2_support, weighted.mean(data$q2, data$weight))
  expect_equal(out$percent_both_support[[1]], weighted.mean(data$q1 & data$q2, data$weight))
})

test_that("measure_pairwise_support computes weighted co-support on CES data", {
  qs <- c("enviro_carbon", "abortion_always")
  
  ces_in <- ces %>%
    filter(!is.na(ces[[qs[1]]]) & !is.na(ces[[qs[2]]])) %>%
    filter(year == 2020)
  
  p1_true <- weighted.mean(ces_in[[qs[1]]]=="Support", ces_in$weight, na.rm=T)
  p2_true <- weighted.mean(ces_in[[qs[2]]]=="Support", ces_in$weight, na.rm=T)
  pboth_true <- weighted.mean(ces_in[[qs[1]]]=="Support" & ces_in[[qs[2]]]=="Support", ces_in$weight, na.rm=T) 
  
  ces_out <- measure_pairwise_support(
    ces_in,
    ques_cols = qs,
    weight_col = "weight",
    include_self = FALSE,
    treat_na = "exclude"
  )
  #table(ces[[qs[1]]], ces[[qs[2]]]) |> prop.table()
  expect_equal(nrow(ces_out), 1)
  expect_equal(ces_out$percent_q1_support, p1_true, 
               tolerance = 0.001)
  expect_equal(ces_out$percent_q2_support, p2_true, 
               tolerance = 0.001)
  expect_equal(ces_out$percent_both_support[[1]], pboth_true,
               tolerance = 0.001)
})
