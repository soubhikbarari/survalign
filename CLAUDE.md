# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`survalign` is an R package for measuring within-group alignment in survey data — how unified a group's members are across a *basket of issues* simultaneously, not just one at a time. The core insight is that groups may show high per-issue support but low cross-issue co-alignment.

## Common Commands

```r
# Install dependencies (uses renv for lockfile reproducibility)
renv::restore()

# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-align.R")

# Run tests matching a pattern
devtools::test(filter = "weighted")

# Regenerate documentation from roxygen2 comments
devtools::document()

# Build and check the package
devtools::check()

# Build pkgdown site
pkgdown::build_site()
```

## Package Architecture

### Source files (`R/`)

- **`funcs-align.R`** — Core exported functions:
  - `measure_alignment()` — Main function. Takes a data frame, question columns, and group column; returns a `survalign` S3 object (a list) with slots: `respondent_alignment`, `question_plurality_opinions`, `question_pluralities`, `question_cumulative_pluralities`, `group_stats`, `question_labels`. Metadata stored as attributes (`group_col`, `weight_col`, `id_col`, `treat_na`, `group_label`).
  - `measure_alignment_waves()` — Wraps `measure_alignment()` in a loop over wave values (e.g., years), returning a named `survalign.waves` list.
  - `print.survalign()` / `summary.survalign()` — S3 methods for the output class.

- **`funcs-plot.R`** — All plotting functions (all return `ggplot` objects, except `plot_alignment_curve(interactive=TRUE)` which returns a `plotly` widget):
  - `plot_cumulative_support()` — Bar chart of per-item and cumulative plurality support, faceted by group.
  - `plot_individual_alignments()` — Histogram/density of per-respondent alignment scores by group.
  - `plot_alignment_curve()` — CDF-style curve: x = alignment threshold, y = share of respondents meeting it.
  - `plot_group_stat_over_time()` — Line plot of a group_stats metric over waves.
  - `plot_coverage()` — Heatmap of item missingness by wave.
  - `plot_support()` — Heatmap or bar chart of item support rates by group/wave.
  - `plot_pairwise_support()` — Heatmap of pairwise co-support.

- **`utils.R`** — Internal helpers plus a few exported functions:
  - `binarize_responses()` — Converts Likert-scale columns to 0/1 using a threshold.
  - `measure_pairwise_support()` — Computes pairwise co-support statistics.
  - `resolve_ques()` (internal) — Resolves question columns from `ques_cols` or `ques_stem` regex.
  - `weighted_se()` (internal) — Design-weighted SE using Kish effective sample size.
  - `dump_labels()` (internal) — Extracts Haven/labelled column labels.

- **`data.R`** — Documentation for bundled datasets (`ces` and `gss`).

### Data

- `data/ces.rds` — Bundled CES survey data used in examples and tests. Columns include `year`, `id`, `pid3`, `weight`, and policy question columns matched by stems like `(abort|immig|enviro|guns|military|trade)`.
- `data/gss.rds` — Bundled GSS survey data.

### Tests (`tests/testthat/`)

- `test-align.R` — Tests for `measure_alignment()`: structure, numerical correctness, NA handling, weighting, tie-breaking, edge cases.
- `test-utils.R` — Tests for plotting helpers, `binarize_responses()`, `measure_pairwise_support()`, and data loading.

### Paper

This is where an academic paper is being written around the concepts, the measurement 
framework and the empirical results.

The paper is meant for a social science, primarily political science, but also 
public opinion / social psychology / survey methodology audience. 

It will eventually be submitted to a journal like Political Analysis or Journal
of Survey Statistics and Methodology.

## Key Design Patterns

**Question column resolution**: All major functions accept either `ques_cols` (explicit column names) or `ques_stem` (regex matched against `colnames(data)`). The internal `resolve_ques()` helper handles both paths uniformly.

**NA handling**: `treat_na = "exclude"` (default) drops NA responses from alignment calculations; `treat_na = "unaligned"` counts them as not aligned. Both variants are computed and stored; the selected one is copied to the primary column at the end of `measure_alignment()`.

**Weighting**: All alignment statistics are design-weighted. If no `weight_col` is supplied, a column of 1s is added silently. Plurality ties are broken alphabetically by response value for determinism.

**Haven/labelled data**: Input data may use `haven`-labelled columns (e.g., from `.dta` or `.sav` files). `measure_alignment()` calls `labelled::unlabelled()` and `haven::zap_labels()` before processing.

**Output attributes**: The `survalign` list carries metadata as R attributes (`group_col`, `weight_col`, `id_col`, `treat_na`, `group_label`). Plot functions read these attributes to auto-configure axes and legends — pass the `results` object directly rather than extracting individual slots.
