#' Measure group alignment in survey data
#'
#' Computes plurality responses, respondent alignment, and cumulative plurality
#' statistics by group for the specified question columns.
#' @param data Data frame containing responses.
#' @param wave_col Column with wave identifiers (e.g., year).
#' @param ques_cols Optional character vector of question columns to include.
#' @param ques_stem Optional vector of substrings/regex used to match columns.
#' @param group_col Grouping variable or named list for labeling.
#' @param weight_col Optional weight column name.
#' @param id_col Optional respondent id column name.
#' @param treat_na How to handle NA responses in alignment.
#' @return List of analysis tables and metadata attributes.
#' @export
measure_alignment <- function(
    data, 
    wave_col = NULL,
    ques_cols = NULL,
    ques_stem = NULL,
    group_col = NULL, 
    weight_col = NULL, 
    id_col = NULL,
    verbose = TRUE,
    treat_na = c("exclude", "unaligned")
) {
  treat_na <- match.arg(treat_na)

  if (is.null(weight_col)) {
    weight_col <- "weight"
    data$weight <- 1
  }
  if (is.null(id_col)) {
    id_col <- "respondent_id"
    data$respondent_id <- 1:nrow(data)
  }
  if (is.null(group_col)) {
    group_col <- "all"
    data$group_col <- "All"
  }
  if (is.list(group_col)) {
    group_label <- names(group_col)[1]
    group_col <- group_col[[1]]
  } else {
    group_label <- attr(data[[group_col]], "label")
    group_col <- group_col[1]
  }

  ques_cols <- resolve_ques(
    data = data,
    ques_cols = ques_cols,
    ques_stem = ques_stem,
    exclude_cols = wave_col
  )
  
  question_labels <- dump_labels(data[ques_cols])
  
  if (verbose) cat("\n[0/5] Pivotting...")
  data <- data %>%
    labelled::unlabelled()

  data_long <- data %>%
    mutate_at(ques_cols, ~haven::zap_labels(.x)) %>%
    mutate_at(ques_cols, ~as.character(.x)) %>%
    select_at(c(group_col, weight_col, id_col, ques_cols)) %>%
    filter_at(group_col, ~!is.na(.x)) %>%
    pivot_longer(cols = ques_cols, names_to = "question", values_to = "response")
  
  if (all(is.na(data_long$response)))
    stop("all responses are NA")
  
  if (all(is.na(data_long[[group_col]])))
    stop("all group values are NA")
  
  if (all(is.na(data_long[[weight_col]])))
    stop("all weight values are NA")
  
  if (verbose) cat("\n[1/5] Computing group-wise plurality opinions")
  plurality_opinions <- data_long %>%
    filter(!is.na(response)) %>%
    group_by(!!sym(group_col), question, response) %>%
    summarise(count_response_wtd = sum(!!sym(weight_col), na.rm = TRUE), 
              count_response_uwtd = n(),
              .groups = "drop") %>%
    group_by(!!sym(group_col), question) %>%
    mutate(prop_response_wtd = count_response_wtd / sum(count_response_wtd, na.rm = TRUE),
           prop_response_uwtd = count_response_uwtd / sum(count_response_uwtd, na.rm = TRUE)) %>%
    arrange(desc(prop_response_wtd)) %>%
    # filter(F_AGECAT == "50-64") %>%
    filter(row_number() == 1) %>%
    mutate(maj_wtd = prop_response_wtd > 0.5) %>%
    arrange_at(c("question", group_col)) %>%
    rename(plurality_response = response)
  if (nrow(plurality_opinions) == 0) {
    stop("No plurality opinions computed. Confirm question columns are present/valid and responses exist.")
  }
  if (any(plurality_opinions$prop_response_wtd < 0 | plurality_opinions$prop_response_wtd > 1)) {
    stop("Plurality proportions out of [0,1] range. Check weights and question responses.")
  }
  if (all(abs(plurality_opinions$prop_response_wtd - 1) < 1e-8)) {
    stop("All respondents selected plurality response to all questions; there is no variation in the requested questions.")
  }
  
  if (verbose) cat("\n[2/5] Measuring question-level respondent group alignment")
  respondent_question_alignment <- data_long %>%
    left_join(
      plurality_opinions %>% select(!!sym(group_col), question, plurality_response),
      by = c(group_col, "question")
    ) %>%
    # filter(F_AGECAT == "50-64") %>%
    mutate(
      aligned = (response == plurality_response)
    ) %>%
    select(!!sym(group_col), !!sym(id_col), !!sym(weight_col), question, response, plurality_response, 
           starts_with("aligned"))
    
  if (verbose) cat("\n[3/5] Sorting questions by alignment within each group")
  question_pluralities <- plurality_opinions %>%
    arrange(!!sym(group_col), desc(prop_response_wtd)) %>%
    select(!!sym(group_col), question, plurality_response, prop_response_wtd) %>%
    group_by(!!sym(group_col)) %>%
    mutate(question_plurality_rank = row_number()) %>%
    ungroup()

  if (verbose) cat("\n[4/5] Measuring respondent's overall group alignment")
  respondent_alignment <- respondent_question_alignment %>%
    group_by(!!sym(group_col), !!sym(id_col), !!sym(weight_col)) %>%
    summarise(prop_questions_aligned_drop_na = weighted.mean(aligned == T, w=!!sym(weight_col), na.rm=T),
              prop_questions_aligned_keep_na = weighted.mean(aligned %in% T, w=!!sym(weight_col), na.rm=T),
              prop_items_na = mean(is.na(response)),
              num_items = n(),
              .groups = "drop") %>%
    group_by(!!sym(group_col))
  if (nrow(respondent_alignment) == 0) {
    message("\nFailed: no respondent alignment statistics computed. Check id/weight/group/question variables.")
    browser()
  }
  if (any(respondent_alignment$prop_questions_aligned_drop_na < 0 | respondent_alignment$prop_questions_aligned_drop_na > 1, na.rm = TRUE)) {
    message("\nFailed: per-respondent alignment proportions fall outside [0,1].")
    browser()
  }

  # # Check:
  # respondent_question_alignment %>%
  #   inner_join(question_pluralities %>% 
  #     filter(question_plurality_rank %in% 1), 
  #     by = c("group", "question", "plurality_response")) %>%
  #   group_by(!!sym(group_col), !!sym(id_col), !!sym(weight_col)) %>%
  #   summarise(aligned_drop_na = if (all(is.na(aligned))) NA else all(aligned == T, na.rm = TRUE),
  #             aligned_keep_na = if (all(is.na(aligned))) F else all(aligned %in% TRUE),
  #             prop_response_wtd = first(prop_response_wtd),
  #             .groups = "drop") %>%
  #   group_by(!!sym(group_col)) %>%
  #   summarise(prop_cumulative_plurality_drop_na = weighted.mean(aligned_drop_na, w = !!sym(weight_col), na.rm = T),
  #             prop_cumulative_plurality_keep_na = weighted.mean(aligned_keep_na, w = !!sym(weight_col), na.rm = T),
  #             prop_response_wtd = first(prop_response_wtd),
  #             .groups = "drop")
  
  if (verbose) cat("\n[5/5] Computing cumulative pluralities")
  question_cumulative_pluralities <- purrr::map_dfr(1:max(question_pluralities$question_plurality_rank), .progress = T, function(r) {
    curr_question_alignment <- question_pluralities %>%
      filter(question_plurality_rank <= r)
    # if (treat_na == "exclude") {
    #   respondent_question_alignment <- respondent_question_alignment %>%
    #     filter(!is.na(response))
    # }

    out <- respondent_question_alignment %>%
      ## subset to all questions at current alignment or higher
      inner_join(curr_question_alignment,
                 by = c(group_col, "question", "plurality_response")) %>%
      arrange_at(group_col) %>%
      ## determine whether each respondent is in perfect alignment with all questions so far 
      group_by(!!sym(group_col), !!sym(id_col), !!sym(weight_col)) %>%
      summarise(aligned_drop_na = if (all(is.na(aligned))) NA else all(aligned == T, na.rm = TRUE),
                aligned_keep_na = if (all(is.na(aligned))) F else all(aligned %in% TRUE),
                .groups = "drop") %>%
      ## at group level, determine prop that are in alignment with all questions so far
      group_by(!!sym(group_col)) %>%
      summarise(prop_cumulative_plurality_drop_na = weighted.mean(aligned_drop_na, w = !!sym(weight_col), na.rm = T),
                prop_cumulative_plurality_keep_na = weighted.mean(aligned_keep_na, w = !!sym(weight_col), na.rm = T),
                .groups = "drop") %>%
      ## merge back with info about current question
      left_join(curr_question_alignment %>%
                  filter(question_plurality_rank == r), 
                by = c(group_col))
    if (r == 1) {
      diffs <- out$prop_cumulative_plurality_drop_na - out$prop_response_wtd
      if (!all(abs(diffs) < 1e-5)) {
        print(out %>% select(!!sym(group_col), prop_cumulative_plurality_drop_na, prop_response_wtd))
        message("Something went wrong")
        browser()
      }
    }
    return(out)
  })
  if (nrow(question_cumulative_pluralities) == 0) {
    message("\nFailed: cumulative plurality table is empty.")
    browser()
  }
  
  if (treat_na == "exclude") {
    message("\nNote: NA responses dropped (treated as neither aligned nor unaligned)")
    respondent_alignment$prop_questions_aligned <- respondent_alignment$prop_questions_aligned_drop_na
    question_cumulative_pluralities$prop_cumulative_plurality <- question_cumulative_pluralities$prop_cumulative_plurality_drop_na
  } else if (treat_na == "unaligned") {
    message("\nNote: NA responses explicitly treated as unaligned with group majority")
    respondent_alignment$prop_questions_aligned <- respondent_alignment$prop_questions_aligned_keep_na
    question_cumulative_pluralities$prop_cumulative_plurality <- question_cumulative_pluralities$prop_cumulative_plurality_keep_na
  }

  if (any(question_cumulative_pluralities$prop_cumulative_plurality < 0 | question_cumulative_pluralities$prop_cumulative_plurality > 1, na.rm = TRUE)) {
    message("\nFailed: cumulative plurality proportions fall outside [0,1].")
    browser()
  }
  if (all(question_cumulative_pluralities$prop_cumulative_plurality >= 0.9999, na.rm = TRUE)) {
    message("\nFailed: all cumulative plurality proportions equal 100%, so alignment statistics will be uninformative. Check data/question selection.")
    browser()
  }

  group_stats <- respondent_alignment %>%
    group_by(!!sym(group_col)) %>%
    summarise(cumulative_perfect_alignment = weighted.mean(prop_questions_aligned == 1, w=!!sym(weight_col), na.rm=T),
              cumulative_weak_alignment = weighted.mean(prop_questions_aligned > 0.5, w=!!sym(weight_col), na.rm=T),
              alignment_mean = weighted.mean(prop_questions_aligned, w=!!sym(weight_col), na.rm=T),
              alignment_median = median(prop_questions_aligned, na.rm=T),
              alignment_se = sd(prop_questions_aligned, na.rm=T)/sqrt(n()),
              n_respondents = n(),
              .groups = "drop")
  
  group_stats <- group_stats %>% 
    left_join(question_cumulative_pluralities %>%
      group_by(!!sym(group_col)) %>%
      summarise(mean_plurality = mean(prop_response_wtd, na.rm=T),
                cumulative_issue_alignment_n = ifelse(any(prop_cumulative_plurality >= 0.5), max(question_plurality_rank[prop_cumulative_plurality >= 0.5]), 0),
                cumulative_issue_alignment_prop = cumulative_issue_alignment_n / n_distinct(question),
                .groups = "drop"),
      by = group_col)
  
  attr(group_stats$cumulative_perfect_alignment, "label") <- "Percent of respondents perfectly aligned"
  attr(group_stats$cumulative_perfect_alignment, "description") <- "Respondents who support their group's plurality position across all issues"
  
  attr(group_stats$cumulative_weak_alignment, "label") <- "Percent of respondents weakly aligned"
  attr(group_stats$cumulative_weak_alignment, "description") <- "Respondents who support their group's plurality position across most issues"
  
  attr(group_stats$alignment_mean, "label") <- "Average percent of respondent alignment"
  attr(group_stats$alignment_mean, "description") <- "Average percent of issues where respondent supports their group's plurality position"
  
  attr(group_stats$alignment_median, "label") <- "Median percent of respondent alignment"
  attr(group_stats$alignment_median, "description") <- "Median percent of issues where respondent supports their group's plurality position"
  
  attr(group_stats$alignment_se, "label") <- "Standard error of respondent alignment"
  attr(group_stats$alignment_se, "description") <- "Standard error of percent of issues where respondent supports their group's plurality position"
  
  attr(group_stats$mean_plurality, "label") <- "Average plurality"
  attr(group_stats$mean_plurality, "description") <- "Average percent of respondents aligned with group plurality across all issues"
  
  attr(group_stats$cumulative_issue_alignment_n, "label") <- "Cumulative issue alignment (N)"
  attr(group_stats$cumulative_issue_alignment_n, "description") <- "Number of issues where a majority of respondents support their group's plurality position"
  
  attr(group_stats$cumulative_issue_alignment_prop, "label") <- "Cumulative issue alignment (%)"
  attr(group_stats$cumulative_issue_alignment_prop, "description") <- "Percent of issues where a majority of respondents support their group's plurality position"

  # Output all dataframes
  output <- list(
    respondent_alignment = respondent_alignment,
    question_plurality_opinions = plurality_opinions,
    question_pluralities = question_pluralities,
    question_cumulative_pluralities = question_cumulative_pluralities,
    group_stats = group_stats,
    question_labels = question_labels
  )
  attr(output, "group_label") <- group_label
  attr(output, "group_col") <- group_col
  attr(output, "weight_col") <- weight_col
  attr(output, "id_col") <- id_col
  attr(output, "treat_na") <- treat_na
  
  class(output) <- c("survalign", class(output))
  return(output)
}


#' Print method for survalign objects.
#' @method print survalign
#' @export
print.survalign <- function(x, ...) {
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("Package 'cli' needed for color output")
  }
  cli::cli_text("{.strong Survey Alignment Measures}")
  cli::cli_bullets(c(
    "- {.field Group}: {.val {attr(x, 'group_col')}}",
    "- {.field Weight}: {.val {attr(x, 'weight_col')}}",
    "- {.field NA handling}: {.val {attr(x, 'treat_na')}}",
    "- {.field Number of groups}: {.val {nrow(x$group_stats)}}",
    "- {.field Number of respondents}: {.val {nrow(x$respondent_alignment)}}",
    "- {.field Number of questions}: {.val {dplyr::n_distinct(x$question_pluralities$question)}}"
  ))
  invisible(x)
}

#' Summary method for survalign objects.
#'
#' Returns group-level alignment statistics from a `survalign` object created by `measure_alignment()`.
#'
#' @param object An object of class 'survalign'.
#' @param ... Additional arguments (ignored).
#' @return A data frame of group stats, including respondent alignment and plurality metrics.
#' @export
summary.survalign <- function(object, format = c("wide", "long")) {
  format <- match.arg(format)
  if (format == "wide") {
    return(object$group_stats)
  } else {
    return(object$group_stats %>%
             pivot_longer(-any_of(attr(object, "group_col")),
                          names_to = "statistic", values_to = "value"))
  }
}



#' Analyze opinion coalitions across waves.
#'
#' Wrapper around `analyze_opinion_coalitions` that iterates over waves (e.g. years),
#' optionally binarizing items before analysis.
#' @param data Data frame containing all waves.
#' @param ques_cols Character vector of question columns.
#' @param ques_stem Optional vector of substrings/regex used to match columns.
#' @param group_col Grouping variable or named list for labeling.
#' @param wave_col Column containing wave identifiers.
#' @param waves Optional vector of waves to analyze.
#' @param id_col Optional respondent id column name.
#' @param weight_col Optional weight column name.
#' @param binarize Logical; apply `binarize_items` before analysis.
#' @param binarize_args Named list of arguments for `binarize_items`.
#' @param na_action Logical; if a wave is missing, either throw error or warning.
#' @param progress Logical; show progress in purrr::map.
#' @param ... Additional arguments passed to `analyze_opinion_coalitions`.
#' @return Named list of results by wave.
#' @export
measure_alignment_waves <- function(
    data,
    ques_cols = NULL,
    ques_stem = NULL,
    group_col,
    wave_col = "year",
    waves = NULL,
    id_col = "case_id",
    weight_col = "weight",
    binarize = FALSE,
    na_action = c("warn", "fail"),
    binarize_args = list(),
    progress = TRUE,
    ...
) {
  na_action <- match.arg(na_action)
  
  wave_col <- as.character(wave_col)
  if (!(wave_col %in% colnames(data))) {
    stop("Wave variable '", wave_col, "' not present in the supplied data")
  }
  wave_values <- waves
  if (is.null(wave_values)) {
    wave_values <- unique(data[[wave_col]])
  }
  wave_values <- wave_values[!is.na(wave_values)]
  if (length(wave_values) == 0) {
    stop("No waves supplied for ", wave_col)
  }
  
  if (is.null(ques_cols) & is.null(ques_stem)) {
    stop("Must supply either ques_cols or ques_stem")
  }

  purrr::map(
    wave_values,
    .progress = progress,
    function(current_wave) {
      message("\n[Wave = ", current_wave, "]\n")
      # tryCatch({
        df_wave <- data %>%
          filter(.data[[wave_col]] == current_wave)
        if (nrow(df_wave) == 0) {
          msg <- paste0("No data available for ", current_wave)
          if (na_action == "fail") stop(msg)
          else { warnings(msg); return(NULL)}
        }
        
        ques_cols_wave <- resolve_ques(
          data = df_wave,
          ques_cols = ques_cols,
          ques_stem = ques_stem,
          exclude_cols = wave_col
        )
        
        ques_cols_wave <- df_wave %>%
          select(all_of(ques_cols_wave)) %>%
          select(where(~!all(is.na(.)))) %>%
          colnames()

        if (length(ques_cols_wave) == 0) {
          msg <- paste0("No non-missing question columns for wave ", current_wave)
          if (na_action == "fail") stop(msg)
          else { warnings(msg); return(NULL)}
        }

        if (binarize) {
          binarize_call <- c(
            list(data = df_wave, ques_cols = ques_cols_wave),
            binarize_args
          )
          df_wave <- do.call(binarize_items, binarize_call)
        }

        measure_alignment(
          data = df_wave,
          ques_cols = ques_cols_wave,
          group_col = group_col,
          id_col = id_col,
          weight_col = weight_col,
          ...
        )
      # }, error = function(e) {
      #   warning("Wave ", current_wave, " failed: ", conditionMessage(e), call. = FALSE)
      #   list()
      # })
    }
  ) -> results

  names(results) <- as.character(wave_values)
  class(results) <- c("survalign.waves", class(results))
  
  results
}
