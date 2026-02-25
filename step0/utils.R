library(tidyverse)
library(ellmer)
library(jsonlite)
library(here)
library(haven)
library(labelled)
library(stringr)

#' Binarize survey items by cutpoints.
#'
#' Converts selected question columns to 0/1 based on scale or proportion cutpoints,
#' with optional NA recoding. Stores `binarized_cols` as an attribute.
#' @param data Data frame containing the items.
#' @param question_cols Character vector of columns to binarize.
#' @param prop_cutpoint Proportion threshold for default binarization.
#' @param scale_cutpoint Named list of cutpoints by scale length.
#' @param na_values Values to recode to NA before binarization.
#' @return Data frame with binarized columns.
binarize_items <- function(data, question_cols, prop_cutpoint = 0.5, scale_cutpoint = list(), na_values = NULL) {
  if (prop_cutpoint < 0 | prop_cutpoint > 1) {
    stop("prop_cutpoint must be between 0 and 1 (reflecting proportion of scale endpoint above which to code as 1)")
  }

  if (any(!question_cols %in% colnames(data))) {
    message("The following question columns are not present in the data and will be skipped: ", 
            paste(question_cols[!question_cols %in% colnames(data)], collapse = ", "))
    question_cols <- question_cols[question_cols %in% colnames(data)]
  }

  out <- data %>%
    mutate(across(all_of(question_cols), ~ {
      if (!all(is.null(na_values))) {
        .[. %in% na_values] <- NA
      }
      if (all(is.na(.))) {
        return(.)
      }

      if (!is.null(scale_cutpoint) && as.character(n_distinct(.)) %in% names(scale_cutpoint)) {
        y <- as.numeric(. > scale_cutpoint[[as.character(n_distinct(.))]])
      } else {
        x <- as.numeric(.)
        offset <- ifelse(min(x, na.rm = TRUE) == 1, 1, 0)
        y <- as.numeric((x - offset) > prop_cutpoint * (n_distinct(x, na.rm=T) - 1))
      }
      if (all(is.na(y)) || length(unique(na.omit(y))) < 2) {
        warning("Binarization of column '", cur_column(), "' resulted in all NA or constant values")
      }
      return(y)
    }))
  attr(out, "binarized_cols") <- question_cols
  out
}

#' Extract column labels and tab membership.
#'
#' Builds a data frame of variable labels and TRUE/FALSE indicators for each unique
#' value in `tab_col`, optionally writing the result to CSV.
#' @param data Data frame to inspect.
#' @param tab_col Column used to create tab indicators.
#' @param file Optional CSV output path.
#' @return Data frame of labels and tab indicators.
dump_column_labels_with_tabs <- function(data, tab_col, file = NULL) {
  # Extract variable names and safely extract labels
  data[[tab_col]] <- haven::zap_labels(data[[tab_col]])
  info <- purrr::map_dfr(colnames(data), .progress = TRUE, function(col) {
    tab_vals <- data %>% filter_at(col, ~!is.na(.x)) %>% pull(all_of(tab_col)) %>% unique()
    label <- if (!is.null(attr(data[[col]], "label"))) as.character(attr(data[[col]], "label")) else NA
    out <- data.frame(var=col, label=label)
    for (val in tab_vals) {
      out[[as.character(val)]] <- TRUE
    }
    return(out)
  }) %>%
    bind_rows() %>%
    rowwise() %>%
    mutate(across(-c("var", "label"), ~isTRUE(.x)))
  if (!is.null(file) && !is.na(file)) {
    # Write to CSV
    write.csv(info, file = file, row.names = FALSE)
  }
  return(info)
}

#' Extract column labels.
#'
#' Creates a data frame of variable names and labels, optionally writing to CSV.
#' @param data Data frame to inspect.
#' @param file Optional CSV output path.
#' @return Data frame with `var` and `label`.
dump_column_labels <- function(data, file = NULL) {
  # Extract variable names and safely extract labels
  info <- lapply(colnames(data), function(col) {
    label <- if (!is.null(attr(data[[col]], "label"))) as.character(attr(data[[col]], "label")) else NA
    data.frame(var=col, label=label)
  }) %>%
    bind_rows()
  if (!is.null(file) && !is.na(file)) {
    # Write to CSV
    write.csv(info, file = file, row.names = FALSE)
  }
  
  return(info)
}

#' Identify relevant columns using an LLM and cached results.
#'
#' Sends column labels in batches, caches results to disk, and returns the subset
#' classified as the target class.
#' @param data Data frame to inspect.
#' @param cols Candidate columns; defaults to all columns.
#' @param class Target class label.
#' @param sys_prompt System prompt for the classifier.
#' @param verbose Logical; emit progress messages.
#' @param max_attempts Max retry attempts per batch.
#' @param batch_size Number of columns per request.
#' @param cache_file RDS path for cached results.
#' @return Named list of columns classified as the target class.
identify_relevant_columns <- function(
    data, 
    cols = colnames(data),
    class = 'sociopolitical',
    sys_prompt = sprintf("Determine if each of the columns has to do with %s. Return in json the 'class' (%s or something else) and then 'label' (a short label for the question)", class, class),
    verbose = TRUE,
    max_attempts = 5,
    batch_size = 5,
    cache_file = sprintf("temp/column_%s_classes.rds", class)
) {
  chat <- chat_openai(sys_prompt)
  out.cols <- list()
  
  # Load or initialize cache
  cache <- if (file.exists(cache_file)) readRDS(cache_file) else list()
  
  # Helper: check cache
  is_cached <- function(key) !is.null(cache[[key]])
  
  # Helper: update cache
  cache_result <- function(key, value) {
    cache[[key]] <- value
    saveRDS(cache, cache_file)
  }
  
  # Prepare column texts
  column_texts <- sapply(cols, function(col) {
    label <- attr(data[[col]], "label")
    paste0(col, ": ", label)
  }, USE.NAMES = TRUE)
  
  # Filter out already cached
  uncached_cols <- cols[!vapply(cols, is_cached, logical(1))]
  
  if (verbose) {
    message("Cached: ", length(cols) - length(uncached_cols), "/", length(cols))
  }
  
  # Process in batches
  for (batch_start in seq(1, length(uncached_cols), by = batch_size)) {
    message("[",batch_start, "/", length(uncached_cols), "]")
    
    batch_end <- min(batch_start + batch_size - 1, length(uncached_cols))
    batch_cols <- uncached_cols[batch_start:batch_end]
    batch_prompts <- column_texts[batch_cols]
    
    batch_input <- paste(batch_prompts, collapse = "\n")
    
    for (attempt in 1:max_attempts) {
      tryCatch({
        response <- chat$chat(batch_input)
        response <- gsub("(```json|```)", "", response)
        parsed <- jsonlite::fromJSON(paste0("[", gsub("\\}\\s*\\{", "},{", response), "]"))
        
        for (j in seq_along(batch_cols)) {
          col <- batch_cols[j]
          res <- parsed[[j]]
          cache_result(col, res)
          if (res$class == class) {
            out.cols[[col]] <- res$label
          }
        }
        
        break  # Success
        
      }, error = function(e) {
        print(e)
        browser()
        cat("\nBatch error...retrying in a bit\n")
        Sys.sleep(10)
      })
    }
  }
  
  # Compile final output from cache
  for (col in cols) {
    if (is_cached(col)) {
      result <- cache[[col]]
      if (result$class == class) {
        out.cols[[col]] <- result$label
      }
    }
  }
  
  return(out.cols)
}


#' Build a simple variable/label tibble.
#'
#' @param data Data frame to inspect.
#' @return Tibble with `var` and `label`.
generate_column_labels_df <- function(data) {
  tibble(
    var = colnames(data),
    label = purrr::map_chr(colnames(data), ~ as.character(attr(data[[.x]], "label")))
  )
}


#' Analyze opinion coalitions for grouped survey data.
#'
#' Computes plurality responses, respondent alignment, and cumulative plurality
#' statistics by group for the specified question columns.
#' @param data Data frame containing responses.
#' @param question_cols Character vector of question columns.
#' @param group_var Grouping variable or named list for labeling.
#' @param weight_var Optional weight column name.
#' @param id_var Optional respondent id column name.
#' @param treat_na How to handle NA responses in alignment.
#' @return List of analysis tables and metadata attributes.
analyze_opinion_coalitions <- function(
    data, 
    question_cols, 
    group_var = NULL, 
    weight_var = NULL, 
    id_var = NULL,
    treat_na = c("exclude", "unaligned")
) {
  treat_na <- match.arg(treat_na)

  if (is.null(weight_var)) {
    weight_var <- "weight"
    data$weight <- 1
  }
  if (is.null(id_var)) {
    id_var <- "respondent_id"
    data$respondent_id <- 1:nrow(data)
  }
  if (is.null(group_var)) {
    message("Computing for all respondents...")
    group_var <- "all"
    data$group_var <- "All"
  }
  if (is.list(group_var)) {
    group_label <- names(group_var)[1]
    group_var <- group_var[[1]]
  } else {
    group_label <- NULL
    group_var <- group_var[1]
  }

  if (any(!question_cols %in% colnames(data))) {
    message("The following question columns are not present in the data and will be skipped: ", 
            paste(question_cols[!question_cols %in% colnames(data)], collapse = ", "))
    question_cols <- question_cols[question_cols %in% colnames(data)]
  }

  question_labels <- dump_column_labels(data[question_cols])
  
  message("Step 0: Pivotting")
  data <- data %>%
    labelled::unlabelled()

  data_long <- data %>%
    mutate(across(all_of(question_cols), ~haven::zap_labels(.x))) %>%
    mutate(across(all_of(question_cols), ~as.character(.x))) %>%
    select(all_of(c(group_var, weight_var, id_var, question_cols))) %>%
    filter_at(group_var, ~!is.na(.x)) %>%
    pivot_longer(cols = question_cols, names_to = "question", values_to = "response")
  
  if (all(is.na(data_long$response)))
    stop("all responses are NA")
  
  if (all(is.na(data_long[[group_var]])))
    stop("all group values are NA")
  
  if (all(is.na(data_long[[weight_var]])))
    stop("all weight values are NA")
  
  message("Step 1: Compute group-wise plurality opinions")
  plurality_opinions <- data_long %>%
    filter(!is.na(response)) %>%
    group_by(!!sym(group_var), question, response) %>%
    summarise(count_response_wtd = sum(!!sym(weight_var), na.rm = TRUE), 
              count_response_uwtd = n(),
              .groups = "drop") %>%
    group_by(!!sym(group_var), question) %>%
    mutate(prop_response_wtd = count_response_wtd / sum(count_response_wtd, na.rm = TRUE),
           prop_response_uwtd = count_response_uwtd / sum(count_response_uwtd, na.rm = TRUE)) %>%
    arrange(desc(prop_response_wtd)) %>%
    # filter(F_AGECAT == "50-64") %>%
    filter(row_number() == 1) %>%
    mutate(maj_wtd = prop_response_wtd > 0.5) %>%
    arrange_at(c("question", group_var)) %>%
    rename(plurality_response = response)
  if (nrow(plurality_opinions) == 0) {
    message("Step 1 failed: no plurality opinions computed. Confirm question columns are present/valid and responses exist.")
    browser()
  }
  if (any(plurality_opinions$prop_response_wtd < 0 | plurality_opinions$prop_response_wtd > 1)) {
    message("Step 1 failed: plurality proportions out of [0,1] range. Check weights and question responses.")
    browser()
  }
  if (all(abs(plurality_opinions$prop_response_wtd - 1) < 1e-8)) {
    message("Step 1 failed: every question's plurality response has 100% weight; there is no variation in the requested questions.")
    browser()
  }
  
  message("Step 2: Create a dataframe for whether each respondent agrees with the group majority")
  respondent_question_alignment <- data_long %>%
    left_join(
      plurality_opinions %>% select(!!sym(group_var), question, plurality_response),
      by = c(group_var, "question")
    ) %>%
    # filter(F_AGECAT == "50-64") %>%
    mutate(
      aligned = (response == plurality_response)
    ) %>%
    select(!!sym(group_var), !!sym(id_var), !!sym(weight_var), question, response, plurality_response, 
           starts_with("aligned"))
    
  message("Step 3: Sort questions by alignment within each group")
  question_pluralities <- plurality_opinions %>%
    arrange(!!sym(group_var), desc(prop_response_wtd)) %>%
    select(!!sym(group_var), question, plurality_response, prop_response_wtd) %>%
    group_by(!!sym(group_var)) %>%
    mutate(question_plurality_rank = row_number()) %>%
    ungroup()

  message("Step 4: Compute respondent's overall level of alignment with group")
  respondent_alignment <- respondent_question_alignment %>%
    group_by(!!sym(group_var), !!sym(id_var), !!sym(weight_var)) %>%
    summarise(prop_questions_aligned_drop_na = mean(aligned == T, na.rm=T),
              prop_questions_aligned_keep_na = mean(aligned %in% T, na.rm=T),
              prop_items_na = mean(is.na(response)),
              num_items = n(),
              .groups = "drop") %>%
    group_by(!!sym(group_var))
  if (nrow(respondent_alignment) == 0) {
    message("Step 4 failed: no respondent alignment statistics computed. Check id/weight/group/question variables.")
    browser()
  }
  if (any(respondent_alignment$prop_questions_aligned_drop_na < 0 | respondent_alignment$prop_questions_aligned_drop_na > 1, na.rm = TRUE)) {
    message("Step 4 failed: per-respondent alignment proportions fall outside [0,1].")
    browser()
  }

  # Check:
  respondent_question_alignment %>%
    inner_join(question_pluralities %>% 
      filter(question_plurality_rank %in% 1)) %>%
    group_by(!!sym(group_var), !!sym(id_var), !!sym(weight_var)) %>%
    summarise(aligned_drop_na = if (all(is.na(aligned))) NA else all(aligned == T, na.rm = TRUE),
              aligned_keep_na = if (all(is.na(aligned))) F else all(aligned %in% TRUE),
              prop_response_wtd = first(prop_response_wtd),
              .groups = "drop") %>%
    group_by(!!sym(group_var)) %>%
    summarise(prop_cumulative_plurality_drop_na = weighted.mean(aligned_drop_na, w = !!sym(weight_var), na.rm = T),
              prop_cumulative_plurality_keep_na = weighted.mean(aligned_keep_na, w = !!sym(weight_var), na.rm = T),
              prop_response_wtd = first(prop_response_wtd),
              .groups = "drop")
  
  message("Step 5: Compute cumulative pluralities (% aligned between each question and previous highest aligned question)")
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
                 by = c(group_var, "question", "plurality_response")) %>%
      arrange_at(group_var) %>%
      ## determine whether each respondent is in perfect alignment with all questions so far 
      group_by(!!sym(group_var), !!sym(id_var), !!sym(weight_var)) %>%
      summarise(aligned_drop_na = if (all(is.na(aligned))) NA else all(aligned == T, na.rm = TRUE),
                aligned_keep_na = if (all(is.na(aligned))) F else all(aligned %in% TRUE),
                .groups = "drop") %>%
      ## at group level, determine prop that are in alignment with all questions so far
      group_by(!!sym(group_var)) %>%
      summarise(prop_cumulative_plurality_drop_na = weighted.mean(aligned_drop_na, w = !!sym(weight_var), na.rm = T),
                prop_cumulative_plurality_keep_na = weighted.mean(aligned_keep_na, w = !!sym(weight_var), na.rm = T),
                .groups = "drop") %>%
      ## merge back with info about current question
      left_join(curr_question_alignment %>%
                  filter(question_plurality_rank == r), 
                by = c(group_var))
    if (r == 1) {
      diffs <- out$prop_cumulative_plurality_drop_na - out$prop_response_wtd
      if (!all(abs(diffs) < 1e-5)) {
        print(out %>% select(!!sym(group_var), prop_cumulative_plurality_drop_na, prop_response_wtd))
        message("Something went wrong")
        browser()
      }
    }
    return(out)
  })
  if (nrow(question_cumulative_pluralities) == 0) {
    message("Step 5 failed: cumulative plurality table is empty.")
    browser()
  }
  
  message("Step 6: Summary statistics")
  if (treat_na == "exclude") {
    message(" Note: NA responses dropped (treated as neither aligned nor unaligned)")
    respondent_alignment$prop_questions_aligned <- respondent_alignment$prop_questions_aligned_drop_na
    question_cumulative_pluralities$prop_cumulative_plurality <- question_cumulative_pluralities$prop_cumulative_plurality_drop_na
  } else if (treat_na == "unaligned") {
    message(" Note: NA responses explicitly treated as unaligned with group majority")
    respondent_alignment$prop_questions_aligned <- respondent_alignment$prop_questions_aligned_keep_na
    question_cumulative_pluralities$prop_cumulative_plurality <- question_cumulative_pluralities$prop_cumulative_plurality_keep_na
  }

  if (any(question_cumulative_pluralities$prop_cumulative_plurality < 0 | question_cumulative_pluralities$prop_cumulative_plurality > 1, na.rm = TRUE)) {
    message("Step 5 failed: cumulative plurality proportions fall outside [0,1].")
    browser()
  }
  if (all(question_cumulative_pluralities$prop_cumulative_plurality >= 0.9999, na.rm = TRUE)) {
    message("Step 5 failed: all cumulative plurality proportions equal 100%, so alignment statistics will be uninformative. Check data/question selection.")
    browser()
  }

  group_stats <- respondent_alignment %>%
    group_by(!!sym(group_var)) %>%
    summarise(cumulative_perfect_alignment = mean(prop_questions_aligned == 1, na.rm=T),
              cumulative_weak_alignment = mean(prop_questions_aligned > 0.5, na.rm=T),
              alignment_mean = mean(prop_questions_aligned, na.rm=T),
              alignment_median = median(prop_questions_aligned, na.rm=T),
              alignment_se = sd(prop_questions_aligned, na.rm=T)/sqrt(n()),
              n_respondents = n(),
              .groups = "drop")
  
  group_stats <- group_stats %>% 
    left_join(question_cumulative_pluralities %>%
      group_by(!!sym(group_var)) %>%
      summarise(mean_plurality = mean(prop_response_wtd, na.rm=T),
                cumulative_issue_alignment_n = max(question_plurality_rank[prop_cumulative_plurality >= 0.5]),
                cumulative_issue_alignment_prop = cumulative_issue_alignment_n / n_distinct(question),
                .groups = "drop"),
      by = group_var)
  
  attr(group_stats$cumulative_perfect_alignment, "label") <- 
    "% of respondents perfectly aligned (side with group plurality across issues)"
  attr(group_stats$cumulative_weak_alignment, "label") <- 
    "% of respondents weakly aligned (side with group plurality across most issues)"
  attr(group_stats$alignment_mean, "label") <- 
    "Average alignment (share of responses in group plurality) across respondents"
  attr(group_stats$alignment_median, "label") <- 
    "Median alignment (share of responses in group plurality) across respondents"
  attr(group_stats$alignment_se, "label") <- 
    "Standard error of alignment (share of responses in group plurality) across respondents"
  attr(group_stats$mean_plurality, "label") <- 
    "Average plurality (share of respondents in group plurality) across issues"
  attr(group_stats$cumulative_issue_alignment_n, "label") <- 
    "Cumulative issue alignment (number of issues where most respondents side with cumulative group plurality)"
  attr(group_stats$cumulative_issue_alignment_prop, "label") <- 
    "Cumulative issue alignment (prop. of issues where most respondents side with cumulative group plurality)"

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
  attr(output, "group_var") <- group_var
  attr(output, "weight_var") <- weight_var
  attr(output, "id_var") <- id_var
  attr(output, "treat_na") <- treat_na
  
  return(output)
}

#' Analyze opinion coalitions across waves.
#'
#' Wrapper around `analyze_opinion_coalitions` that iterates over waves (e.g. years),
#' optionally binarizing items before analysis.
#' @param data Data frame containing all waves.
#' @param question_cols Character vector of question columns.
#' @param group_var Grouping variable or named list for labeling.
#' @param wave_var Column containing wave identifiers.
#' @param waves Optional vector of waves to analyze.
#' @param id_var Optional respondent id column name.
#' @param weight_var Optional weight column name.
#' @param binarize Logical; apply `binarize_items` before analysis.
#' @param binarize_args Named list of arguments for `binarize_items`.
#' @param progress Logical; show progress in purrr::map.
#' @param ... Additional arguments passed to `analyze_opinion_coalitions`.
#' @return Named list of results by wave.
analyze_opinion_coalitions_waves <- function(
    data,
    question_cols,
    group_var,
    wave_var = "year",
    waves = NULL,
    id_var = "case_id",
    weight_var = "weight",
    binarize = FALSE,
    binarize_args = list(),
    progress = TRUE,
    ...
) {
  wave_var <- as.character(wave_var)
  if (!(wave_var %in% colnames(data))) {
    stop("Wave variable '", wave_var, "' not present in the supplied data")
  }
  wave_values <- waves
  if (is.null(wave_values)) {
    wave_values <- unique(data[[wave_var]])
  }
  wave_values <- wave_values[!is.na(wave_values)]
  if (length(wave_values) == 0) {
    stop("No waves supplied for ", wave_var)
  }

  purrr::map(
    wave_values,
    .progress = progress,
    function(current_wave) {
      message("\n[Wave = ", current_wave, "]\n")
      # tryCatch({
        df_wave <- data %>%
          filter(.data[[wave_var]] == current_wave)
        if (nrow(df_wave) == 0) {
          stop("No data available for wave ", current_wave)
        }

        available_questions <- intersect(question_cols, colnames(df_wave))
        if (length(available_questions) == 0) {
          stop("None of the requested question columns exist for wave ", current_wave)
        }

        question_cols_wave <- df_wave %>%
          select(all_of(available_questions)) %>%
          select(where(~!all(is.na(.)))) %>%
          colnames()

        if (length(question_cols_wave) == 0) {
          stop("No non-missing question columns for wave ", current_wave)
        }

        if (binarize) {
          binarize_call <- c(
            list(data = df_wave, question_cols = question_cols_wave),
            binarize_args
          )
          df_wave <- do.call(binarize_items, binarize_call)
        }

        analyze_opinion_coalitions(
          data = df_wave,
          question_cols = question_cols_wave,
          group_var = group_var,
          id_var = id_var,
          weight_var = weight_var,
          ...
        )
      # }, error = function(e) {
      #   warning("Wave ", current_wave, " failed: ", conditionMessage(e), call. = FALSE)
      #   list()
      # })
    }
  ) -> results

  names(results) <- as.character(wave_values)
  results
}

#' Plot descending issue alignment by group.
#'
#' @param results Output from `analyze_opinion_coalitions`.
#' @param title Plot title override.
#' @param show_response_value Logical; show response values in labels.
#' @return ggplot object.
plot_descending_issue_alignment <- function(
    results,
    title = ifelse(all(is.null(attr(results, "binarized_cols"))), 
                   "Issue Plurality vs. Cumulative Plurality",
                   "Issue Majority vs. Cumulative Majority"),
    show_response_value = TRUE
) {
  dat <- results$question_cumulative_pluralities %>%
    rename_at(attr(results, "group_var"), ~"group_var") %>%
    mutate(group_var = str_to_title(group_var)) %>%
    arrange(group_var, desc(prop_cumulative_plurality)) %>%
    group_by(group_var) %>%
    # keep all items where alignment except for first without alignment
    mutate(row_id = row_number()) %>%
    mutate(keep = if_else(prop_cumulative_plurality > 0.5, TRUE, FALSE)) %>%
    mutate(first_below = if_else(!keep & row_id == min(row_id[!keep]), TRUE, FALSE)) %>%
    filter(keep | first_below) %>%
    select(-keep, -first_below, -row_id) %>%
    ungroup() %>%
    # prep for plot
    mutate(question = tolower(gsub("\\_", "-", question))) %>%
    mutate(label = paste(question, plurality_response, sep = ": "))
  dat <- dat %>%
    mutate(
      label = paste(question, plurality_response, sep = ": "),
      group_label = interaction(group_var, label, drop = TRUE, sep = "."),
      group_label = factor(
        group_label,
        levels = dat %>%
          arrange(group_var, (prop_cumulative_plurality)) %>%
          mutate(group_label = interaction(group_var, paste(question, plurality_response, sep = ": "))) %>%
          pull(group_label)
      )
    )
  ggplot(dat, aes(y = group_label)) +
    geom_bar(stat = "identity", aes(x = prop_response_wtd, 
                                    fill = "For Single\nQuestion")) + 
    geom_bar(stat = "identity", aes(x = prop_cumulative_plurality, 
                                    fill = "For Question\nand All Above")) +
    geom_vline(xintercept = 0.5, color = "white", lty = 2) +
    scale_fill_manual(values = c("grey", "black"), name = "Plurality:") +
    scale_x_continuous(labels = scales::percent_format(1)) +
    scale_y_discrete(labels = function(labels) {
      if (show_response_value)
        str_replace(labels, '^.*\\.', '')
      else
        str_replace(labels, '^.*\\.', '') |> str_replace("\\:.*$", '')
    }) +
    facet_grid(group_var ~ ., scales = "free", space = "free") +
    labs(x = "",
         y = NULL,
         title = title) +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle = 0, size = rel(1.25)))
}

#' Plot alignment distributions and summary means by group.
#'
#' @param results Output from `analyze_opinion_coalitions`.
#' @param exclude_vals Group values to exclude.
#' @param type Density or histogram.
#' @param show_distributions Logical; show distribution shapes.
#' @param show_means Logical; show mean estimates and intervals.
#' @param ci_scale Multiplier for confidence intervals.
#' @return ggplot object.
plot_alignment_by_group <- function(
    results, 
    exclude_vals = "DK/REF", 
    type = c("density","histogram"),
    show_distributions = FALSE,
    show_means = TRUE,
    ci_scale = 1.96
) {
  binarized <- !all(is.null(attr(results, "binarized_cols")))
  type <- match.arg(type)
  data <- results$respondent_alignment %>% 
    filter_at(attr(results, "group_var"), ~!(.x %in% exclude_vals)) %>%
    rename_at(attr(results, "group_var"), ~"g") %>%
    group_by(g) %>%
    mutate(val_label = paste0(trimws(g, whitespace = "[ \t\r\n“”]"),"\n(n=",n(),")")) %>%
    ungroup()
  plot <- data %>%
    ggplot(aes(x = prop_questions_aligned, 
               color = g, fill = g)) +
    scale_y_continuous(name = "") +
    scale_x_continuous(labels = scales::percent_format(1), 
                       name = ifelse(binarized, "% of responses aligned with group majority across all issues", "% of responses aligned with group plurality across all issues"))
  if (show_distributions) {
    plot <- plot + {
      if (type == "density") {
        geom_density()
      } else {
        geom_histogram(position = "identity", alpha = 0.5)
      } 
    } +
      scale_fill_discrete(name = attr(results, "group_label")) +
      scale_color_discrete(name = attr(results, "group_label"))
  }
  if (show_means) {
    plot <- plot +
      geom_pointrange(data = data %>%
                        group_by(g, val_label) %>%
                        summarise(mean_prop = mean(prop_questions_aligned, na.rm=T),
                                  se_prop = sd(prop_questions_aligned, na.rm=T)/sqrt(n()),
                                  ci_upr = mean_prop + ci_scale*se_prop,
                                  ci_lwr = mean_prop - ci_scale*se_prop,
                                  .groups = "drop"),
                      aes(x=mean_prop, xmin=ci_lwr, xmax=ci_upr, y=0), color="black") 
  }
  plot + 
    facet_grid(val_label ~ .) + {
      if (show_means & !show_distributions)
        theme_bw()
      else
        theme_minimal()
    } +
    labs(caption="Note: observations are individual respondents.") +
    theme(legend.position = "none",
          axis.text.y = element_blank())
}

#' Plot cumulative alignment curves by group.
#'
#' @param results Output from `analyze_opinion_coalitions`.
#' @param group_label Optional label for legend.
#' @param group_colors Optional manual colors.
#' @param exclude_vals Group values to exclude.
#' @param resolution Vector of quantiles to evaluate.
#' @return ggplot object.
plot_cumulative_alignment_curve <- function(
    results, 
    group_label = attr(results, "group_label"),
    group_colors = NULL,
    exclude_vals = "DK/REF", 
    resolution = seq(0, 1, by = 0.01)
) {
  binarized <- !all(is.null(attr(results, "binarized_cols")))
  cumul_align <- resolution %>%
    map(~results$respondent_alignment %>%
          filter_at(attr(results, "group_var"), ~!(.x %in% exclude_vals)) %>%
          group_by_at(attr(results, "group_var")) %>%
          summarise(q = .x,
                    p = mean(prop_questions_aligned >= .x, na.rm=T),
                    n = n(),
                    .groups = "drop")) %>%
    bind_rows() %>%
    mutate(val_label = paste0(as.character(.[[attr(results, "group_var")]]) %>%
                                trimws(whitespace = "[ \t\r\n“”]") %>%
                                stringr::str_to_title(), 
                              " (n=",n,")"))  
  p <- cumul_align %>%
    distinct(p, .keep_all = TRUE) %>%
    ggplot(aes(y=q, x=p, 
               color=val_label, 
               linetype=val_label)) +
    geom_point() +
    geom_line(size=1) +
    geom_vline(xintercept = 0.5, alpha = 0.5) + 
    geom_hline(yintercept = 0.5, alpha = 0.5)
  
  if (!is.null(group_colors)) {
    p <- p + scale_color_manual(values = group_colors, , name=paste0(group_label,":"))
  } else {
    p <- p + scale_color_brewer(palette = "Set1", name=paste0(group_label,":")) 
  }
  p <- p + 
    scale_linetype_manual(values = c("solid", "dotted", "dotdash", "longdash", "twodash", "dashdot"),
                          name=paste0(group_label,":")) +
    geom_line(size=1) +
    scale_y_continuous(label = scales::percent_format(1),
                       limits = c(0, 1),
                       name = ifelse(binarized, "with % (or more) positions aligned with group majority", "with % (or more) positions aligned with group plurality")) +
    scale_x_continuous(label=scales::percent_format(1),
                       limits = c(0, 1),
                       name="% of respondents...") +
    theme_bw()
  p
}

#' Plot group-level metrics over time.
#'
#' @param results_list Named list of results by wave/year.
#' @param metric Column in group_stats to plot.
#' @return ggplot object.
plot_group_stat_over_time <- function(
    results_list, 
    metric = c(
      "cumulative_perfect_alignment",  
      "cumulative_weak_alignment",
      "alignment_mean",
      "cumulative_issue_alignment_prop"
    ),
    year_col = "year",
    group_col = "group",
    group_label = "Group",
    group_colors = NULL
) {
  metric <- match.arg(metric)

  # Build long table of group stats across periods
  group_stats_t <- purrr::imap_dfr(results_list, function(result, period_label) {
    if (!is.list(result) || is.null(result$group_stats)) return(NULL)
    group_var <- attr(result, "group_var")
    if (is.null(group_var) || !(group_var %in% colnames(result$group_stats))) return(NULL)
    result$group_stats %>%
      dplyr::rename(!!rlang::sym(group_col) := !!rlang::sym(group_var)) %>%
      dplyr::mutate(!!rlang::sym(year_col) := period_label)
  })

  if (nrow(group_stats_t) == 0) {
    stop("No group stats available in results_list")
  }

  # Ensure metric exists
  if (!(metric %in% colnames(group_stats_t))) {
    stop(sprintf("Metric '%s' not found in group stats output", metric))
  }

  # Derive y-axis label from attribute if available
  sample_result <- NULL
  for (r in results_list) {
    if (is.list(r) && !is.null(r$group_stats) && metric %in% colnames(r$group_stats)) {
      sample_result <- r
      break
    }
  }
  y_label <- if (!is.null(sample_result)) {
    lab <- attr(sample_result$group_stats[[metric]], "label")
    if (!is.null(lab) && nzchar(lab)) lab else ""
  } else {
    ""
  }

  # Prepare plotting data
  plot_data <- group_stats_t
  # if (!is.null(yrs)) {
  #   if (year_col %in% colnames(plot_data)) {
  #     plot_data <- plot_data %>% filter(!!rlang::sym(year_col) %in% yrs)
  #   }
  # }

  # Convert year for plotting (if possible)
  plot_data <- plot_data %>% mutate(!!rlang::sym(year_col) := as.numeric(!!rlang::sym(year_col)))

  p <- ggplot(plot_data, aes(x = !!rlang::sym(year_col), y = !!rlang::sym(metric), color = !!rlang::sym(group_col), group = !!rlang::sym(group_col))) +
    geom_point(aes(size = .data$n_respondents)) +
    geom_line() +
    geom_hline(yintercept = 0.5, lty = 2, alpha = 0.5) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(labels = scales::percent_format(1), name = ifelse(nzchar(y_label), y_label, metric)) +
    #facet_grid(as.formula(paste0(group_col, " ~ .")), scales = "free_y") +
    scale_size(name = "Sample Size:", guide = "none")
  
  if (any(!is.null(group_colors))) {
    p <- p + scale_color_manual(values = group_colors, name=paste0(group_label,":"))
  } else {
    p <- p + scale_color_discrete(name = paste0(group_label,":"))
  }
  
  p <- p +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle = 0, size = rel(1.25)))

  return(p)
}


#' Plot top questions over time as a tiled heatmap.
#'
#' @param results_list Named list of results by wave/year.
#' @param metric Column in question_cumulative_pluralities to plot.
#' @param top_n Number of top-ranked questions per group.
#' @param short_labels Logical; abbreviate labels.
#' @return ggplot object.
plot_questions_over_time <- function(
    results_list,
    metric = c("prop_cumulative_plurality", "prop_response_wtd"),
    top_n = 10,
    short_labels = TRUE,
    palette = "Set1"
) {
  metric <- match.arg(metric)

  # Collect question-level cumulative pluralities across periods (results_list should be a named list)
  df <- purrr::imap_dfr(results_list, function(res, period_label) {
    if (!is.list(res) || is.null(res$question_cumulative_pluralities)) return(NULL)
    group_var <- attr(res, "group_var")
    if (is.null(group_var)) group_var <- "group"
    qdf <- res$question_cumulative_pluralities %>%
      dplyr::rename(!!rlang::sym(group_var) := !!rlang::sym(group_var)) %>%
      dplyr::mutate(year = period_label) %>%
      dplyr::mutate(question_label = paste(question, plurality_response, sep = ": ")) %>%
      dplyr::select(all_of(group_var), question, question_label, question_plurality_rank, prop_response_wtd, prop_cumulative_plurality, year)
    # normalize group column name
    qdf %>% dplyr::rename(group = !!rlang::sym(group_var))
  })

  if (nrow(df) == 0) stop("No question_cumulative_pluralities found in results_list")

  # convert year to numeric if possible
  df <- df %>% mutate(year = as.numeric(as.character(year)))

  df <- df %>% mutate(question = gsub("\\_", "-", question))

  # Choose top N questions per group by mean rank across years (lower rank = more important)
  top_questions <- df %>%
    group_by(group, question, question_label) %>%
    summarise(mean_rank = mean(question_plurality_rank, na.rm = TRUE), .groups = "drop") %>%
    arrange(group, mean_rank) %>%
    group_by(group) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    mutate(qkey = paste(group, question, sep = "___"))

  df <- df %>% mutate(qkey = paste(group, question, sep = "___")) %>%
    filter(qkey %in% top_questions$qkey)

  # Build human-friendly label for legend
  df <- df %>% mutate(question_short = ifelse(short_labels, question, question_label))

  # For plotting as tiles we will create a per-group question key so labels are unique
  # Order questions within each group by mean_rank (from top_questions)
  ordering <- top_questions %>% group_by(group) %>% arrange(mean_rank) %>%
    summarize(order = list(question), .groups = "drop")

  df <- df %>%
    left_join(top_questions %>% select(group, question, question_label, qkey), by = c("group", "question"))

  # create a group-specific factor for y axis (qkey preserves group-question uniqueness)
  df <- df %>%
    mutate(qkey = paste(group, question, sep = "___")) %>%
    group_by(group) %>%
    mutate(q_order = match(question, ordering$order[[which(ordering$group==group)[1]]])) %>%
    ungroup()

  # fallback: if ordering failed, keep original question order
  df$q_order[is.na(df$q_order)] <- 1

  df <- df %>% mutate(question_fac = forcats::fct_reorder(qkey, q_order))

  # Build plot: x = year, y = question (within facet), fill = metric, and numeric label
  p <- ggplot(df, aes(x = as.numeric(year), y = question_fac)) +
    geom_tile(aes(fill = !!rlang::sym(metric)), color = "grey") +
    #geom_text(aes(label = scales::percent(!!rlang::sym(metric), accuracy = 0.1)), size = 3) +
    facet_grid(group ~ ., scales = "free_y") +
    scale_x_continuous() +
    scale_fill_viridis_b() +
    # scale_fill_gradient2(low = "red", high = "#00BA38", name = metric) +
    labs(x = "Year", y = NULL) +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0.8)),
          strip.text.y = element_text(angle = 0, size = rel(1.25)),
          legend.position = "top")

  # simplify y labels (remove group prefix) if requested
  if (short_labels) {
    p <- p + scale_y_discrete(labels = function(lbls) sub("^.*___", "", lbls))
  } else {
    p <- p + scale_y_discrete(labels = function(lbls) lbls)
  }

  return(p)
}

#' Summarize top plurality questions by group and year.
#'
#' @param results_list Named list of results by wave/year.
#' @param group_var Group column name in results.
#' @param prop_threshold Minimum cumulative plurality to include.
#' @param top_rank Maximum rank to include.
#' @return Data frame of top-ranked plurality labels by group/year.
summarize_group_pluralities <- function(results_list,
                                        group_var = "pid3",
                                        prop_threshold = 0.5,
                                        top_rank = 10) {
  years <- names(results_list)[seq_along(results_list)]
  purrr::map2_dfr(years, results_list, function(year, dt) {
    if (length(dt) == 0) {
      return(tibble::tibble())
    }
    dt$question_cumulative_pluralities %>%
      dplyr::filter(prop_cumulative_plurality >= prop_threshold) %>%
      dplyr::mutate(year = year) %>%
      dplyr::select(!!sym(group_var),
                    year,
                    question,
                    plurality_response,
                    prop_cumulative_plurality,
                    prop_response_wtd,
                    question_plurality_rank)
  }) %>%
    dplyr::filter(question_plurality_rank <= top_rank) %>%
    dplyr::mutate(label = paste0(question,
                                 ":",
                                 plurality_response,
                                 " (",
                                 round(prop_cumulative_plurality * 100, 2),
                                 "%)")) %>%
    dplyr::select(!!sym(group_var), year, question_plurality_rank, label) %>%
    dplyr::arrange(!!sym(group_var), year, question_plurality_rank) %>%
    tidyr::pivot_wider(names_from = question_plurality_rank, values_from = label) %>%
    as.data.frame()
}
