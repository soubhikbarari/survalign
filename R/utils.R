#' Resolve columns corresponding to questions (internal)
resolve_ques <- function(data, ques_cols = NULL, ques_stem = NULL, exclude_cols = NULL) {
  if (!is.null(ques_cols)) {
    if (!is.null(ques_stem)) {
      warning("ques_cols provided; ignoring ques_stem.")
    }
    missing_cols <- setdiff(ques_cols, colnames(data))
    if (length(missing_cols) > 0) {
      message("Skipping missing question columns: ", paste(missing_cols, collapse = ", "))
    }
    ques_cols <- intersect(ques_cols, colnames(data))
  } else if (!is.null(ques_stem)) {
    pattern <- if (length(ques_stem) == 1) {
      ques_stem
    } else {
      paste(ques_stem, collapse = "|")
    }
    ques_cols <- colnames(data)[stringr::str_detect(colnames(data), pattern)]
  } else {
    exclude_cols <- if (is.null(exclude_cols)) character(0) else exclude_cols
    ques_cols <- setdiff(colnames(data), exclude_cols)
  }

  exclude_cols <- if (is.null(exclude_cols)) character(0) else exclude_cols
  ques_cols <- setdiff(unique(ques_cols), exclude_cols)
  if (length(ques_cols) == 0) {
    stop("No question columns resolved. Provide ques_cols, ques_stem, or adjust exclude_cols.")
  }
  ques_cols
}

#' Get top values for columns (as list)
get_top_vals <- function(data, ques_cols) {
  top_vals <- purrr::map_dbl(ques_cols, function(col) {
    x <- data[[col]]
    if (all(is.na(x))) return(NA_real_)
    suppressWarnings(max(as.numeric(x), na.rm = TRUE))
  })
  names(top_vals) <- ques_cols
  return(top_vals)
}

#' Dump column labels to dataframe (internal)
dump_labels <- function(data, attr = c("label", "shortlabel"), file = NULL) {
  attr <- match.arg(attr)
  info <- lapply(colnames(data), function(col) {
    label <- if (!is.null(attr(data[[col]], attr))) {
      as.character(attr(data[[col]], attr))
    } else {
      NA
    }
    data.frame(var = col, label = label)
  }) %>%
    bind_rows()
  if (!is.null(file) && !is.na(file)) {
    write.csv(info, file = file, row.names = FALSE)
  }
  info
}

#' Binarize survey responses by cutpoints.
#'
#' Converts selected question columns to 0/1 based on scale or proportion cutpoints,
#' with optional NA recoding. Stores `binarized_cols` as an attribute.
#' @param data Data frame containing the items.
#' @param ques_cols Character vector of columns to binarize.
#' @param prop_cutpoint Proportion threshold for default binarization.
#' @param scale_cutpoint Named list of cutpoints by scale length.
#' @param na_values Values to recode to NA before binarization.
#' @return Data frame with binarized columns.
#' @export
binarize_responses <- function(data, ques_cols, prop_cutpoint = 0.5, scale_cutpoint = list(), na_values = NULL) {
  if (prop_cutpoint < 0 | prop_cutpoint > 1) {
    stop("prop_cutpoint must be between 0 and 1 (reflecting proportion of scale endpoint above which to code as 1)")
  }

  if (any(!ques_cols %in% colnames(data))) {
    message("The following question columns are not present in the data and will be skipped: ", 
            paste(ques_cols[!ques_cols %in% colnames(data)], collapse = ", "))
    ques_cols <- ques_cols[ques_cols %in% colnames(data)]
  }

  out <- data %>%
    mutate(across(all_of(ques_cols), ~ {
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
  attr(out, "binarized_cols") <- ques_cols
  out
}

#' Plot question coverage by wave.
#'
#' Builds a heatmap of non-missing counts by wave for selected items.
#' @param data Data frame containing the survey items.
#' @param wave_col Column with wave identifiers (e.g., year).
#' @param ques_cols Optional character vector of question columns to include.
#' @param ques_stem Optional vector of substrings/regex used to match columns.
#' @param show_labels Logical; draw percentage labels on tiles.
#' @return ggplot object.
#' @export
plot_coverage <- function(
  data, 
  wave_col = "year",
  ques_cols = NULL,
  ques_stem = NULL,
  show_labels = TRUE
) {

  wave_col <- as.character(wave_col)[1]
  if (!(wave_col %in% colnames(data))) {
    stop("Wave variable '", wave_col, "' not present in the supplied data.")
  }

  ques_cols <- resolve_ques(
    data = data,
    ques_cols = ques_cols,
    ques_stem = ques_stem,
    exclude_cols = wave_col
  )

  coverage <- data %>%
    select(all_of(c(wave_col, ques_cols))) %>%
    group_by(.data[[wave_col]]) %>%
    summarise(across(all_of(ques_cols), ~mean(is.na(.x))), .groups = "drop")

  p <- coverage %>%
    pivot_longer(cols = -all_of(wave_col), names_to = "variable", values_to = "pct_missing") %>%
    filter(pct_missing != 1) %>%
    mutate(variable = str_to_kebab(variable)) %>%
    ggplot(aes(y = variable, x = .data[[wave_col]], fill = 100*pct_missing)) +
    geom_tile(color = "white")
  
  if (show_labels) {
    p <- p + 
      geom_text(aes(label = sprintf("%.0f", 100*pct_missing),
                    color = ifelse(pct_missing > 0.5, "black", "white")), size = 3)
  }
  p +       
    labs(y = "", x = "Wave") +
    scale_fill_viridis_c(name = "Percent missing:") +
    scale_color_identity() +
    theme_bw() +
    theme(legend.position = "top")
}

#' Plot item support by wave.
#'
#' Computes group-wise percent support for selected items and plots a heatmap.
#' Note: max value in each question column is treated as "support" value.
#' 
#' @param data Data frame containing the survey items.
#' @param wave_col Column with wave identifiers (e.g., year).
#' @param weight_col Weight column; if missing, equal weights are used.
#' @param group_col Grouping column; if NULL, uses a single "all" group.
#' @param facet_by Whether to facet by variable or group.
#' @param ques_cols Optional character vector of question columns to include.
#' @param ques_stem Optional vector of substrings used to match columns.
#' @param show_labels Logical; draw percentage labels on tiles.
#' @return ggplot object.
#' @export
plot_support <- function(
  data, 
  wave_col,
  weight_col = NULL,
  group_col = NULL,
  facet_by = c("variable", "group_col"),
  ques_cols = NULL,
  ques_stem = NULL,
  show_labels = TRUE
) {
  data_clean <- data %>% select(-matches("_orig"))
  wave_col <- as.character(wave_col)[1]
  weight_col <- as.character(weight_col)[1]
  group_col <- as.character(group_col)[1]
  if (!(wave_col %in% colnames(data_clean))) {
    stop("Wave variable '", wave_col, "' not present in the supplied data.")
  }
  if (is.null(group_col) || is.na(group_col)) {
    group_col <- "all"
    data_clean$all <- "all"
  } else if (!(group_col %in% colnames(data_clean))) {
    stop("Group variable '", group_col, "' not present in the supplied data.")
  }
  if (!(weight_col %in% colnames(data_clean))) {
    message("Weight variable '", weight_col, "' not found; using equal weights.")
    data_clean[[weight_col]] <- 1
  }

  ques_cols <- resolve_ques(
    data = data_clean,
    ques_cols = ques_cols,
    ques_stem = ques_stem,
    exclude_cols = c(wave_col, group_col, weight_col)
  )
  
  top_vals <- get_top_vals(
    data = data,
    ques_cols = ques_cols
  )

  facet_by <- match.arg(facet_by)

  coverage <- data_clean %>%
    select(all_of(c(wave_col, group_col, weight_col, ques_cols))) %>%
    group_by_at(c(wave_col, group_col)) %>%
    summarise(across(all_of(ques_cols), ~100*weighted.mean(as.numeric(.x) == top_vals[[cur_column()]], w = .data[[weight_col]], na.rm = TRUE)), .groups = "drop")

  coverage_long <- coverage %>%
    pivot_longer(cols = -any_of(c(wave_col, weight_col, group_col)), names_to = "variable", values_to = "pct_support") %>%
    filter(!is.na(pct_support)) %>%
    rename_at(vars(all_of(group_col)), ~"group_col") %>%
    filter(!is.na(group_col)) %>%
    mutate(variable = stringr::str_to_kebab(variable)) 
  if (length(unique(coverage_long$variable)) > 10) {
    warning("More than 10 variables to plot; heatmap may be hard to read.")
  }
  
  if (facet_by == "variable") {
    p <- coverage_long %>%
      ggplot(aes(y = group_col, x = .data[[wave_col]], fill = pct_support)) +
      geom_tile(color = "white")
    if (show_labels) {
      p <- p + geom_text(aes(label = sprintf("%.0f", pct_support),
                    color = ifelse(pct_support > 50, "black", "white")), size = 3)
    }
    p + 
      facet_grid(variable ~ ., scales = "free", space = "free") +
      scale_fill_viridis_b(name = "Percent support:") +
      scale_color_identity() +
      theme_bw() +
      labs("x" = "Year", "y" = group_col) +
      theme(legend.position = "top",
            strip.text.y.right = element_text(angle = 0))
  } else {
    p <- coverage_long %>%
      ggplot(aes(y = variable, x = .data[[wave_col]], fill = pct_support)) +
      geom_tile(color = "white")
    if (show_labels) {
      p <- p + geom_text(aes(label = sprintf("%.0f", pct_support),
                    color = ifelse(pct_support > 50, "black", "white")), size = 3)
    }
    p +
      facet_grid(group_col ~ ., scales = "free", space = "free") +
      scale_fill_viridis_b(name = "Percent support:") +
      scale_color_identity() +
      theme_bw() +
      labs("x" = "Year", "y" = "Item") +
      theme(legend.position = "top",
            strip.text.y.right = element_text(angle = 0))  
  }
}

#' Measure pairwise co-support for top responses.
#'
#' Computes the share of respondents who select the top response on pairs of
#' questions, optionally weighted.
#' @param data Data frame containing the survey items.
#' @param ques_cols Optional character vector of question columns to include.
#' @param ques_stem Optional vector of substrings/regex used to match columns.
#' @param weight_col Optional weight column; if missing, unweighted.
#' @param include_self Logical; include question self-pairs on the diagonal.
#' @param treat_na How to handle pairs of NA (exclude from percentages or treat as 'oppose')
#' @return Tibble with pairwise support statistics.
#' @export
measure_pairwise_support <- function(
  data,
  ques_cols = NULL,
  ques_stem = NULL,
  weight_col = NULL,
  include_self = TRUE,
  treat_na = c("exclude", "oppose")
) {
  treat_na <- match.arg(treat_na)
  
  weight_col <- if (!is.null(weight_col)) as.character(weight_col)[1] else NULL
  if (!is.null(weight_col) && !(weight_col %in% colnames(data))) {
    message("Weight column '", weight_col, "' not found; computing unweighted.")
    weight_col <- NULL
  }

  ques_cols <- resolve_ques(
    data = data,
    ques_cols = ques_cols,
    ques_stem = ques_stem,
    exclude_cols = weight_col
  )

  top_vals <- get_top_vals(
    data = data,
    ques_cols = ques_cols
  )

  drop_cols <- ques_cols[is.na(top_vals)]
  if (length(drop_cols) > 0) {
    message("Dropping questions with all NA values: ", paste(drop_cols, collapse = ", "))
    ques_cols <- setdiff(ques_cols, drop_cols)
    top_vals <- top_vals[ques_cols]
  }
  if (length(ques_cols) == 0) {
    stop("No non-missing question columns available for pairwise support.")
  }

  combs <- combn(ques_cols, 2, simplify = FALSE)
  if (include_self) {
    combs <- c(combs, purrr::map(ques_cols, ~c(.x, .x)))
  }

  data_use <- data %>%
    select(all_of(c(ques_cols, weight_col)))

  purrr::map_dfr(combs, function(pair) {
    q1 <- pair[1]
    q2 <- pair[2]

    if (treat_na == "exclude") {
      df_pair <- data_use %>%
        filter(!is.na(.data[[q1]]) & !is.na(.data[[q2]]))
      
      if (nrow(df_pair) == 0) {
        return(tibble(
          q1 = q1,
          q2 = q2,
          percent_q1_support = NA_real_,
          percent_q2_support = NA_real_,
          percent_both_support = NA_real_,
          n_pair = 0
        ))
      }

      top1 <- as.numeric(df_pair[[q1]]) == top_vals[[q1]]
      top2 <- as.numeric(df_pair[[q2]]) == top_vals[[q2]]
    } else {
      df_pair <- data_use
      top1 <- as.numeric(df_pair[[q1]]) %in% top_vals[[q1]]
      top2 <- as.numeric(df_pair[[q2]]) %in% top_vals[[q2]]
    }

    if (!is.null(weight_col)) {
      w <- df_pair[[weight_col]]
      percent_q1 <- weighted.mean(top1, w, na.rm = TRUE)
      percent_q2 <- weighted.mean(top2, w, na.rm = TRUE)
      percent_both <- weighted.mean(top1 & top2, w, na.rm = TRUE)
    } else {
      percent_q1 <- mean(top1, na.rm = TRUE)
      percent_q2 <- mean(top2, na.rm = TRUE)
      percent_both <- mean(top1 & top2, na.rm = TRUE)
    }

    tibble(
      q1 = q1,
      q2 = q2,
      percent_q1_support = percent_q1,
      percent_q2_support = percent_q2,
      percent_both_support = percent_both,
      n_pair = nrow(df_pair)
    )
  })
}

#' Plot pairwise co-support heatmap.
#'
#' Visualizes the share of respondents selecting the top response on pairs of
#' questions. Accepts either raw data or the output of
#' `measure_pairwise_support()`.
#' @param data Data frame containing survey items, or a precomputed results tibble.
#' @param ques_cols Optional character vector of question columns to include.
#' @param ques_stem Optional vector of substrings/regex used to match columns.
#' @param weight_col Optional weight column; if missing, unweighted.
#' @param include_self Logical; include question self-pairs on the diagonal.
#' @param show_labels Logical; draw percentage labels on tiles.
#' @return ggplot object.
#' @export
plot_pairwise_support <- function(
  data,
  ques_cols = NULL,
  ques_stem = NULL,
  weight_col = NULL,
  include_self = TRUE,
  show_labels = TRUE
) {
  results <- if (is.data.frame(data) &&
    all(c("q1", "q2", "percent_both_support") %in% colnames(data))) {
    data
  } else {
    measure_pairwise_support(
      data = data,
      ques_cols = ques_cols,
      ques_stem = ques_stem,
      weight_col = weight_col,
      include_self = include_self
    )
  }

  plot_data <- results %>%
    filter(!is.na(percent_both_support)) %>%
    mutate(
      q1 = stringr::str_to_kebab(q1),
      q2 = stringr::str_to_kebab(q2),
      pct = percent_both_support * 100
    )

  p <- plot_data %>%
    ggplot(aes(x = q1, y = q2, fill = pct)) +
    geom_tile(color = "grey") +
    scale_x_discrete(name = "") +
    scale_y_discrete(name = "") +
    scale_fill_viridis_c(name = "Percent support both:") +
    scale_color_identity(name = "Percent support both:") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")

  if (show_labels) {
    p <- p + geom_text(aes(label = sprintf("%.0f", pct),
                           color = ifelse(pct < 50, "white", "black")), size = 3)
  }
  p
}