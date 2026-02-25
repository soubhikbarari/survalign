#' Read CES Common Columns
#'
#' This function reads in common column details for a given year from a CSV file.
#'
#' @param yyyy A four-digit integer representing the year for which to read column data.
#'
#' @return A tibble with two character columns: `var` and `label`.
#' If the file does not exist or there is an error reading it, an empty tibble is returned.
read_ces_common_columns <- function(yyyy) {
  path <- here::here("ques", sprintf("cols_ces_%s_common.csv", yyyy))
  if (!file.exists(path)) {
    return(tibble::tibble(var = character(), label = character()))
  }
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(
      var = readr::col_character(),
      label = readr::col_character()
    )),
    error = function(e) {
      warning("Unable to read ", path, ": ", e$message)
      tibble::tibble(var = character(), label = character())
    }
  )
}

match_label_record <- function(records, data_cols, exact = NULL, pattern = NULL, exclude = NULL) {
  if (is.null(records) || nrow(records) == 0) {
    return(NULL)
  }
  records <- as.data.frame(records, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(records))) {
    var <- records$var[i]
    if (!is.null(data_cols) && !(var %in% data_cols)) {
      next
    }
    label <- records$label[i]
    if (is.null(label) || is.na(label)) {
      label <- ""
    }
    label_clean <- trimws(tolower(label))
    if (!is.null(exclude) && nzchar(exclude) && grepl(exclude, label_clean, perl = TRUE)) {
      next
    }
    if (!is.null(exact) && label_clean == exact) {
      return(list(var = var, label = records$label[i]))
    }
    if (!is.null(pattern) && grepl(pattern, label_clean, perl = TRUE)) {
      return(list(var = var, label = records$label[i]))
    }
  }
  NULL
}

resolve_demo_column <- function(year, label_lookup, data_label_lookup, data_cols, combos, name) {
  for (combo in combos) {
    result <- match_label_record(label_lookup, data_cols, combo$exact, combo$pattern, combo$exclude)
    if (!is.null(result)) {
      return(result)
    }
  }
  for (combo in combos) {
    result <- match_label_record(data_label_lookup, data_cols, combo$exact, combo$pattern, combo$exclude)
    if (!is.null(result)) {
      message("[", year, "] Using dataset labels for ", name, " column (", result$label, ", ", result$var, ")")
      return(result)
    }
  }
  stop("Could not identify ", name, " column for ", year)
}

label_to_character <- function(vec) {
  if (is.null(vec)) {
    return(NA_character_)
  }
  if (haven::is.labelled(vec)) {
    vec <- haven::as_factor(vec)
  }
  if (is.factor(vec)) {
    return(as.character(vec))
  }
  if (is.character(vec)) {
    return(vec)
  }
  as.character(vec)
}

standardize_education_label <- function(label) {
  label_clean <- stringr::str_to_lower(stringr::str_squish(label))
  dplyr::case_when(
    stringr::str_detect(label_clean, "less than|no high school|did not finish|didn't finish|no diploma|no hs") ~ "Less than HS",
    stringr::str_detect(label_clean, "postgrad|post-grad|master|mba|professional|phd|doctorate") ~ "Post-grad",
    stringr::str_detect(label_clean, "some college|2-year|two-year|two year|associate|aa|a\\.a\\.|a\\.s\\.") ~ "Some college",
    stringr::str_detect(label_clean, "college|bachelor|ba|bs|b\\.a\\.|b\\.s\\.|4-year|4 year|four-year|four year") &
      !stringr::str_detect(label_clean, "some college") ~ "College",
    stringr::str_detect(label_clean, "high school|ged") & !stringr::str_detect(label_clean, "some") ~ "High school",
    TRUE ~ NA_character_
  )
}

standardize_race_label <- function(label) {
  label_clean <- stringr::str_to_lower(stringr::str_squish(label))
  dplyr::case_when(
    stringr::str_detect(label_clean, "hispanic|latino") ~ "hispanic",
    stringr::str_detect(label_clean, "black|african american") ~ "black",
    stringr::str_detect(label_clean, "asian") ~ "asian",
    stringr::str_detect(label_clean, "white") &
      !stringr::str_detect(label_clean, "hispanic") ~ "non-hispanic white",
    stringr::str_detect(label_clean, "other|multiracial|mixed|native american|american indian|alaska|pacific") ~ "other",
    TRUE ~ "other"
  )
}

age_combos <- list(
  list(exact = "birth year"),
  list(pattern = "birth.*year"),
  list(pattern = "year were you born"),
  list(pattern = "born.*year"),
  list(pattern = "birthyr")
)
educ_combos <- list(
  list(exact = "education"),
  list(pattern = "education")
)
race_combos <- list(
  list(exact = "race"),
  list(pattern = "race", exclude = "candidate|house|senate|member|representative|district|state|current|party|control|other")
)

plot_yearly_coverage_heatmap <- function(data) {
  coverage <- data %>%
    select(-any_of(c("case_id", "pid3", "age", "age4", "education", "race", "weight"))) %>%
    group_by(year) %>%
    summarise(across(everything(), ~sum(!is.na(.x))))

  coverage %>%
    pivot_longer(cols = -year, names_to = "variable", values_to = "n_nonmissing") %>%
    filter(n_nonmissing > 0) %>%
    ggplot(aes(y = variable, x = year, fill = n_nonmissing/1000)) +
    geom_tile(color = "white") +
    scale_fill_viridis_b(name = "N non-missing (x 1K)") +
    theme_bw() +
    theme(legend.position = "top")
}

summarize_yearly_coverage <- function(data) {
  coverage <- data %>%
    select(-any_of(c("case_id", "pid3", "age", "age4", "education", "race", "weight"))) %>%
    group_by(year) %>%
    summarise(across(everything(), ~sum(!is.na(.x))))

  coverage %>%
    pivot_longer(cols = -year, names_to = "variable", values_to = "n_nonmissing") %>%
    filter(n_nonmissing > 0) %>%
    group_by(variable) %>%
    summarise(years = paste0(sort(year), collapse = ", "), n_years = n()) %>%
    arrange(desc(n_years), variable)
}

plot_yearly_support_heatmap <- function(data, ques_rgx, group_var = "pid3", support_val = 1, facet_by = c("variable", "group_var")) {
  if (is.null(group_var)){
    group_var <- "all"
    data$all <- "all"
  }
  facet_by <- match.arg(facet_by)

  coverage <- data %>%
    select(-matches("_orig")) %>%
    select(year, all_of(group_var), weight, matches(ques_rgx)) %>%
    group_by_at(c("year", group_var)) %>%
    summarise(across(everything(), ~100*weighted.mean(.x == support_val, w = weight, na.rm = TRUE)), .groups = "drop")

  coverage_long <- coverage %>%
    pivot_longer(cols = -any_of(c("year", "weight", group_var)), names_to = "variable", values_to = "pct_support") %>%
    filter(!is.na(pct_support)) %>%
    rename_at(vars(all_of(group_var)), ~"group_var") %>%
    filter(!is.na(group_var)) %>%
    mutate(variable = stringr::str_to_kebab(variable)) 
  if (length(unique(coverage_long$variable)) > 10) {
    warning("More than 10 variables to plot; heatmap may be hard to read.")
  }
  
  if (facet_by == "variable") {
    coverage_long %>%
      ggplot(aes(y = group_var, x = year, fill = pct_support)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.0f", pct_support),
                    color = ifelse(pct_support > 50, "black", "white")), size = 3) +
      facet_grid(variable ~ ., scales = "free", space = "free") +
      scale_fill_viridis_b(name = "Percent support:") +
      scale_color_identity() +
      theme_bw() +
      labs("x" = "Year", "y" = group_var) +
      theme(legend.position = "top",
            strip.text.y.right = element_text(angle = 0))
  } else {
    coverage_long %>%
      ggplot(aes(y = variable, x = year, fill = pct_support)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.0f", pct_support),
                    color = ifelse(pct_support > 50, "black", "white")), size = 3) +
      facet_grid(group_var ~ ., scales = "free", space = "free") +
      scale_fill_viridis_b(name = "Percent support:") +
      scale_color_identity() +
      theme_bw() +
      labs("x" = "Year", "y" = "Item") +
      theme(legend.position = "top",
            strip.text.y.right = element_text(angle = 0))  
  }
}
