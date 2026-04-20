

#' Plot cumulative issue majority vs individual issue majority by group
#'
#' For each group, plot support for items in descending order of individual
#' issue majority, overlaying the individual issue majority vs. the cumulative
#' issue majority (proportion supporting the majority on all issues up to and
#' including rank k).
#'
#' @param results Output from `measure_alignment()`.
#' @param show_response_value Logical; show response values in labels.
#' @param majority_cutoff Logical; trim items that fall below majority cumulative support in each group.
#' @return ggplot object.
#' @export
plot_cumulative_majority <- function(
    results,
    show_response_value = TRUE,
    majority_cutoff = FALSE
) {
  dat <- results$question_cumulative_pluralities |>
    rename_at(base::attr(results, "group_col", exact = TRUE), ~"group_col") |>
    mutate(group_col = str_to_title(group_col)) |>
    arrange(group_col, desc(prop_cumulative_plurality)) |>
    group_by(group_col) |>
    # keep all items where alignment except for first without alignment
    mutate(row_id = row_number()) |>
    mutate(keep = case_when(
      !majority_cutoff ~ TRUE,
      majority_cutoff & prop_cumulative_plurality >= 0.5 ~ TRUE, 
      majority_cutoff & prop_cumulative_plurality < 0.5 ~ FALSE,
      .default = TRUE
    )) |>
    mutate(first_below = if_else(!keep & suppressWarnings(row_id == min(row_id[!keep])), TRUE, FALSE)) |>
    filter(keep | first_below) |>
    select(-keep, -first_below, -row_id) |>
    ungroup() |>
    # prep for plot
    mutate(question = tolower(gsub("\\_", "-", question))) |>
    mutate(label = paste(question, plurality_response, sep = ": "))
  dat <- dat |>
    mutate(
      label = paste(question, plurality_response, sep = ": "),
      group_label = interaction(group_col, label, drop = TRUE, sep = "."),
      group_label = factor(
        group_label,
        levels = dat |>
          arrange(group_col, (prop_cumulative_plurality)) |>
          mutate(group_label = interaction(group_col, paste(question, plurality_response, sep = ": "))) |>
          pull(group_label)
      )
    )
  ggplot(dat, aes(y = group_label)) +
    geom_bar(stat = "identity", aes(x = prop_response_wtd, 
                                    fill = "For single\nquestion")) + 
    geom_bar(stat = "identity", aes(x = prop_cumulative_plurality, 
                                    fill = "For question\nand all above")) +
    geom_vline(xintercept = 0.5, color = "white", lty = 2) +
    scale_fill_manual(values = c("grey", "black"), name = "Support:") +
    scale_x_continuous(labels = scales::percent_format(1), name = "Percent support") +
    scale_y_discrete(labels = function(labels) {
      if (show_response_value)
        str_replace(labels, '^.*\\.', '')
      else
        str_replace(labels, '^.*\\.', '') |> str_replace("\\:.*$", '')
    }) +
    facet_grid(group_col ~ ., scales = "free", space = "free") +
    labs(x = "", y = NULL) +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle = 0, size = rel(1.25)))
}

#' Plot alignment distributions and summary means by group.
#'
#' @param results Output from `measure_alignment()`.
#' @param exclude_vals Group values to exclude.
#' @param show_dists_as How to visualize distributions.
#' @param show_mean Logical; show mean estimates and intervals.
#' @param ci_scale Multiplier for confidence intervals.
#' @return ggplot object.
#' @export
plot_individual_alignments <- function(
    results, 
    exclude_vals = "DK/REF", 
    show_dists_as = c("histogram","density","none"),
    show_mean = TRUE,
    ci_scale = 1.96
) {
  show_dists_as <- match.arg(show_dists_as)
  data <- results$respondent_alignment |> 
    filter_at(base::attr(results, "group_col", exact = TRUE), ~!(.x %in% exclude_vals)) |>
    rename_at(base::attr(results, "group_col", exact = TRUE), ~"g") |>
    group_by(g) |>
    mutate(val_label = g) |>
    # mutate(val_label = paste0(trimws(g, whitespace = "[ \t\r\n“”]"),"\n(n=",n(),")")) |>
    ungroup()
  plot <- data |>
    ggplot(aes(x = prop_questions_aligned)) +
    scale_y_continuous(name = "") +
    scale_x_continuous(labels = scales::percent_format(1), 
                       name = "% of responses aligned with group majority across all issues")
  
  if (show_dists_as=="density") {
    plot <- plot +
      geom_density(fill="grey", color="grey")
  } else if (show_dists_as=="histogram") {
    plot <- plot +
      geom_histogram(fill="grey", color="grey", bins = 10)
  } 
  plot <- plot +
    scale_fill_discrete(name = base::attr(results, "group_label", exact = TRUE)) +
    scale_color_discrete(name = base::attr(results, "group_label", exact = TRUE))
  
  if (show_mean) {
    plot <- plot +
      geom_pointrange(data = data |>
                        group_by(g, val_label) |>
                        summarise(mean_prop = mean(prop_questions_aligned, na.rm=T),
                                  se_prop = sd(prop_questions_aligned, na.rm=T)/sqrt(n()),
                                  ci_upr = mean_prop + ci_scale*se_prop,
                                  ci_lwr = mean_prop - ci_scale*se_prop,
                                  .groups = "drop"),
                      aes(x=mean_prop, xmin=ci_lwr, xmax=ci_upr, y=0), color="black") 
  }
  
  plot + 
    facet_grid(val_label ~ .) + {
      if (show_mean & !(show_dists_as %in% c("density","histogram")))
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
#' @param results Output from `measure_alignment()`.
#' @param group_label Optional label for legend.
#' @param group_colors Optional manual colors.
#' @param exclude_vals Group values to exclude.
#' @param resolution Vector of quantiles to evaluate.
#' @param interactive Logical; if TRUE, return an interactive plotly widget with
#'   per-point tooltips. Requires the \code{plotly} package.
#' @return A ggplot object, or a plotly widget when \code{interactive = TRUE}.
#' @export
plot_alignment_curve <- function(
    results, 
    group_label = base::attr(results, "group_label", exact = TRUE),
    group_colors = NULL,
    exclude_vals = "DK/REF", 
    resolution = seq(0, 1, by = 0.01),
    interactive = FALSE
) {
  binarized <- !all(is.null(base::attr(results, "binarized_cols", exact = TRUE)))
  grp_col <- base::attr(results, "group_col", exact = TRUE)
  cumul_align <- resolution |>
    map(~results$respondent_alignment |>
          filter(!(.data[[grp_col]] %in% exclude_vals)) |>
          group_by(across(all_of(grp_col))) |>
          summarise(q = .x,
                    p = mean(prop_questions_aligned >= .x, na.rm=T),
                    n = n(),
                    .groups = "drop")) |>
    bind_rows() |>
    mutate(val_label = as.character(.data[[grp_col]]))

  cumul_align <- cumul_align |>
    bind_rows(
      cumul_align |>
        group_by(across(all_of(grp_col))) |>
        summarise(q = 1, p = 0, n = first(n), val_label = first(val_label), .groups = "drop")
    )

  plot_data <- cumul_align |>
    distinct(across(all_of(c(grp_col, "p"))), .keep_all = TRUE) |>
    mutate(
      tooltip = sprintf(
        "<b>%s%%</b> of %s respondents\nagree with the %s majority\non <b>%s%% or more</b> of issues",
        round(p * 100, 1), val_label, val_label, round(q * 100, 1)
      )
    )

  # x = alignment threshold (q); y = share of respondents meeting that threshold (p)
  p <- plot_data |>
    ggplot(aes(x = q, y = p,
               color = val_label,
               group = val_label,
               shape = val_label,
               linetype = val_label,
               text = tooltip)) +
    geom_point() +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = 0.5, alpha = 0.5) +
    geom_hline(yintercept = 0.5, alpha = 0.5)

  if (!is.null(group_colors)) {
    p <- p + scale_color_manual(values = group_colors, name = paste0(group_label, ":"))
  } else {
    p <- p + scale_color_brewer(palette = "Set1", name = paste0(group_label, ":"))
  }
  p <- p +
    scale_shape_discrete(name = paste0(group_label, ":")) +
    scale_linetype_manual(
      values = c("solid", "dotted", "dotdash", "longdash", "twodash", "dashdot"),
      name = paste0(group_label, ":")
    ) +
    scale_x_continuous(
      labels = scales::percent_format(1),
      limits = c(0, 1),
      name = "...who support % (or more) of group majority positions"
      # name = "Alignment threshold: % (or more) of group majority positions supported"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(1),
      limits = c(0, 1),
      name = "% of respondents..."
      # name = "% of respondents meeting threshold"
    ) +
    theme_bw()

  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop(
        "Package 'plotly' is required for interactive = TRUE. ",
        "Install it with: install.packages(\"plotly\")",
        call. = FALSE
      )
    }
    p <- p + theme(legend.position = "bottom")
    # geom_vline/geom_hline have no 'text' aesthetic and cause ggplotly errors;
    # strip them and re-add as plotly layout shapes instead.
    p$layers <- p$layers[
      !sapply(p$layers, function(l) inherits(l$geom, c("GeomVline", "GeomHline")))
    ]
    pl <- plotly::ggplotly(p, tooltip = "text")
    pl <- plotly::layout(pl, shapes = list(
      list(type = "line", xref = "x", yref = "paper",
           x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
           line = list(color = "rgba(0,0,0,0.4)")),
      list(type = "line", xref = "paper", yref = "y",
           x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
           line = list(color = "rgba(0,0,0,0.4)"))
    ))
    return(pl)
  }

  p
}

#' Plot group-level metrics over time.
#'
#' @param results_list Named list of results by wave/year.
#' @param metric Column in group_stats to plot.
#' @param wave_col Output column name for waves/years.
#' @param group_col Output column name for groups.
#' @param group_label Legend label for group colors.
#' @param group_colors Optional manual colors.
#' @return ggplot object.
#' @export
plot_group_stat_over_time <- function(
    results_list, 
    metric = c(
      "cumulative_perfect_alignment",  
      "cumulative_weak_alignment",
      "alignment_mean",
      "cumulative_issue_alignment_prop"
    ),
    wave_col = "year",
    group_col = "group",
    group_label = "Group",
    group_colors = NULL
) {
  wave_col <- as.character(wave_col)[1]
  group_col <- as.character(group_col)[1]
  metric <- match.arg(metric)
  
  # Build long table of group stats across periods
  group_stats_t <- purrr::imap_dfr(results_list, function(result, period_label) {
    if (!is.list(result) || is.null(result$group_stats)) return(NULL)
    result_group_col <- base::attr(result, "group_col", exact = TRUE)
    if (is.null(result_group_col) || !(result_group_col %in% colnames(result$group_stats))) return(NULL)
    result$group_stats |>
      dplyr::rename(!!rlang::sym(group_col) := !!rlang::sym(result_group_col)) |>
      dplyr::mutate(!!rlang::sym(wave_col) := period_label)
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
    lab <- base::attr(sample_result$group_stats[[metric]], "label", exact = TRUE)
    if (!is.null(lab) && nzchar(lab)) lab else ""
  } else {
    ""
  }
  y_label <- paste0(y_label, "*")
  cap_label <- if (!is.null(sample_result)) {
    lab <- base::attr(sample_result$group_stats[[metric]], "description", exact = TRUE)
    if (!is.null(lab) && nzchar(lab)) lab else ""
  } else {
    ""
  }
  cap_label <- paste0("*", cap_label)
  
  # Prepare plotting data
  plot_data <- group_stats_t
  # if (!is.null(yrs)) {
  #   if (year_col %in% colnames(plot_data)) {
  #     plot_data <- plot_data |> filter(!!rlang::sym(year_col) %in% yrs)
  #   }
  # }
  
  # Convert wave for plotting (if possible)
  plot_data <- plot_data |> mutate(!!rlang::sym(wave_col) := as.numeric(!!rlang::sym(wave_col)))
  
  p <- ggplot(plot_data, aes(x = !!rlang::sym(wave_col), y = !!rlang::sym(metric), color = !!rlang::sym(group_col), group = !!rlang::sym(group_col))) +
    geom_point(aes(size = .data$n_respondents)) +
    geom_line() +
    geom_hline(yintercept = 0.5, lty = 2, alpha = 0.5) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(labels = scales::percent_format(1), name = ifelse(nzchar(y_label), y_label, metric)) +
    #facet_grid(as.formula(paste0(group_col, " ~ .")), scales = "free_y") +
    scale_size(name = "Sample Size:", guide = "none")
  
  # Add ±2 SE ribbon when plotting alignment_mean and alignment_se is available
  if (metric == "alignment_mean" && "alignment_se" %in% colnames(plot_data)) {
    p <- p + geom_ribbon(
      aes(
        ymin = .data$alignment_mean - 2 * .data$alignment_se,
        ymax = .data$alignment_mean + 2 * .data$alignment_se,
        fill  = !!rlang::sym(group_col)
      ),
      alpha = 0.15,
      color = NA
    )
  }
  
  if (any(!is.null(group_colors))) {
    p <- p + 
      scale_color_manual(values = group_colors, name = paste0(group_label, ":")) +
      scale_fill_manual(values = group_colors, name = paste0(group_label, ":"))
  } else {
    p <- p + 
      scale_color_discrete(name = paste0(group_label, ":")) +
      scale_fill_discrete(name = paste0(group_label, ":"))
  }
  
  p <- p +
    labs(caption=cap_label) +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle = 0, size = rel(1.25)))
  
  return(p)
}
