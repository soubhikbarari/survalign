setwd("~/Research_Group Dropbox/Soubhik Barari/Projects/repos/survalign/")

source("step0/utils.R")
source("step0/ces_utils.R")

message("Preparing CES")

# Read Data ---------------------------------------------------------------

attitudes_class <- "sociopolitical attitudes or positions (not news consumption or behaviors)"

ces_files <- list.files("data-raw", pattern = "ces_\\d{4}_common\\.dta", full.names = TRUE)

if (!exists("ces_list")) {
  ces_list <- purrr::map(1:length(ces_files), .progress=TRUE, function(i){
    message("Reading ", ces_files[i])
    tryCatch({
      if (i == 2) ### 2009 file needs haven
        df <- foreign::read.dta(ces_files[i])
      else
        df <- haven::read_dta(ces_files[i])
    }, error = function(e) {
      df <- foreign::read.dta(ces_files[i])
    })
    return(df)
  })
  names(ces_list) <- stringr::str_extract(basename(ces_files), "\\d{4}")
} else {
  message("Using existing ces_list in environment")
}

ces_common_demo_labels <- setNames(
  purrr::map(names(ces_list), function(yyyy) {
    read_ces_common_columns(yyyy)
  }),
  names(ces_list)
)

ces_cumul <- read_dta(here("data-raw/cumulative_ces_policy_preferences.dta"))
colnames(ces_cumul)

ces_cumul_labs <- readxl::read_excel("data-raw/ques/cols_cumulative_ces_policy_preferences.xlsx")

# Process Data ------------------------------------------------------------

if (!exists("ces_ids")) {
  ces_ids <- purrr::map_dfr(1:length(ces_files), .progress = TRUE, function(i) {
    year <- names(ces_list)[i]
    df_sub <- ces_list[[i]]
    cols <- colnames(df_sub)
    col_labels <- vapply(cols, function(col) {
      label <- attr(df_sub[[col]], "label")
      if (is.null(label)) {
        ""
      } else {
        paste0(as.character(label), collapse = "; ")
      }
    }, FUN.VALUE = character(1))
    
    col_label_lookup <- tibble::tibble(var = cols, label = col_labels)
    label_lookup <- ces_common_demo_labels[[year]]
    
    if (year == "2009") {
      age_info <- list(var = "v207", label = "Birth year (hard-coded)")
      educ_info <- list(var = "v213", label = "Education (hard-coded)")
      race_info <- list(var = "v211", label = "Race (hard-coded)")
    } else {
      age_info <- resolve_demo_column(year, label_lookup, col_label_lookup, cols, age_combos, "age")
      educ_info <- resolve_demo_column(year, label_lookup, col_label_lookup, cols, educ_combos, "education")
      race_info <- resolve_demo_column(year, label_lookup, col_label_lookup, cols, race_combos, "race")
    }
    
    message(
      "[", year, "] age=", age_info$var, " (", age_info$label, ")",
      " education=", educ_info$var, " race=", race_info$var
    )
    
    stopifnot(
      age_info$var %in% cols,
      educ_info$var %in% cols,
      race_info$var %in% cols
    )
    
    birth_raw <- df_sub[[age_info$var]]
    birth_year <- as.numeric(haven::zap_labels(birth_raw))
    year_num <- as.numeric(year)
    valid_birth <- !is.na(birth_year) & birth_year > 1800 & birth_year <= year_num
    birth_year[!valid_birth] <- NA_real_
    age_value <- ifelse(is.na(birth_year), NA_real_, year_num - birth_year)
    
    age4_value <- dplyr::case_when(
      age_value >= 18 & age_value <= 29 ~ "18-29",
      age_value >= 30 & age_value <= 49 ~ "30-49",
      age_value >= 50 & age_value <= 64 ~ "50-64",
      age_value >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
    age4_value <- factor(age4_value, levels = c("18-29", "30-49", "50-64", "65+"))
    
    education_value_raw <- label_to_character(df_sub[[educ_info$var]])
    race_value_raw <- label_to_character(df_sub[[race_info$var]])
    education_value <- standardize_education_label(education_value_raw)
    race_value <- standardize_race_label(race_value_raw)
    
    if (year %in% c("2008", "2010")) {
      id_col <- "V100"
    } else if (year %in% c("2009")) {
      id_col <- "v100"
    } else if ("caseid" %in% cols) {
      id_col <- "caseid"
    } else if ("V100" %in% cols) {
      id_col <- "V100"
    } else {
      id_cols <- col_labels[grepl("(panelist ID|case ID)", col_labels, ignore.case = TRUE)][1]
      id_col <- names(id_cols)[1]
    }
    
    print(col_labels[grepl("party ID", col_labels, ignore.case = TRUE)])
    
    if (year == "2008") {
      pid_col <- "CC307"
    } else if (year == "2009") {
      pid_col <- "cc423"
    } else if (year %in% c("2010", "2011")) {
      pid_col <- "V212a"
    } else if ("pid3" %in% cols) {
      pid_col <- "pid3"
    } else {
      stop("")
    }
    if (!(pid_col %in% colnames(df_sub))) {
      browser()
    }
    
    if ("commonweight" %in% cols) {
      wgt_col <- "commonweight"
    } else if ("weight" %in% cols) {
      wgt_col <- "weight"
    } else {
      wgt_col <- switch(year,
                        "2008" = "V201",
                        "2009" = "v200",
                        "2010" = "V101",
                        "2011" = "V101",
                        "2012" = "V103",
                        "2017" = "weights_common"
      )
    }
    
    df_sub %>%
      rename_at(id_col, ~"case_id") %>%
      rename_at(pid_col, ~"pid3") %>%
      rename_at(wgt_col, ~"weight") %>%
      mutate(pid3 = case_when(
        grepl("demo", pid3, ignore.case = TRUE) | pid3 %in% c("1", 1) ~ "Democrat",
        grepl("repub", pid3, ignore.case = TRUE) | pid3 %in% c("2", 2) ~ "Republican",
        grepl("(indep|other)", pid3, ignore.case = TRUE) | pid3 %in% c("3", 3) ~ "Independent/Other",
      )) %>%
      mutate(
        age = age_value,
        age4 = age4_value,
        education = education_value,
        race = race_value
      ) %>%
      unlabelled() %>%
      select(case_id, pid3, age, age4, education, race, weight) %>%
      mutate(year = year_num)
  })
} else {
  message("Using existing ces_ids in environment")
}

table(ces_ids$pid3, useNA = "always")

ces_cumul_id <- ces_cumul %>%
  mutate(case_id = as.numeric(case_id)) %>%
  inner_join(ces_ids, by = join_by(year, case_id)) %>%
  select(year, case_id, pid3, age, age4, education, race, weight, everything())

# Patch Data --------------------------------------------------------------

ces_cumul_id <- ces_cumul_id %>%
  # some instances of miscodings
  mutate(
    gaymarriage_legalize = case_when(
      year == 2009 ~ gaymarriage_ban,
      TRUE ~ gaymarriage_legalize
    ),
    gaymarriage_ban = case_when(
      year == 2009 ~ NA,
      TRUE ~ gaymarriage_ban
    ),
    healthcare_aca = case_when(
      year == 2014 & healthcare_aca == 1 ~ 2,
      year == 2014 & healthcare_aca == 2 ~ 1,
      TRUE ~ healthcare_aca
    ),
    enviro_mpg_raise = case_when(
      year == 2018 & enviro_mpg_raise == 1 ~ 2,
      year == 2018 & enviro_mpg_raise == 2 ~ 1,
      TRUE ~ enviro_mpg_raise      
    )
  )

ces_cumul_id %>% 
  filter(year == 2009) %>%
  count(pid3, gaymarriage_legalize)

# Add New Years -----------------------------------------------------------

build_year_cumul <- function(yyyy) {
  var_col <- paste0("var", yyyy)
  cols_df <- ces_cumul_labs %>%
    select(all_of(var_col), category, variable) %>%
    rename(varname = all_of(var_col)) %>%
    filter(!is.na(varname)) %>%
    as.data.frame()
  col_lookup <- setNames(cols_df$varname, cols_df$variable)
  
  df_year <- ces_list[[as.character(yyyy)]] %>%
    mutate(year = yyyy)
  
  birth_raw <- df_year$birthyr
  birth_year <- as.numeric(haven::zap_labels(birth_raw))
  year_num <- as.numeric(yyyy)
  valid_birth <- !is.na(birth_year) & birth_year > 1800 & birth_year <= year_num
  birth_year[!valid_birth] <- NA_real_
  age_value <- ifelse(is.na(birth_year), NA_real_, year_num - birth_year)
  
  age4_value <- dplyr::case_when(
    age_value >= 18 & age_value <= 29 ~ "18-29",
    age_value >= 30 & age_value <= 49 ~ "30-49",
    age_value >= 50 & age_value <= 64 ~ "50-64",
    age_value >= 65 ~ "65+",
    TRUE ~ NA_character_
  )
  age4_value <- factor(age4_value, levels = c("18-29", "30-49", "50-64", "65+"))
  
  education_value_raw <- label_to_character(df_year$educ)
  race_value_raw <- label_to_character(df_year$race)
  education_value <- standardize_education_label(education_value_raw)
  race_value <- standardize_race_label(race_value_raw)
  
  df_year <- df_year %>%
    rename_at("commonweight", ~"weight") %>%
    rename_at("caseid", ~"case_id") %>%
    mutate(
      age = age_value,
      age4 = age4_value,
      education = education_value,
      race = race_value
    ) %>%
    mutate(pid3 = case_when(
      grepl("demo", pid3, ignore.case = TRUE) | pid3 %in% c("1", 1) ~ "Democrat",
      grepl("repub", pid3, ignore.case = TRUE) | pid3 %in% c("2", 2) ~ "Republican",
      grepl("(indep|other)", pid3, ignore.case = TRUE) | pid3 %in% c("3", 3, "4", 4) ~ "Independent/Other",
      TRUE ~ NA
    )) %>%
    select_at(c("case_id", "year", "pid3", "education", "race", "age", "age4", "weight", cols_df$varname)) %>%
    rename(!!!col_lookup)
  
  attr(df_year, "col_lookup") <- col_lookup
  df_year
}

ces_2022_cumul <- build_year_cumul(2022)
ces_2023_cumul <- build_year_cumul(2023)
ces_2024_cumul <- build_year_cumul(2024)

# Remove any existing rows from these years, then bind the new ones
ces_cumul_id <- ces_cumul_id %>% filter(!(year %in% c(2022, 2023, 2024)))
ces_cumul_id <- suppressWarnings(bind_rows(
  ces_cumul_id,
  ces_2022_cumul,
  ces_2023_cumul,
  ces_2024_cumul
))

# quick checks (same as before)
ces_cumul_id %>% select(military_terroristcamp)
ces_2024_cumul %>% select(military_terroristcamp)

# Add Labels --------------------------------------------------------------

for (i in 2:nrow(ces_cumul_labs)) {
  attr(ces_cumul_id[[ces_cumul_labs$variable[i]]], "label") <- ces_cumul_labs$label[i]
}

# Order Column Labels -----------------------------------------------------

ces <- ces_cumul_id %>%
  rename(educ5 = education,
         race5 = race) %>%
  select(-matches("scale"), -matches("_vs_"), -matches("_cuts_")) %>%
  mutate(
    race5 = haven::labelled(
      case_when(
        is.na(race5) | race5 == ".r" ~ haven::tagged_na('r'),
        TRUE ~ as.numeric(fct_inorder(race5))
      ),
      labels = c(
        "White (Non-Hispanic)" = 1,
        "Black" = 2,
        "Hispanic" = 3,
        "Asian" = 4,
        "Other" = 5,
        "Nonresponse" = haven::tagged_na('r')
      ),
      label = "Race (5)"
    ),
    educ5 = haven::labelled(
      case_when(
        is.na(educ5) | educ5 == ".r" ~ haven::tagged_na('r'),
        TRUE ~ as.numeric(fct_inorder(educ5))
      ),
      labels = c(
        "Less than HS" = 1,
        "High school" = 2,
        "Some college" = 3,
        "College" = 4,
        "Post-grad" = 5,
        "Nonresponse" = haven::tagged_na('r')
      ),
      label = "Education (5)"
    ),
    pid3 = haven::labelled(
      case_when(
        is.na(pid3) | pid3 == ".r" ~ haven::tagged_na('r'),
        TRUE ~ as.numeric(fct_inorder(pid3))
      ),
      labels = c(
        "Democrat" = 1,
        "Republican" = 2,
        "Independent/Other" = 3,
        "Nonresponse" = haven::tagged_na('r')
      ),
      label = "Party (3)"
    ),
    age4 = haven::labelled(
      case_when(
        is.na(age4) | age4 == ".r" ~ haven::tagged_na('r'),
        TRUE ~ as.numeric(fct_inorder(age4))
      ),
      labels = c(
        "18-29" = 1,
        "30-49" = 2,
        "50-64" = 3,
        "65+" = 4,
        "Nonresponse" = haven::tagged_na('r')
      ),
      label = "Age (4)"
    )
  )

## change my mind, let's keep group vars unlabelled
ces <- ces %>%
  mutate(pid3 = labelled::unlabelled(pid3),
         age4 = labelled::unlabelled(age4),
         educ5 = labelled::unlabelled(educ5),
         race5 = labelled::unlabelled(race5))

for (ci in colnames(ces)[9:ncol(ces)]) {
  u <- unique(ces[[ci]])
  if (any(u %in% c(3,4,5))) {
    message(ci)
    print(u)
    
    if (ci == "affirmativeaction") {
      ces <- ces %>%
        mutate(affirmativeaction = case_when(
          affirmativeaction %in% c(1, 2) ~ 1,
          affirmativeaction %in% c(3, 4) ~ 0,
          TRUE ~ as.numeric(affirmativeaction)
        ))
    }
    if (grepl("spending_", ci) & all(1:5 %in% u)) {
      ces[[ci]] <- case_when(
        ces[[ci]] %in% 1:2 ~ 1,
        ces[[ci]] %in% 3:5 ~ 0,
        TRUE ~ as.numeric(ces[[ci]])
      )
    }
  }
}

ces <- ces %>%
  group_by(year) %>%
  mutate(across(
    9:ncol(.),
    ~ case_when(
      is.na(.x) ~ ifelse(
        all(is.na(.x)),
        haven::tagged_na('s'),
        haven::tagged_na('r')
      ),
      .x == 2 ~ 0,
      .x == 8 ~ haven::tagged_na('a'),
      .x == 9 ~ haven::tagged_na('b'),
      TRUE ~ .x
    ) |> haven::labelled(
      labels = c(
        "Oppose" = 0,
        "Support" = 1,
        "Skipped" = haven::tagged_na('a'),
        "Not asked (respondent)" = haven::tagged_na('b'),
        "Not asked (survey)" = haven::tagged_na('s'),
        "Nonresponse" = haven::tagged_na('r')
      ),
      label = attr(.x, "label")
    )
  )) %>%
  ungroup()

for (ci in colnames(ces)[9:ncol(ces)]) {
  message(ci)
  print(table(ces[[ci]]))
}

# Attach Labels -----------------------------------------------------------

labeldata <- read_csv("data-raw/ques/cols_cumulative_ces_policy_preferences.csv")
colnames(ces)
for (ci in colnames(ces)[9:ncol(ces)]) {
  message(ci)
  row <- labeldata %>% filter(variable == ci)
  if (nrow(row) == 1) {
    attr(ces[[ci]], "label") <- row$description
    attr(ces[[ci]], "category") <- row$category
    attr(ces[[ci]], "shortlabel") <- row$shortlabel
  }
  print(table(ces[[ci]]))
}
ces <- ces %>% rename(id = case_id)

ces$year <- as.factor(ces$year)
ces$id <- as.factor(ces$id)

attr(ces$year, "label") <- "Survey Year"
attr(ces$id, "label") <- "Respondent ID"
attr(ces$weight, "label") <- "Survey Weight (U.S. Adult)"

attr(ces$pid3, "label") <- "Party (3)"
attr(ces$age, "label") <- "Age"
attr(ces$age4, "label") <- "Age (4)"
attr(ces$educ5, "label") <- "Education (5)"
attr(ces$race5, "label") <- "Race (5)"

attr(ces$weight, "weight_var") <- TRUE
attr(ces$pid3, "group_var") <- TRUE
attr(ces$age, "group_var") <- TRUE
attr(ces$age4, "group_var") <- TRUE
attr(ces$educ5, "group_var") <- TRUE
attr(ces$race5, "group_var") <- TRUE

# Get Spending Items ------------------------------------------------------

#colnames(ces)[grepl("spending_", colnames(ces))]

# ces_spending_years <- intersect(seq(2014, 2024, 2), sort(unique(ces_cumul_id$year)))
ces_spending_items <- ces_cumul_items[stringr::str_starts(ces_cumul_items, "spending_")]

ces_spending_items_summary <- tibble::tibble()
ces_spending_items_covered <- character()

# if (length(ces_spending_items) > 0 && length(ces_spending_years) > 0) {
#   ces_spending_items_summary <- ces_cumul_id %>% 
#     summarize_yearly_coverage() %>%
#     filter(variable %in% ces_spending_items)
#   
#   ces_spending_items_covered <- ces_spending_items_summary %>%
#     filter(n_years == length(ces_spending_years)) %>%
#     pull(variable)
#   
#   if (length(ces_spending_items_covered) == 0) {
#     warning("No spending_* items covered across all selected years; using all spending_* items found.")
#     ces_spending_items_covered <- ces_spending_items
#   }
# } else {
#   warning("Skipping spending_* item selection: no spending items or no eligible years.")
# }

# Save --------------------------------------------------------------------

attr(ces, "description") <- 
  "Cumulative Cooperative Election Study (CES)"

saveRDS(ces, file = "data/ces.rds")

# Checks ------------------------------------------------------------------
## Common Items -----------------------------------------------------------

plot_yearly_coverage_heatmap(ces)
summarize_yearly_coverage(ces)

ces %>% select(year, starts_with("spending")) %>% plot_yearly_coverage_heatmap()

## Common Items by Category -----------------------------------------------

ces_topic_years <- intersect(2014:2024, sort(unique(ces$year)))

ces %>% 
  plot_yearly_support_heatmap(ques_rgx = "immig", group_var = "pid3", facet_by = "group_var")
ces %>% 
  plot_yearly_support_heatmap(ques_rgx = "health", group_var = "pid3", facet_by = "group_var")
ces %>% 
  plot_yearly_support_heatmap(ques_rgx = "gun", group_var = "pid3", facet_by = "group_var")
ces %>% 
  plot_yearly_support_heatmap(ques_rgx = "abortion", group_var = "pid3", facet_by = "group_var")
ces %>% 
  plot_yearly_support_heatmap(ques_rgx = "enviro", group_var = "pid3", facet_by = "group_var")
ces %>% 
  plot_yearly_support_heatmap(ques_rgx = "spend", group_var = "pid3", facet_by = "group_var")


ces_topic_items <- c( # only items with consistent coverage
  "immig_legalize", 
  # Dems: gradually increasing support (57-84%)
  # Reps: moderate opposition (40-55%)
  "immig_border",
  # Dems: moderate support (50-60%)
  # Reps: very high support through (70-90%)
  
  "healthcare_aca",
  # Dems: very high opposition (70-90%)
  # Reps: very high support (70-90%)
  
  "guns_permits",
  # Dems: high opposition (60-80%)
  # Reps: moderate support (50-60%)
  "guns_bgchecks",
  # Dems: extremely high support (+95%)
  # Reps: high support (~80%)
  "guns_assaultban",
  # Dems: very high support (+80%)
  # Reps: moderate opposition (40%)
  
  "abortion_always",
  # Dems: high support (~80%)
  # Reps: moderate opposition (30%)
  "abortion_20weeks",
  # Dems: moderate opposition (~50%) 
  # Reps: high support (~80%)
  "abortion_conditional",
  # Dems: moderate opposition (~30-40%)
  # Reps: moderate support (~60%)
  
  "enviro_renewable",
  "enviro_mpg_raise",
  "enviro_carbon",
  "enviro_airwateracts"
  # Dems: very high support (80%)
  # Reps: moderate opposition (40%)
)

# TODO: obviously some items, doesn't matter what you pick...but other policies
#       results are sensitive to specific item choice. 

