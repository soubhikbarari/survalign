setwd("~/Research_Group Dropbox/Soubhik Barari/Projects/repos/survalign/")

source("step0/utils.R")
source("step0/ces_utils.R")

message("Preparing GSS")

# Read Data ---------------------------------------------------------------

# gss_22 <- read_dta(here("data/gss_2022.dta"))
# gss_22 %>%
#   filter(year == 2022) %>%
#   select(where(~ !all(is.na(.))))
#
# dump_column_labels(gss_22, file = "ques/cols_gss_2022.csv")
#
# gss_22_items <- identify_relevant_columns(gss_22, class = "sociopolitical")

if (!exists("gss_72_22_raw")) {
  gss_72_22_raw <- read_dta(here("data-raw/gss_1972-2022.dta"))
}

# Process Data ------------------------------------------------------------

gss <- gss_72_22_raw %>%
  mutate(age4 = factor(case_when(
    age >= 18 & age <= 29 ~ "18-29",
    age >= 30 & age <= 49 ~ "30-49",
    age >= 50 & age <= 64 ~ "50-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_
  ), levels = c("18-29", "30-49", "50-64", "65+"))) %>%
  mutate(pid3 = factor(case_when(
    partyid %in% 0:2 ~ "Democrat",
    partyid %in% 4:6 ~ "Republican",
    partyid %in% c(3,7) ~ "Independent/Other",
    TRUE ~ NA_character_
  ), levels = c("Democrat","Republican", "Independent/Other")), 
  educ5 = factor(case_when(
    degree == 0 ~ "Less than HS",
    degree == 1 ~ "High school",
    degree == 2 ~ "Some college", 
    degree == 3 ~ "College",
    degree == 4 ~ "Post-grad",
    TRUE ~ NA_character_
  ), levels = c("Less than HS", "High school", "Some college", "College", "Post-grad")), 
  race3 = factor(case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race %in% 3:4 ~ "Other",
    TRUE ~ NA_character_
  ), levels = c("White", "Black", "Other"))) %>%
  rename(
    weight_nrps = "wtssnrps",
    weight_sps = "wtssps"
  )

# gss_72_22 %>%
#   dump_column_labels(file = "data-raw/ques/cols_gss_1972-2022.csv")

gss_72_22_items <- read_csv(here("data-raw/ques/cols_gss_1972-2022_pol_gpt.csv")) %>%
  mutate(variable = tolower(variable))

stopifnot(all(gss_72_22_items$variable %in% colnames(gss_72_22)))

gss_72_22_items %>%
  filter(!(variable %in% colnames(gss_72_22)))

gss_72_22_pol_year <- gss_72_22 %>%
  select(any_of(c("year", gss_72_22_items$variable))) %>%
  dump_column_labels_with_tabs(tab_col = "year", file = "data-raw/ques/cols_gss_1972-2022_pol_year.csv")

# Patch Data --------------------------------------------------------------

gss <- gss %>%
  mutate(marhomo = case_when(
    is.na(marhomo) ~ marhomo1,
    is.na(marhomo1) ~ marhomo,
    TRUE ~ marhomo
  ))

gss <- gss %>%
  mutate(year = as.factor(year), id = as.factor(id)) %>%
  select(year, id, 
         weight_nrps, weight_sps,
         age4, pid3, educ5, race3,
         all_of(gss_72_22_pol_year$var))

attr(gss$year, "label") <- "Survey Year"
attr(gss$id, "label") <- "Respondent ID"
attr(gss$weight_nrps, "label") <- "Post-Strat. Weight w/NR Adj (U.S. Adult)"
attr(gss$weight_sps, "label") <- "Post-Strat. Weight (U.S. Adult)"

attr(gss$pid3, "label") <- "Party (3)"
attr(gss$age4, "label") <- "Age (4)"
attr(gss$educ5, "label") <- "Education (5)"
attr(gss$race3, "label") <- "Race (5)"

attr(gss$weight_nrps, "weight_var") <- TRUE
attr(gss$weight_sps, "weight_var") <- TRUE

attr(gss$pid3, "group_var") <- TRUE
attr(gss$age4, "group_var") <- TRUE
attr(gss$educ5, "group_var") <- TRUE
attr(gss$race3, "group_var") <- TRUE

# Binarize ----------------------------------------------------------------

gss <- gss %>%
  mutate(
    # nat*
    across(starts_with("nat"), ~case_when(.x == 1 ~ 1, .x %in% c(2,3) ~ 0, TRUE ~ NA_real_), .names = "{.col}_b"),
    # eqwlth
    eqwlth_b = case_when(eqwlth %in% 1:3 ~ 1, eqwlth %in% 4:7 ~ 0, is.na(eqwlth) ~ NA_real_),
    # helppoor
    helppoor_b = case_when(helppoor %in% 1:2 ~ 1, helppoor %in% 3:5 ~ 0, is.na(helppoor) ~ NA_real_),
    # taxrich
    taxrich_b = case_when(taxrich == 1 ~ 1, taxrich %in% 2:3 ~ 0, is.na(taxrich) ~ NA_real_),
    # laborpow
    laborpow_b = case_when(laborpow %in% 4:5 ~ 1, laborpow %in% 1:3 ~ 0, is.na(laborpow) ~ NA_real_),
    # helpblk
    helpblk_b = case_when(helpblk %in% 1:2 ~ 1, helpblk %in% 3:5 ~ 0, is.na(helpblk) ~ NA_real_),
    # racdif1, racdif4
    racdif1_b = case_when(racdif1 == 1 ~ 1, racdif1 == 2 ~ 0, is.na(racdif1) ~ NA_real_),
    racdif4_b = case_when(racdif4 == 1 ~ 1, racdif4 == 2 ~ 0, is.na(racdif4) ~ NA_real_),
    # affrmact, abany, abrape, abpoor, abdefect, abhlth, absingle, abnomore, polhitok
    across(c(abany, abrape, abpoor, abdefect, abhlth, absingle, abnomore, polhitok), 
           ~case_when(.x %in% 1 ~ 1, .x %in% 2 ~ 0, is.na(.x) ~ NA_real_), .names = "{.col}_b"),
    across(c(affrmact), 
           ~case_when(.x %in% 1:2 ~ 1, .x %in% 3:4 ~ 0, is.na(.x) ~ NA_real_), .names = "{.col}_b"),
    # homosex
    homosex_b = case_when(homosex %in% 1:3 ~ 0, homosex == 4 ~ 1, is.na(homosex) ~ NA_real_),
    # marhomo, spkcom, spkrac, spkhomo
    across(c(marhomo),
           ~case_when(.x %in% 1:2 ~ 1, .x %in% 3:5 ~ 0, is.na(.x) ~ NA_real_), .names = "{.col}_b"),
    across(c(spkcom, spkrac, spkhomo),
           ~case_when(.x %in% 1 ~ 1, .x %in% 2 ~ 0, is.na(.x) ~ NA_real_), .names = "{.col}_b"),
    # colhomo, colrac
    colhomo_b = case_when(colhomo == 4 ~ 1, colhomo == 5 ~ 0, is.na(colhomo) ~ NA_real_),
    colrac_b  = case_when(colrac == 4 ~ 1, colrac == 5 ~ 0, is.na(colrac) ~ NA_real_),
    # cappun, gunlaw
    cappun_b = case_when(cappun == 1 ~ 1, cappun == 2 ~ 0, is.na(cappun) ~ NA_real_),
    gunlaw_b = case_when(gunlaw == 1 ~ 1, gunlaw == 2 ~ 0, is.na(gunlaw) ~ NA_real_),
    # colcom
    colcom_b = case_when(colcom == 5 ~ 1, colcom == 4 ~ 0, is.na(colcom) ~ NA_real_),
    # libcom, librac, libhomo
    libcom_b = case_when(libcom == 2 ~ 1, libcom == 1 ~ 0, is.na(libcom) ~ NA_real_),
    librac_b = case_when(librac == 2 ~ 1, librac == 1 ~ 0, is.na(librac) ~ NA_real_),
    libhomo_b = case_when(libhomo == 2 ~ 1, libhomo == 1 ~ 0, is.na(libhomo) ~ NA_real_),
    # letin1a
    letin1a_b = case_when(letin1a %in% 1:2 ~ 1, letin1a %in% 3:5 ~ 0, is.na(letin1a) ~ NA_real_),
    # fefam
    fefam_b = case_when(fefam %in% 1:2 ~ 1, fefam %in% 3:4 ~ 0, is.na(fefam) ~ NA_real_),
    # fejobaff
    fejobaff_b = case_when(fejobaff %in% 1:2 ~ 1, fejobaff %in% 3:4 ~ 0, is.na(fejobaff) ~ NA_real_)
  )

stopifnot(any(gss$nataid_b == 0) & any(gss$nataid_b == 1))
stopifnot(any(gss$abdefect_b == 0) & any(gss$abdefect_b == 1))

gss <- gss %>%
  group_by(year) %>%
  mutate(across(
    ends_with("_b"),
    ~ case_when(
      is.na(.x) ~ ifelse(
        all(is.na(.x)),
        haven::tagged_na('s'),
        haven::tagged_na('r')
      ),
      TRUE ~ .x
    ) |> haven::labelled(
      labels = c(
        "Oppose" = 0,
        "Support" = 1,
        "Not asked (survey)" = haven::tagged_na('s'),
        "Nonresponse" = haven::tagged_na('r')
      ),
      label = attr(.x, "label")
    )
  )) %>%
  # distinct(year, eqwlth_b) %>% as.data.frame()
  ungroup()

stopifnot(any(gss$nataid_b == 0) & any(gss$nataid_b == 1))
stopifnot(any(gss$abdefect_b == 0) & any(gss$abdefect_b == 1))

gss %>%
  select(ends_with("_b")) %>%
  summarise(across(everything(), ~n_distinct(na.omit(.x)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "n_unique_non_na") %>%
  filter(n_unique_non_na == 1) %>%
  pull(var) %>%
  {stopifnot(length(.) == 0)}

# Labels ------------------------------------------------------------------

for (gi in unique(gss_72_22_items$variable)) {
  message(gi)
  row <- gss_72_22_items %>% filter(variable == gi)
  if (gi %in% colnames(gss)) {
    attr(gss[[gi]], "label") <- row$label
    attr(gss[[gi]], "category") <- stringr::str_to_title(row$category)
    if (!is.null(attr(gss[[gi]], "labels"))) {
      names(attr(gss[[gi]], "labels")) <- stringr::str_to_title(names(attr(gss[[gi]], "labels")))
      names(attr(gss[[gi]], "labels")) <- stringr::str_replace_all(
        names(attr(gss[[gi]], "labels")),
        regex("\\b(Iap|Dk|Na)\\b", ignore_case = TRUE),
        toupper
      )
    }
  }
  
  if (paste0(gi,"_b") %in% colnames(gss)) {
    attr(gss[[paste0(gi,"_b")]], "label") <- row$label_b
    attr(gss[[paste0(gi,"_b")]], "category") <- stringr::str_to_title(row$category)
  }
}

gss <- gss %>%
  select(year, id, 
         age4, pid3, educ5, race3,
         weight_nrps, weight_sps,
         sort(c(gss_72_22_items$variable, paste0(gss_72_22_items$variable, "_b"))))
  
# Save --------------------------------------------------------------------

attr(gss, "description") <- 
  "Cumulative General Social Survey (GSS)"

saveRDS(gss, file = "data/gss.rds")

# Checks ------------------------------------------------------------------
## Common Items -----------------------------------------------------------

plot_yearly_coverage_heatmap(gss)
summarize_yearly_coverage(gss)

## Common Items by Category -----------------------------------------------

gss %>% 
  plot_yearly_support_heatmap(ques_rgx = "^ab.*b", weight_var = "weight_nrps", group_var = "pid3", facet_by = "group_var")

gss %>% 
  plot_yearly_support_heatmap(ques_rgx = "^nat.*b", weight_var = "weight_nrps", group_var = "pid3", facet_by = "group_var")


