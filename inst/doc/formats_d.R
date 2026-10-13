## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----load_packages, message=FALSE---------------------------------------------
library(rtables)
library(junco)
library(dplyr)

## ----d-style format setup-----------------------------------------------------
junco_def_d <- c(
 "mean" = "xx.dx",
 "mean_sd" = "xx.dx (xx.dxx)",
 "mean_se" = "xx.dx (xx.dxx)",
 "range" = "(xx.d, xx.d)",
 "count_fraction" = jjcsformat_count_fraction
)

## ----invest d-style defaults pre, include=FALSE-------------------------------
d_style <- vapply(junco_def_d_all, FUN = function(fmt){ 
  !is.function(fmt) && is.character(fmt) && grepl("xx.d", fmt, fixed = TRUE)},
  FUN.VALUE = logical(1))

non_d_style <- junco_def_d_all[!d_style]

d_style <- junco_def_d_all[d_style]

## ----invest d-style defaults--------------------------------------------------
d_style <- vapply(junco_def_d_all, FUN = function(fmt){ 
  !is.function(fmt) && is.character(fmt) && grepl("xx.d", fmt, fixed = TRUE)},
  FUN.VALUE = logical(1))

non_d_style <- junco_def_d_all[!d_style]

d_style <- junco_def_d_all[d_style]
names(d_style)
d_style[["adj_mean_se"]]

## ----junco_def_d_all with d is 1 is junco_default_formats---------------------

fmt_1 <- fmt_spec_single_d(d = 1, fmt_d_def = junco_def_d_all)
fmt_1[["lr_stat_df"]] <- NULL

# get_fmt_details tool applied to a format specification vector
fmt_details <- get_fmt_details(fmt_1, recursive = FALSE, as_tibble = TRUE)

fmt_details[fmt_details$stat == "adj_mean_se", c("stat", "fun_fact", "str", "str_formatters")]

# to compare against junco_default_formats, except lr_stat_df
junco_default_formats_adj <- junco_default_formats
junco_default_formats_adj[["lr_stat_df"]] <- NULL

# compare_fmt_specs tool to compare 2 format specification vectors
comp <- compare_fmt_specs(fmt_1, junco_default_formats)


## ----demographic example------------------------------------------------------
set.seed(101)
f2n <- function(x) as.numeric(x) - 1
df <- ex_adsl
df <- df |> 
  mutate(newvar = rnorm(nrow(df), mean = 50 + 3 * f2n(ARMCD) + 4 * AGE, sd = 20))

# a mix of categorical and continuous variables
vars_all <- c("AGE", "SEX", "BMRKR1", "newvar")

# set all variables to d = 0
vars_d <- rep(0, length(vars_all))
names(vars_d) <- vars_all
# set some variables to a different d value
vars_d["BMRKR1"] <- 2
vars_d["newvar"] <- 1

# conversion of d-style format to valid format specification
vars_fmt <- fmt_spec_var_d(vars_d,
 stats_in = NULL,
 fmt_d_def = junco_def_d,
 fmt_d_in = NULL
)

lyt <- basic_table() |>
 split_cols_by("ARMCD") |>
 analyze(
   vars = vars_all,
   afun = tern::a_summary,
   # note .formats = "default" in extra_args to afun   
   extra_args = list(
     .stats = c("n", "mean_sd", "range", "count_fraction"),
     .formats = "default"
   ),
   # note the usage of format argument to analyze call
   format = vars_fmt,
   section_div = " "
)
rslt <- build_table(lyt, df)
rslt

## ----bds example--------------------------------------------------------------
# set precision d on a parameter by parameter basis 
df_d <- tribble(
 ~PARAMCD, ~d,
 "ALT", 2,
 "CRP", 3,
 "IGA", 1
)
# convert d-based setup into format specification
# a new column fmt_d is created with format specifications
df_d <- fmt_spec_df_d(df_d,
 d_column = "d",
 fmt_column = "fmt_d",
 stats_in = NULL,
 fmt_d_def = junco_def_d_all,
 fmt_d_in = NULL
)

df <- ex_adlb |>
 filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))

# join with dataframe with format specs
df <- df |>
 left_join(df_d)

lyt <- basic_table() |>
 split_cols_by("ARMCD") |>
 split_rows_by("PARAMCD", split_fun = drop_split_levels) |>
 split_rows_by("AVISIT", split_fun = drop_split_levels) |>
 analyze(
   vars = "AVAL",
   afun = tern::a_summary,
   # note .formats = "default" in extra_args to afun
   extra_args = list(
     .stats = c("n", "mean_se", "range"),
     .formats = "default"
   ),
   # note the usage of formats_var argument to analyze call   
   formats_var = "fmt_d"
 )
 
rslt <- build_table(lyt, df, alt_counts_df = ex_adsl)
rslt

## ----bds example 2------------------------------------------------------------
fmt_2 <- fmt_spec_single_d(d = 2, fmt_d_def = junco_def_d_all)

df <- ex_adlb |>
 filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))

 lyt <- basic_table() |>
 split_cols_by("ARMCD") |>
 split_rows_by("PARAMCD", split_fun = drop_split_levels) |>
 split_rows_by("AVISIT", split_fun = drop_split_levels) |>
 analyze(
   vars = "AVAL",
   afun = tern::a_summary,
   extra_args = list(
     .stats = c("n", "mean_se", "range"),
     .formats = fmt_2
   )
 )
 
rslt <- build_table(lyt, df, alt_counts_df = ex_adsl)
rslt

## ----bds example 3------------------------------------------------------------
df <- ex_adlb |>
 filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) |> 
mutate(AVISIT = droplevels(AVISIT))

# join with dataframe with format specs
df <- df |>
 left_join(df_d)

mysplitfun <- make_split_fun(
   post = list(stats_in_cols_setup())
)

lyt <- basic_table() |>
 split_cols_by_multivar(
   c("AVAL", "BASE", "CHG"),
   varlabels = c("Visit Values", " ", "Change from Baseline")
 ) |>
 split_cols_by("STUDYID", split_fun = mysplitfun) |>
 split_rows_by("PARAM", split_fun = drop_split_levels) |>
 split_rows_by("ARMCD", labels_var = "ARM") |>
 analyze("AVISIT", afun = column_stats, extra_args = list(
   exclude_visits = toupper(c("Baseline", "Screening")),
   .formats = "default",
   formats_var = "fmt_d"
 ))
rslt <- build_table(lyt, df, round_type = "sas")
rslt

