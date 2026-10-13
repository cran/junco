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
library(tern)

## ----example------------------------------------------------------------------
fmt <- format_sigfig_j(3)
fmt(0.001234)
fmt(1234.5)
fmt(1234.5, round_type = "sas")
signif(1234.5, 3)
format_sigfig(3)(1234.5)

fmt2 <- format_sigfig_j(3, format = "xx (xx)")
fmt2(c(1234.5, 0.00567))

format_sigfig_j(3, drop0trailing = FALSE)(0.01)
format_sigfig_j(3, drop0trailing = TRUE)(0.01)
format_sigfig(3)(0.01)

## ----example 2 - datasetup, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
adsl <- random.cdisc.data::cadsl |>
  filter(ACTARM == "A: Drug X")
adpc <- random.cdisc.data::cadpc |>
  filter(ACTARM == "A: Drug X", PARAM %in% c("Plasma Drug X", "Urine Drug X"))

# Setting up the data
adpc_1 <- adpc |>
  mutate(
    NFRLT = as.factor(NFRLT),
    AVALCAT1 = as.factor(AVALCAT1),
    NOMTPT = as.factor(paste(NFRLT, "/", PCTPT))
  ) |>
  select(STUDYID, NOMTPT, ACTARM, VISIT, AVAL, PARAM, AVALCAT1)

adpc_1$NOMTPT <- factor(
  adpc_1$NOMTPT,
  levels = levels(adpc_1$NOMTPT)[order(as.numeric(gsub(".*?([0-9\\.]+).*", "\\1", levels(adpc_1$NOMTPT))))]
)

## ----example 2 - layout with analyze_vars_in_cols, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
# Row structure
lyt_rows <- basic_table() |>
  split_rows_by(
    var = "PARAM",
    split_fun = drop_split_levels,
    split_label = "Parameter",
    label_pos = "topleft"
  ) |>      
  split_rows_by(
    var = "ACTARM",
    split_fun = drop_split_levels,
    split_label = "Treatment Group",
    label_pos = "topleft"
  ) |>
  split_rows_by(
    var = "VISIT",
    split_fun = drop_split_levels,
    split_label = "Visit",
    label_pos = "topleft"
  ) |>
  split_rows_by(
    var = "NOMTPT",
    split_fun = drop_split_levels,
    split_label = "Nominal Time (hr) / Timepoint",
    label_pos = "topleft",
    child_labels = "hidden"
  )

## ----example 2 - continue layout, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
lyt <- lyt_rows |>
  analyze_vars_in_cols(
    vars = c("AVAL", "AVALCAT1", rep("AVAL", 8)),
    .stats = c("n", "n_blq", "mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
    .formats = c(
      n = "xx.", n_blq = "xx.", mean = format_sigfig_j(3), sd = format_sigfig_j(3), cv = "xx.x", median = format_sigfig_j(3),
      geom_mean = format_sigfig_j(3), geom_cv = "xx.x", min = format_sigfig_j(3), max = format_sigfig_j(3)
    ),
    .labels = c(
      n = "n", n_blq = "Number\nof\nLTRs/BLQs", mean = "Mean", sd = "SD", cv = "CV (%) Mean",
      geom_mean = "Geometric Mean", geom_cv = "CV % Geometric Mean", median = "Median", min = "Minimum", max = "Maximum"
    ),
    na_str = "NE",
    .aligns = "decimal"
  )

result <- build_table(lyt, df = adpc_1, alt_counts_df = adsl) 

result

## ----setup - stats_in_cols, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
 mysplitfun <- make_split_fun(
   post = list(stats_in_cols_setup())
 )

 lyt_stats_in_cols <- basic_table() |>
   split_cols_by_multivar(
     c("AVAL"),
     varlabels = c("Visit Values")
   ) |>
   split_cols_by("STUDYID", split_fun = mysplitfun) |>
   split_rows_by("PARAM", split_fun = drop_split_levels) |>
   split_rows_by("ACTARM", labels_var = "ACTARM", split_fun = drop_split_levels) |>
   split_rows_by("VISIT", split_fun = drop_split_levels) 
 
 
 adpc_1 <- adpc_1 |> 
   filter(VISIT == "Day 1", !(NOMTPT %in% "0 / Predose")) 

## ----setup - format 1, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
 adpc_fmt <- tribble(
   ~PARAM, ~fmt_d,
   "Plasma Drug X", list(
     mean = format_sigfig_j(3), sd = format_sigfig_j(3), se = format_sigfig_j(3), median = format_sigfig_j(3),
     min = format_sigfig_j(3), max = format_sigfig_j(3)
   ),
   "Urine Drug X", list(
     mean = format_sigfig_j(2), sd = format_sigfig_j(2), se = format_sigfig_j(2), median = format_sigfig_j(2),
     min = format_sigfig_j(2), max = format_sigfig_j(2)
   )
 )

adpc_2 <- left_join(adpc_1, adpc_fmt) 

## ----setup - format 2, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
junco_def_d_sigfig <- list(
     mean = format_sigfig_j(3), sd = format_sigfig_j(3), se = format_sigfig_j(3), median = format_sigfig_j(3),
     min = format_sigfig_j(3), max = format_sigfig_j(3)
   )

adpc_fmt2 <- tribble(
   ~PARAM, ~d,
   "Plasma Drug X", 3,
   "Urine Drug X", 2
 )

adpc_fmt2 <- fmt_spec_df_d(adpc_fmt2,
                    d_column = "d",
                    fmt_column = "fmt_d2",
                    fmt_d_def = junco_def_d_sigfig,
                    fmt_d_in = NULL
)


fmt_details <- lapply(adpc_fmt2$PARAM,
                      FUN = function(param){
                        fmt <- adpc_fmt2[adpc_fmt2$PARAM == param, c("fmt_d2"), drop = TRUE][[1]]
                        fmt
                        get_fmt_details(fmt, recursive = FALSE, as_tibble = TRUE)
                      }
  )

names(fmt_details) <- adpc_fmt2$PARAM

# confirm 2 significant figures for Urine Drug X
# see column d in tibble
print(fmt_details[["Urine Drug X"]][, c("stat", "fun_fact", "str", "d", "drop0trailing", "whole_integer")])

adpc_3 <- left_join(adpc_1, adpc_fmt2) 

## ----example 2 - layout 2, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
 lyt2 <- lyt_stats_in_cols |> 
   analyze("NOMTPT", afun = column_stats, extra_args = list(
     .formats = "default",
     formats_var = "fmt_d"
   ))
 result2 <- build_table(lyt2, adpc_2, round_type = "sas") |> 
   prune_table()
 result2

## ----example 2 - layout 3, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
lyt3 <- lyt_stats_in_cols |> 
 analyze("NOMTPT", afun = column_stats, extra_args = list(
   .formats = "default",
   formats_var = "fmt_d2"
 ))
result3 <- build_table(lyt3, adpc_3, round_type = "sas") |> 
 prune_table()
result3

## ----example 2 - identical result, eval = requireNamespace("random.cdisc.data", quietly = TRUE)----
identical(cell_values(result2), cell_values(result3))
identical(get_formatted_cells(result2), get_formatted_cells(result3))

