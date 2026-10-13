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

## ----data_prep----------------------------------------------------------------
trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

adsl <- pharmaverseadamjnj::adsl |>
  filter(SAFFL == "Y") |>
  select(STUDYID, USUBJID, all_of(trtvar), SAFFL) |>
  mutate(!!trtvar := factor(
    .data[[trtvar]],
    levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
  ))

adae <- pharmaverseadamjnj::adae |>
  filter(TRTEMFL == "Y") |>
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD)

# Add spanning header and risk difference variables
adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)
adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], "vs", ctrl_grp)

ae <- adae |> right_join(adsl, by = "USUBJID")

## ----ref_path-----------------------------------------------------------------
colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

## ----layout_and_table---------------------------------------------------------
extra_args <- list(
  denom = "n_altdf",
  riskdiff = TRUE,
  ref_path = ref_path,
  method = "wald",
  .stats = "count_unique_fraction",
  .formats = c(rr_ci_3d = jjcsformat_xx("xx.x (xx.x, xx.x)"))
)

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
  split_cols_by("colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  ) |>
  analyze("TRTEMFL",
    afun = a_freq_j,
    extra_args = append(extra_args, list(val = "Y", label = "Subjects with >=1 AE"))
  ) |>
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = " ",
    nested = FALSE
  ) |>
  summarize_row_groups("AEBODSYS", cfun = a_freq_j, extra_args = extra_args) |>
  analyze("AEDECOD", afun = a_freq_j, extra_args = extra_args) |>
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)
head(result, 10)

## ----wildcard-----------------------------------------------------------------
# Simulate a split context for ARM == "B: Placebo" under colspan_trt == " "
spl_ctx <- data.frame(
  cur_col_split     = I(list(c("colspan_trt", "ARM"))),
  cur_col_split_val = I(list(c(" ", "B: Placebo")))
)

# Exact match
in_column(c("colspan_trt", " ", "ARM", "B: Placebo"), spl_ctx)

# Wildcard on spanning header — still TRUE
in_column(c("*", "*", "ARM", "B: Placebo"), spl_ctx)

# Different ARM level — FALSE
in_column(c("*", "*", "ARM", "A: Drug X"), spl_ctx)

## ----col_paths----------------------------------------------------------------
col_paths(result)

