## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----data, message = FALSE----------------------------------------------------
library(dplyr)
library(rtables)
library(junco)

set.seed(101)

adchg <- tibble(
  USUBJID = sprintf("SUBJ-%03d", 1:300),
  TRT01A = factor(rep(c("Placebo", "Low Dose", "High Dose"), length.out = 300)),
  REGION = factor(sample(c("EU", "US"), 300, replace = TRUE, prob = c(0.6, 0.4))),
  BASE = rnorm(300, 50, 10),
  CHG  = rnorm(300, 0, 8)
)

## ----combodf------------------------------------------------------------------
combodf <- tibble::tribble(
  ~valname,   ~label,    ~levelcombo,                     ~exargs,
  "ACTIVE",   "Active", c("Low Dose", "High Dose"), list()
)

## ----lyt_table----------------------------------------------------------------
lyt <- basic_table() |>
  split_cols_by(
    "TRT01A",
    ref_group = "Placebo",
    split_fun = add_combo_levels(combodf)
  ) %>%
  add_colcounts() |>
  analyze(
    vars = "CHG",
    afun = a_summarize_ancova_j,
    var_labels = "Change from Baseline",
    extra_args = list(
      variables = list(arm = "TRT01A", covariates = c("BASE", "REGION")),
      ref_path = c("TRT01A", "Placebo"),
      method_combo = "contrasts",
      weights_combo = "equal",
      weights_emmeans = "equal",
      conf_level = 0.95      
    )
  )

build_table(lyt, adchg)

