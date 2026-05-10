## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(junco)
adsl <- create_colspan_var(pharmaverseadamjnj::adsl)
adae <- create_colspan_var(pharmaverseadamjnj::adae)
trt_map <- create_colspan_map(adsl)
print(trt_map)

## -----------------------------------------------------------------------------
lyt <- basic_table() |>
  grouped_cols_w_diffs(trt_map)


build_table(lyt, adsl)

## -----------------------------------------------------------------------------
lyt2 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, diffs_label = "Mean Differences")


build_table(lyt2, adsl)

## -----------------------------------------------------------------------------
lyt_nodiff <- basic_table() |>
  grouped_cols_w_diffs(trt_map, diff_cols = FALSE)


build_table(lyt_nodiff, adsl)

## -----------------------------------------------------------------------------
library(tibble)
combodf1 <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "all_active", "All Xanomeline", c("Xanomeline High Dose", "Xanomeline Low Dose"), list()
)

lyt3 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, combo_map_df = combodf1)

build_table(lyt3, adsl)

## -----------------------------------------------------------------------------
combodf2 <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs, ~is_control,
  "all_active", "All Xanomeline", c("Xanomeline High Dose", "Xanomeline Low Dose"), list(), FALSE,
  "placebo_redux", "Double Placebo!!", c("Placebo"), list(), TRUE
)

lyt4 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, combo_map_df = combodf2)

build_table(lyt4, adsl)

## -----------------------------------------------------------------------------
comp_map1 <- tribble(
  ~active, ~comparator,
  "Xanomeline High Dose", "Placebo",
  "Xanomeline Low Dose", "placebo_redux",
  "all_active", "placebo_redux"
)


lyt5 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, combo_map_df = combodf2, comp_map = comp_map1)

build_table(lyt5, adsl)

## -----------------------------------------------------------------------------
comp_map2 <- tribble(
  ~active, ~comparator,
  "Xanomeline High Dose", "Xanomeline Low Dose",
  "Xanomeline High Dose", "Placebo",
  "Xanomeline Low Dose", "Placebo"
)


lyt6 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, comp_map = comp_map2)

build_table(lyt6, adsl)

