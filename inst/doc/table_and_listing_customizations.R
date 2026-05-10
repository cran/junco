## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

options(docx.add_datetime=FALSE)

library(junco)
library(rlistings)
library(tibble)


## ----setup-title, include=FALSE-----------------------------------------------
#Need to set up a dummy tittle and main footer
titles <- list(title = " ",
                     subtitles = NULL,
                     main_footer = "Dummy Note: On-treatment is defined as ~{optional treatment-emergent}")
#Need tempdir
out_dir <- file.path("..", "output")
if(!dir.exists(out_dir)) {
  dir.create(out_dir)
}

NOT_CRAN <- !testthat:::on_cran()

## ----orientation_rtf----------------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",
                show_colcounts = FALSE
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----orientation_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE------
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/exampleorientation"),
#              orientation = "landscape")

## ----orientation_docx_export, eval=NOT_CRAN, message=FALSE, warning=FALSE-----
# export_TLG_as_docx(result,
#                 tblid = "exampleorientation",
#                 output_dir = out_dir,
#                 orientation = "landscape")

## ----orientation_rtf_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "exampleorientation", orientation = "landscape")

## ----fontsize_rtf-------------------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",
                show_colcounts = FALSE
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----fontsize_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE---------
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/examplefontsize"),
#              fontspec = formatters::font_spec("Times", 8, 1.2))
# 

## ----fontsize_docx_export, eval=NOT_CRAN, message=FALSE, warning=FALSE--------
# export_TLG_as_docx(result,
#                 tblid = "examplefontsize",
#                 output_dir = out_dir,
#                 theme = theme_docx_default_j(font = "Times New Roman", font_size = 8L))

## ----fontsize_rtf_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "examplefontsize", fontspec = formatters::font_spec("Times", 8, 1.2))

## ----spanning_header_setup----------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

adsl

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

  colspan_trt_map

## ----spanning_header_full-----------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

  # No combined column needed for spanning header example
  mysplit <- make_split_fun()

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt",
                split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A",
                show_colcounts = FALSE,
                split_fun = mysplit
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----spanning_header_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/examplespanningheader1"),
#              orientation = "landscape")

## ----spanning_header_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "examplespanningheader1", orientation = "landscape")

## ----combined_column_setup----------------------------------------------------
add_combo <- add_combo_facet("Combined",
    label = "Combined",
    levels = c("Example Drug 5 mg", "Example Drug 10 mg", "Example Drug 20 mg")
  )

  mysplit <- make_split_fun(post = list(add_combo))

## ----combined_column_full-----------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

  # Set up levels and label for the required combined column
  add_combo <- add_combo_facet("Combined",
    label = "Combined",
    levels = c("Example Drug 5 mg", "Example Drug 10 mg", "Example Drug 20 mg")
  )

  mysplit <- make_split_fun(post = list(add_combo))

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt",
                split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A",
                show_colcounts = FALSE,
                split_fun = mysplit
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----combined_column1_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/examplecombinedcolumn1"),
#              orientation = "landscape")

## ----combined_column_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "examplecombinedcolumn1", orientation = "landscape")

## ----improved_combined_column_setup, eval=FALSE-------------------------------
#   # choose if any facets need to be removed - e.g remove the combined column for placebo
#   rm_combo_from_placebo <- cond_rm_facets(
#     facets = "Combined",
#     ancestor_pos = NA,
#     value = " ",
#     split = "colspan_trt"
#   )
# 
#   mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))

## ----improved_combined_column_full--------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

  # Set up levels and label for the required combined column
  add_combo <- add_combo_facet("Combined",
    label = "Combined",
    levels = c("Example Drug 5 mg", "Example Drug 10 mg", "Example Drug 20 mg")
  )

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt",
                split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A",
                show_colcounts = FALSE,
                split_fun = mysplit
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----combined_column2_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/examplecombinedcolumn2"),
#              orientation = "landscape")

## ----improved_combined_column_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "examplecombinedcolumn2", orientation = "landscape")

## ----newline_data-------------------------------------------------------------
adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

adsl

## ----newline_table_rtf--------------------------------------------------------
library(junco)
library(rtables)

adsl <- data.frame(TRT01A = c("Example Drug\\line 5 mg",
                              "Example Drug\\line 10 mg",
                              "Example Drug\\line 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----newline_table_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/newlinetable"),
#              orientation = "landscape")

## ----string_map_newline-------------------------------------------------------
string_map_newline <- rbind(default_str_map, c("\\line", "\n"))
string_map_newline

## ----newline_table_docx-------------------------------------------------------
library(junco)
library(rtables)

adsl <- data.frame(TRT01A = c("Example Drug\\line5 mg",
                              "Example Drug\\line10 mg",
                              "Example Drug\\line20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)


## ----newline_table_docx_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# export_TLG_as_docx(result,
#                 tblid = "examplenewline",
#                 output_dir = out_dir,
#                 orientation = "landscape",
#                 string_map = string_map_newline)

## ----newline_table_rtf_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "newlinetable", orientation = "landscape",
                  string_map = string_map_newline)

## ----newline_listing_rtf------------------------------------------------------
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for\\line Period 01") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)

## ----newline_listing_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/examplelistingnewline2"),
#              orientation = "landscape")

## ----newline_listing_docx-----------------------------------------------------
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for\\linePeriod 01") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)


## ----newline_listing_docx_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# export_TLG_as_docx(result,
#                 tblid = "examplelistingnewline2",
#                 output_dir = out_dir,
#                 orientation = "landscape",
#                 string_map = string_map_newline)

## ----newline_listing_rtf_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "examplelistingnewline2", orientation = "landscape",
                  string_map = string_map_newline)

## ----border_matrix------------------------------------------------------------
library(junco)

adsl <- data.frame(span = c("Example Drug Low Dose",
                            "Example Drug Low Dose",
                            "Example Drug High Dose",
                            " "),
                   TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first level in column header
  split_cols_by("span",,
                show_colcounts = FALSE
  ) |>
  ### second level in column header
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----border_matrix_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/bordertable"),
#              orientation = "landscape")

## ----border_matrix_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "bordertable", orientation = "landscape")

## ----default_border_matrix----------------------------------------------------
header_border <- junco:::make_header_bordmat(obj = result)
header_border

## ----updated_border_matrix----------------------------------------------------
header_border[1, 4] <- 4
header_border

## ----custom_border_matrix_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/custombordertable"),
#              orientation = "landscape",
#              border_mat = header_border)

## ----custom_border_matrix_docx_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# export_TLG_as_docx(result,
#                 tblid = "custombordertable",
#                 output_dir = out_dir,
#                 orientation = "landscape",
#                 border_mat = header_border)

## ----custom_border_matrix_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "custombordertable", orientation = "landscape", border_mat = header_border)

## ----default_markup_rtf-------------------------------------------------------
library(tibble)

dps_markup_df <- tibble::tribble(
  ~keyword,
  ~rtfstart,
  ~rtfend,
  "super",
  "\\super",
  "\\nosupersub",
  "sub",
  "\\sub",
  "\\nosupersub",
  "optional",
  "",
  ""
)

dps_markup_df

## ----superscript_example_data-------------------------------------------------
adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

adsl

## ----superscript_table_rtf----------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = c("Example Drug~[super a] 5 mg",
                              "Example Drug~[super a] 10 mg",
                              "Example Drug~[super a] 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----superscript_table_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/superscripttable"),
#              orientation = "landscape",
#              markup_df = dps_markup_df)

## ----default_markup_docx2-----------------------------------------------------
library(tibble)

docx_markup_df <- tibble::tribble(
  ~keyword,
  ~replace_by,
  "super",
  "flextable::as_sup",
  "sub",
  "flextable::as_sub",
  "optional",
  ""
)

docx_markup_df

## ----superscript_table_docx---------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = c("Example Drug~[super a] 5 mg",
                              "Example Drug~[super a] 10 mg",
                              "Example Drug~[super a] 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)


## ----superscript_table_docx_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# export_TLG_as_docx(result,
#                 tblid = "superscripttable",
#                 output_dir = out_dir,
#                 orientation = "landscape",
#                 markup_df_docx = docx_markup_df)

## ----superscript_table_rtf_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "superscripttable", orientation = "landscape",  markup_df_docx = docx_markup_df)

## ----superscript_listing_rtf--------------------------------------------------
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for Period 01~[super a]") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)

## ----superscript_listing_rtf_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/examplelistingsuperscript2"),
#              orientation = "landscape")

## ----default_markup_docx------------------------------------------------------
library(tibble)

docx_markup_df <- tibble::tribble(
  ~keyword,
  ~replace_by,
  "super",
  "flextable::as_sup",
  "sub",
  "flextable::as_sub",
  "optional",
  ""
)

docx_markup_df

## ----superscript_listing_docx-------------------------------------------------

library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for Period 01~[super a]") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)


## ----superscript_listing_docx_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# export_TLG_as_docx(result,
#                 tblid = "examplelistingsuperscript2",
#                 output_dir = out_dir,
#                 orientation = "landscape",
#                 markup_df_docx = docx_markup_df)

## ----superscript_listing_rtf_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result, "examplelistingsuperscript2", orientation = "landscape",  markup_df_docx = docx_markup_df)

## ----multiple_docs_data_prep--------------------------------------------------
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

string_map <- default_str_map
trtvar <- "TRT01A"
popfl <- "SAFFL"

################################################################################
# Process Data:
################################################################################

adsl <- pharmaverseadamjnj::adsl %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl), RACE)

adae <- pharmaverseadamjnj::adae %>%
  filter(TRTEMFL == "Y" & AEBODSYS == "Skin and subcutaneous tissue disorders") %>%
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD, RACE)

adsl$colspan_trt <- factor(ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
                           levels = c("Active Study Agent", " ")
)

colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = trtvar
)

# Add total for Race - adsl
totalrace1 <- adsl %>%
  filter(RACE != "UNKNOWN" & !is.na(RACE)) %>%
  mutate(RACE = "Total")

adsl <- bind_rows(totalrace1, adsl)

adsl <- adsl %>%
  mutate(RACEcat = case_when(
    RACE == "Total" ~ "Total",
    RACE == "WHITE" ~ "White",
    RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
    RACE == "ASIAN" ~ "Asian",
    RACE != "UNKNOWN" & !is.na(RACE) ~ "Other"
  )) %>%
  filter(RACEcat %in% c("Total", "White", "Black", "Asian", "Other")) %>%
  select(-RACE)

adsl$spanheader <- factor(ifelse(adsl$RACEcat == "Total", " ", "Race"),
                          levels = c(" ", "Race")
)

adsl$RACEcat <- factor(adsl$RACEcat, levels = c("Total", "White", "Black", "Asian", "Other"))

# Add total for Race - adae
totalrace2 <- adae %>%
  filter(RACE != "UNKNOWN" & !is.na(RACE)) %>%
  mutate(RACE = "Total")

adae <- bind_rows(totalrace2, adae)

adae <- adae %>%
  mutate(RACEcat = case_when(
    RACE == "Total" ~ "Total",
    RACE == "WHITE" ~ "White",
    RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
    RACE == "ASIAN" ~ "Asian",
    RACE != "UNKNOWN" & !is.na(RACE) ~ "Other"
  )) %>%
  filter(RACEcat %in% c("Total", "White", "Black", "Asian", "Other")) %>%
  select(-RACE)

adae$RACEcat <- factor(adae$RACEcat, levels = c("Total", "White", "Black", "Asian", "Other"))

# join data together
ae <- left_join(adsl, adae, by = c("USUBJID", "RACEcat"))

################################################################################
# Define layout and build table:
################################################################################

extra_args_1 <- list(denom = "n_altdf",
                     .stats = c("count_unique_fraction")
)


extra_args_2 <- list(denom = "n_altdf",
                     .stats = c("count_unique")
)



lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = FALSE
) %>%
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map))

lyt <- lyt %>%
  split_cols_by(trtvar)

lyt <- lyt %>%
  split_cols_by("spanheader", split_fun = trim_levels_in_group("RACEcat")) %>%
  split_cols_by("RACEcat") %>%
  analyze(popfl,
          afun = a_freq_j,
          show_labels = "hidden",
          section_div = c(" "),
          extra_args = append(extra_args_2,
                              list(
                                label = "Analysis set: Safety",
                                val = "Y",
                                section_div = c(" ")))
  ) %>%
  analyze("TRTEMFL",
          afun = a_freq_j,
          show_labels = "hidden",
          extra_args = append(extra_args_1,
                              list(label = "Subjects with >=1 AE",
                                   val = "Y",
                                   section_div = c(" ")))
  ) %>%
  split_rows_by("AEBODSYS",
                split_label = "System Organ Class",
                split_fun = trim_levels_in_group("AEDECOD"),
                label_pos = "topleft",
                section_div = c(" "),
                nested = FALSE
  ) %>%
  summarize_row_groups("AEBODSYS",
                       cfun = a_freq_j,
                       extra_args = extra_args_1
  ) %>%
  analyze("AEDECOD",
          afun = a_freq_j,
          extra_args = extra_args_1
  ) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)
result <- set_titles(result, titles)
#########################################################################################
# Post-Processing step to sort by descending count on chosen active treatment columns.
# For this table we can use a defined colpath so it takes the appropriate sub-column ("Total")
# for the last active treatment group/combined and use this for its sort order.
# If you only have 1 active treatment arm, consider using jj_complex_scorefun(spanningheadercolvar = NA, usefirstcol = TRUE)
# See function documentation for jj_complex_scorefun should your require a different sorting behavior.
#########################################################################################

# col_paths_summary(result)

if (length(adae$TRTEMFL) != 0) {
  result <- sort_at_path(result, c("root", "AEBODSYS"),
                         scorefun = jj_complex_scorefun(colpath = c("colspan_trt", "Active Study Agent", trtvar, "Xanomeline High Dose", "spanheader", " ", "RACEcat", "Total"))
  )
  result <- sort_at_path(result, c("root", "AEBODSYS", "*", "AEDECOD"),
                         scorefun = jj_complex_scorefun(colpath = c("colspan_trt", "Active Study Agent", trtvar, "Xanomeline High Dose", "spanheader", " ", "RACEcat", "Total"))
  )
}

## ----multiple_docs_table1-----------------------------------------------------
################################################################################
# Convert to tbl file and output table
################################################################################


## ----multiple_docs_table1_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(string_map = string_map, tt = result,
#              file = paste0(out_dir, "/aetablemultipledocs1"), orientation = "landscape"
# )

## ----multiple_docs_table1_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
x <- tt_to_flextable_j(string_map = string_map, tt = result, "aetablemultipledocs1", orientation = "landscape", paginate= TRUE)
x[[1]]
knitr::asis_output("<br><br>")
x[[2]]
knitr::asis_output("<br><br>")

## ----multiple_docs_table2-----------------------------------------------------
################################################################################
# Convert to tbl file and output table
################################################################################


## ----multiple_docs_table2_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(string_map = string_map, tt = result,
#              file = paste0(out_dir, "/aetablemultipledocs2"), orientation = "landscape",
#              nosplitin = list(cols = c(trtvar))
# )

## ----multiple_docs_table2_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
x <- tt_to_flextable_j(string_map = string_map, tt = result, "aetablemultipledocs2", orientation = "landscape",
                  nosplitin = list(cols = c(trtvar)), paginate= TRUE)
x[[1]]
knitr::asis_output("<br><br>")
x[[2]]
knitr::asis_output("<br><br>")
x[[3]]
knitr::asis_output("<br><br>")


## ----multiple_docs_table3_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(string_map = string_map, tt = result,
#              file = paste0(out_dir, "/aetablemultipledocs3"), orientation = "landscape",
#              nosplitin = list(cols = c(trtvar)),
#              combined_rtf = TRUE
# )

## ----multiple_docs_table3_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
x <- tt_to_flextable_j(string_map = string_map, tt = result, "aetablemultipledocs3", orientation = "landscape",
                  nosplitin = list(cols = c(trtvar)), combined_rtf = TRUE, paginate = TRUE)
x[[1]]
knitr::asis_output("<br><br>")
x[[2]]
knitr::asis_output("<br><br>")
x[[3]]
knitr::asis_output("<br><br>")

## ----page_break_table---------------------------------------------------------
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",
                show_colcounts = FALSE
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

## ----page_break_table_export, eval=NOT_CRAN, message=FALSE, warning=FALSE-----
# tt_to_tlgrtf(result,
#              file = paste0(out_dir, "/examplepagebreak"))

## ----page_break_table_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
male <- result[1:5, keep_titles = TRUE, keep_footers = FALSE]
tt_to_flextable_j(male, "examplepagebreak")
knitr::asis_output("<br><br>")

## ----page_break_table_display_part2, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----

female <- result[6:10, keep_titles = TRUE, keep_footers = TRUE]
tt_to_flextable_j(female, "examplepagebreak")

## ----split_listing_part1------------------------------------------------------
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for Period 01~[super a]") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)
keep <- result$TRT01A %in% c("Example Drug 5 mg", "Example Drug 10 mg")

result1 <- result[keep, ]

## ----split_listing_part1_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result1,
#   file = paste0(out_dir, "/examplelistingmultiplefilesPART1OF2"),
#   orientation = "landscape"
# )

## ----split_listing_docx_part1_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# 
# export_TLG_as_docx(result1,
#   tblid = "examplelistingmultiplefilesPART1OF2",
#   output_dir = out_dir,
#   orientation = "landscape"
# )

## ----split_listing_part1_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result1, "examplelistingmultiplefilesPART1OF2",
  orientation = "landscape")

## ----split_listing_part2------------------------------------------------------

keep <- result$TRT01A %in% c("Example Drug 20 mg", "Placebo")

result2 <- result[keep, ]

## ----split_listing_part2_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# tt_to_tlgrtf(result2,
#   file = paste0(out_dir, "/examplelistingmultiplefilesPART2OF2"),
#   orientation = "landscape"
# )

## ----split_listing_docx_part2_export, eval=NOT_CRAN, message=FALSE, warning=FALSE----
# 
# export_TLG_as_docx(result2,
#   tblid = "examplelistingmultiplefilesPART2OF2",
#   output_dir = out_dir,
#   orientation = "landscape"
# )

## ----split_listing_part2_display, echo=FALSE, fig.align="center", out.width="100%", out.height="200px"----
tt_to_flextable_j(result2, "examplelistingmultiplefilesPART2OF2",
  orientation = "landscape")

