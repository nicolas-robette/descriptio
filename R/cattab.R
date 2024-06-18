# library(gtsummary)
# data(trial)
# str(trial)
# trial2 = select(trial, stage, age, trt, ttdeath)
# grade = trial$grade

cattab <- function(x,
                   y,
                   weights = NULL,
                   percent = "column",
                   robust = TRUE,
                   show.n = TRUE,
                   show.asso = TRUE,
                   digits = c(1,1),
                   na.rm = TRUE,
                   na.value = "NAs") {
  
  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("gtsummary package should be installed to use this function")
  
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt package should be installed to use this function")
  
  if (!requireNamespace("survey", quietly = TRUE))
    stop("survey package should be installed to use this function")  
  
  if(is.null(weights)) weights <- rep(1, times = nrow(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  my_cramer <- function(data, variable, by, ...) {
    weighted.cramer(data$variables[[variable]], data$variables[[by]], weights = weights)
  }
  
  my_eta <- function(data, variable, by, ...) {
    stats::summary.lm(stats::aov(data$variables[[variable]] ~ data$variables[[by]], weights = weights))$r.squared
  }
  
  # x_name <- gsub("$", "_", deparse(substitute(x)), fixed = TRUE)
  y_name <- gsub("$", "_", deparse(substitute(y)), fixed = TRUE)
  
  df <- data.frame(y,x)
  names(df)[1] <- y_name
  
  dfw <- survey::svydesign(ids = ~ 1, data = df, weights = ~ weights)
  
  res <- gtsummary::tbl_svysummary(dfw,
                                   by = gtsummary::all_of(y_name),
                                   percent = percent,
                                   type = list(gtsummary::all_continuous() ~ "continuous2"),
                                   statistic = list(gtsummary::all_categorical() ~ paste("{p}%",ifelse(show.n," ({n})","")),
                                                    gtsummary::all_continuous2() ~ c(ifelse(robust,"{median}","{mean} ({sd})"),"({p25} - {p75})")),
                                   digits = list(gtsummary::all_categorical() ~ c(digits[1],0),
                                                 gtsummary::all_continuous2() ~ digits[2]),
                                   missing = ifelse(na.rm, "no", "ifany"),
                                   missing_text = na.value) |>
      gtsummary::add_overall(last = TRUE) |>
      gtsummary::modify_header(label ~ "",
                               gtsummary::all_stat_cols(stat_0 = FALSE) ~ "{level} <br> _(n={n})_",
                               stat_0 ~ "**Total** <br> _(n={n})_") |>
      gtsummary::modify_footnote(gtsummary::all_stat_cols() ~ NA) |>
      gtsummary::modify_spanning_header(gtsummary::all_stat_cols(stat_0 = FALSE) ~ paste0("**",
                                                                                          ifelse(is.null(attr(y,"label")), y_name, attr(y,"label")),
                                                                                          "**")) |>
      gtsummary::bold_labels()
  
  if(show.asso) {
    res  <- res |>
      gtsummary::add_stat(fns = list(gtsummary::all_categorical() ~ my_cramer,
                                     gtsummary::all_continuous2() ~ my_eta)) |>
      gtsummary::modify_header(add_stat_1 ~ "**Association**") |>
      gtsummary::modify_footnote(gtsummary::all_stat_cols() ~ NA,
                                 add_stat_1 ~ "Cramer's V (categorical var.) or eta-squared (continuous var.)")
  }
  
  res <- gtsummary::as_gt(res)
  
  res <- res |>
    gt::text_replace(
      locations = gt::cells_body(),
      pattern = "Median",
      replacement = "median") |>
    gt::text_replace(
      locations = gt::cells_body(),
      pattern = "Mean \\(SD\\)",
      replacement = "mean (sd)") |>
    gt::text_replace(
      locations = gt::cells_body(),
      pattern = "IQR",
      replacement = "p25 - p75") |>
    gt::tab_style(
      style = list(gt::cell_text(style = "italic",
                                 weight = 350,
                                 color = "grey25"),
                   gt::cell_fill(color = "white")),
      locations = list(
        gt::cells_body(columns = gt::starts_with("stat_0")),
        gt::cells_column_labels(columns = gt::starts_with("stat_0")))
    )
  
  return(res)
    
}


# cattab(trial2, grade)
# cattab(trial2, grade, percent = "row")
# cattab(trial2, grade, show.n = FALSE, robust = FALSE, na.rm = FALSE)
