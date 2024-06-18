regtab <- function(x, y, weights = NULL, continuous = "slopes", show.ci = TRUE, conf.level = 0.95) {
  
  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("gtsummary package should be installed to use this function")
  
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt package should be installed to use this function")
  
  gtsummary::set_gtsummary_theme(
    gtsummary::theme_gtsummary_language(language = "en", decimal.mark = ".", big.mark = " ", ci.sep =" , ")) |>
    suppressMessages()
  
  compute_ci <- function(tbl) {
    body <- tbl$table_body
    lo <- round(100*body$conf.low, 1)
    hi <- round(100*body$conf.high, 1)
    lo[body$row_type == "level"] <- paste0(lo[body$row_type == "level"], "%")
    hi[body$row_type == "level"] <- paste0(hi[body$row_type == "level"], "%")
    lo[body$var_type == "continuous"] <- paste0(lo[body$var_type == "continuous"], " pp")
    hi[body$var_type == "continuous"] <- paste0(hi[body$var_type == "continuous"], " pp")
    lo[body$var_type == "continuous" & sign(body$conf.low)==1] <- paste0("+", lo[body$var_type == "continuous" & sign(body$conf.low)==1])
    hi[body$var_type == "continuous" & sign(body$conf.high)==1] <- paste0("+", hi[body$var_type == "continuous" & sign(body$conf.high)==1])
    res <- character(length = nrow(body))
    res[!is.na(lo)] <- paste(lo[!is.na(lo)], hi[!is.na(lo)], sep = " , ")
    res[res == ""] <- NA
    return(res)
  }
  
  if(is.null(weights)) weights <- rep(1, times = nrow(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  ww <- weights
  
  # y_name <- gsub("$", "_", deparse(substitute(y)), fixed = TRUE)
  facs <- names(x)[sapply(x, is.factor)]
  nums <- names(x)[sapply(x, function(x) is.numeric(x) | is.integer(x))]
  
  df <- data.frame(outcome = y, x)
  # names(df)[1] <- y_name
  
  # Predictions marginales univariees
  if(is.numeric(y) | is.integer(y)) {
    tb1b <- 
    gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                method = stats::lm,
                                method.args = list(weights = ww),
                                y = "outcome",
                                include = -ww,
                                hide_n = TRUE,
                                conf.level = conf.level,
                                add_estimate_to_reference_rows = TRUE,
                                tidy_fun = broom.helpers::tidy_marginal_predictions)
  } else if(is.factor(y) & nlevels(y) == 2) {
    tb1b <- 
    gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                method = stats::glm,
                                method.args = list(family = stats::binomial, weights = ww),
                                y = "outcome",
                                include = -ww,
                                type = "response",
                                estimate_fun = scales::label_percent(accuracy = 0.1),
                                hide_n = TRUE,
                                conf.level = conf.level,
                                add_estimate_to_reference_rows = TRUE,
                                tidy_fun = broom.helpers::tidy_marginal_predictions)
  } else {
    stop("y should be numeric or a dichotomous (a factor with two levels)")
  }
  
  # Ajout des pentes (si besoin)
  if(continuous == "predictions") {
      tb1c <- tb1b
  } else if(continuous == "slopes") {
      if(is.numeric(y) | is.integer(y)) {
        tb1a <- gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                            method = stats::lm,
                                            method.args = list(weights = ww),
                                            y = "outcome",
                                            include = -ww,
                                            hide_n = TRUE,
                                            conf.level = conf.level,
                                            add_estimate_to_reference_rows = TRUE,
                                            tidy_fun = broom.helpers::tidy_parameters)
        amp <- tb1b$table_body[tb1b$table_body$var_class == "factor" & tb1b$table_body$row_type == "level", c("variable", "label", "estimate", "ci")]
        names(amp)[c(3,4)] <- c("AMP_est", "AMP_ci")
        body <- tb1a$table_body
        body$id <- 1:nrow(body)
        body <- merge(body, amp, by = c("variable", "label"), all.x = TRUE, sort = FALSE)
        body <- body[order(body$id), ]
        body$estimate[body$var_class == "factor" & body$row_type == "level"] <- body$AMP_est[body$var_class == "factor" & body$row_type == "level"]
        body$ci[body$var_class == "factor" & body$row_type == "level"] <- body$AMP_ci[body$var_class == "factor" & body$row_type == "level"]
        body$AMP_ce <- body$AMP_ci <- body$id <- NULL
        tb1c <- tb1a
        tb1c$table_body <- body
      } else if(is.factor(y) & nlevels(y) == 2) {
        tb1a <- gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                            method = stats::glm,
                                            method.args = list(family = stats::binomial, weights = ww),
                                            y = "outcome",
                                            include = -ww,
                                            type = "response",
                                            hide_n = TRUE,
                                            conf.level = conf.level,
                                            add_estimate_to_reference_rows = TRUE,
                                            tidy_fun = broom.helpers::tidy_parameters)
        tb1d <- gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                            method = stats::glm,
                                            method.args = list(family = stats::binomial, weights = ww),
                                            y = "outcome",
                                            include = -ww,
                                            type = "response",
                                            hide_n = TRUE,
                                            conf.level = conf.level,
                                            add_estimate_to_reference_rows = TRUE,
                                            tidy_fun = broom.helpers::tidy_avg_slopes)
        amp <- tb1b$table_body[tb1b$table_body$var_class == "factor" & tb1b$table_body$row_type == "level", c("variable", "label", "estimate", "conf.low", "conf.high", "ci")]
        names(amp)[3:6] <- paste0("amp_", names(amp)[3:6])
        body <- tb1a$table_body
        body$id <- 1:nrow(body)
        body <- merge(body, amp, by = c("variable", "label"), all.x = TRUE, sort = FALSE)
        body[body$var_class == "factor" & body$row_type == "level", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_class == "factor" & body$row_type == "level", c("amp_estimate", "amp_conf.low", "amp_conf.high", "amp_ci")]
        body$amp_estimate <- body$amp_conf.low <- body$amp_conf.high <- body$amp_ci <- NULL
        slo <- tb1d$table_body[tb1d$table_body$var_type == "continuous" & tb1d$table_body$row_type == "level", c("variable", "estimate", "conf.low", "conf.high", "ci")]
        names(slo)[2:5] <- paste0("slo_", names(slo)[2:5])
        body <- merge(body, slo, by = "variable", all.x = TRUE, sort = FALSE)
        body[body$var_type == "continuous", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_type == "continuous", c("slo_estimate", "slo_conf.low", "slo_conf.high", "slo_ci")]
        body$slo_estimate <- body$slo_conf.low <- body$slo_conf.high <- body$slo_ci <- NULL
        body <- body[order(body$id), ]
        body$id <- NULL
        tb1c <- tb1a
        tb1c$table_body <- body
        tb1c$table_body$ci <- compute_ci(tb1c)
        tb1c <- tb1c |> 
          gtsummary::modify_fmt_fun(
            update = gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1),
            rows = .data$variable %in% facs) |>
          gtsummary::modify_fmt_fun(
            update = gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1, style_positive = "plus", suffix = " pp"),
            rows = .data$variable %in% nums)
      }

  }
  
  # masquage des colonnes p.value etc.
  if(isTRUE(show.ci)) {
    tb1c <- gtsummary::modify_column_hide(tb1c, columns = c("p.value", "std.error"))
  } else if(isFALSE(show.ci)) {
    tb1c <- gtsummary::modify_column_hide(tb1c, columns = c("ci", "p.value", "std.error"))
  }

  # Ajustement du modele multivarie
  if(is.numeric(y) | is.integer(y)) {
    mod <- stats::lm(outcome ~ ., data = df, weights = ww)
    tb2b <- gtsummary::tbl_regression(mod,
                                      conf.level = conf.level,
                                      add_estimate_to_reference_rows = TRUE,
                                      tidy_fun = broom.helpers::tidy_marginal_predictions)
  } else if(is.factor(y) & nlevels(y) == 2) {
    mod <- stats::glm(outcome ~ ., data = df, weights = ww, family = stats::binomial)
    tb2b <- gtsummary::tbl_regression(mod,
                                      type = "response",
                                      conf.level = conf.level,
                                      estimate_fun = scales::label_percent(accuracy = 0.1),
                                      add_estimate_to_reference_rows = TRUE,
                                      tidy_fun = broom.helpers::tidy_marginal_predictions)
  }
  
  # Ajout des pentes (si besoin)
  if(continuous == "predictions") {
    tb2c <- tb2b
  } else if(continuous == "slopes") {
    if(is.numeric(y) | is.integer(y)) {
      tb2a <- gtsummary::tbl_regression(mod,
                                        conf.level = conf.level,
                                        add_estimate_to_reference_rows = TRUE,
                                        tidy_fun = broom.helpers::tidy_parameters)
      amp <- tb2b$table_body[tb2b$table_body$var_class == "factor" & tb2b$table_body$row_type == "level", c("variable", "label", "estimate", "ci")]
      names(amp)[c(3,4)] <- c("AMP_est", "AMP_ci")
      body <- tb2a$table_body
      body$id <- 1:nrow(body)
      body <- merge(body, amp, by = c("variable", "label"), all.x = TRUE, sort = FALSE)
      body <- body[order(body$id), ]
      body$estimate[body$var_class == "factor" & body$row_type == "level"] <- body$AMP_est[body$var_class == "factor" & body$row_type == "level"]
      body$ci[body$var_class == "factor" & body$row_type == "level"] <- body$AMP_ci[body$var_class == "factor" & body$row_type == "level"]
      body$AMP_est <- body$AMP_ci <- body$id <- NULL
      tb2c <- tb2a
      tb2c$table_body <- body
    } else if(is.factor(y) & nlevels(y) == 2) {
      tb2a <- gtsummary::tbl_regression(mod,
                                        type = "response",
                                        conf.level = conf.level,
                                        add_estimate_to_reference_rows = TRUE,
                                        tidy_fun = broom.helpers::tidy_parameters)
      tb2d <- gtsummary::tbl_regression(mod,
                                        type = "response",
                                        conf.level = conf.level,
                                        add_estimate_to_reference_rows = TRUE,
                                        tidy_fun = broom.helpers::tidy_avg_slopes)
      amp <- tb2b$table_body[tb2b$table_body$var_class == "factor" & tb2b$table_body$row_type == "level", c("variable", "label", "estimate", "conf.low", "conf.high", "ci")]
      names(amp)[3:6] <- paste0("amp_", names(amp)[3:6])
      body <- tb2a$table_body
      body$id <- 1:nrow(body)
      body <- merge(body, amp, by = c("variable", "label"), all.x = TRUE, sort = FALSE)
      body[body$var_class == "factor" & body$row_type == "level", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_class == "factor" & body$row_type == "level", c("amp_estimate", "amp_conf.low", "amp_conf.high", "amp_ci")]
      body$amp_estimate <- body$amp_conf.low <- body$amp_conf.high <- body$amp_ci <- NULL
      slo <- tb2d$table_body[tb2d$table_body$var_type == "continuous" & tb2d$table_body$row_type == "level", c("variable", "estimate", "conf.low", "conf.high", "ci")]
      names(slo)[2:5] <- paste0("slo_", names(slo)[2:5])
      body <- merge(body, slo, by = "variable", all.x = TRUE, sort = FALSE)
      body[body$var_type == "continuous", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_type == "continuous", c("slo_estimate", "slo_conf.low", "slo_conf.high", "slo_ci")]
      body$slo_estimate <- body$slo_conf.low <- body$slo_conf.high <- body$slo_ci <- NULL
      body <- body[order(body$id), ]
      body$id <- NULL
      tb2c <- tb2a
      tb2c$table_body <- body
      tb2c$table_body$ci <- compute_ci(tb2c)
      tb2c <- tb2c |> 
        gtsummary::modify_fmt_fun(
          update = gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1),
          rows = .data$variable %in% facs) |>
        gtsummary::modify_fmt_fun(
          update = gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1, style_positive = "plus", suffix = " pp"),
          rows = .data$variable %in% nums)
    }
  }

  # Masquage des colonnes p.value, etc.
  if(isTRUE(show.ci)) {
    tb2c <- gtsummary::modify_column_hide(tb2c, columns = c("p.value", "std.error"))
  } else if(isFALSE(show.ci)) {
    tb2c <- gtsummary::modify_column_hide(tb2c, columns = c("ci", "p.value", "std.error"))
  }
  
  # Fusion des tableaux
  res <- gtsummary::tbl_merge(list(tb1c, tb2c)) |>
         gtsummary::bold_labels() |>
         gtsummary::modify_fmt_fun(update = gtsummary::starts_with("ci") ~ function(x) gsub("^(.*)$", "\\[\\1\\]", x))
  
  # Intitules des colonnes
  if(isTRUE(show.ci)) {
    res <- res |> 
      gtsummary::modify_header(label ~ "",
                               gtsummary::starts_with("estimate_") ~ "AME",
                               gtsummary::starts_with("ci_") ~ "95% CI") |>
      gtsummary::modify_footnote(update = gtsummary::everything() ~ NA, abbreviation = TRUE) |>
      gtsummary::modify_spanning_header(gtsummary::ends_with("_1") ~ "**univariate**",
                                        gtsummary::ends_with("_2") ~ "**multivariate**")
  } else if(isFALSE(show.ci)) {
    res <- res |> 
            gtsummary::modify_header(label ~ "",
                                     estimate_1 ~ "univariate",
                                     estimate_2 ~ "multivariate") |>
            gtsummary::modify_spanning_header(gtsummary::starts_with("estimate") ~ "**average marginal effects**")
  }
  
  # show_header_names(res)
  # gtsummary::reset_gtsummary_theme()
  return(res)
  
}

# gtsummary::theme_gtsummary_language(language = "en", decimal.mark = ".", big.mark = " ", ci.sep =" , ")
# data(Movies)
# (res11 <- regtab(y = Movies$BoxOffice,
#                  x = Movies[, c("Country", "Budget", "ArtHouse", "Critics")],
#                  continuous = "slopes"))
# (res12 <- regtab(y = Movies$BoxOffice,
#                  x = Movies[, c("Country", "Budget", "ArtHouse", "Critics")],
#                  continuous = "predictions"))
# (res21 <- regtab(y = Movies$Festival,
#                  x = Movies[, c("Country", "Budget", "ArtHouse", "Critics")],
#                  continuous = "slopes"))
# (res22 <- regtab(y = Movies$Festival,
#                  x = Movies[, c("Country", "Budget", "ArtHouse", "Critics")],
#                  continuous = "predictions"))
# gt21 <- gtsummary::as_gt(res21)
# gt21
