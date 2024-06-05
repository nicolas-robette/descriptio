crosstab <- function(x,
                     y,
                     xstrata = NULL,
                     ystrata = NULL,
                     weights = NULL,
                     stat = "rprop",
                     show.n = FALSE,
                     show.cramer = TRUE,
                     na.rm = FALSE,
                     na.value = "NAs",
                     digits = 1,
                     sort = "none",
                     color.cells = FALSE,
                     measure = "phi",
                     limits = c(-1, 1),
                     min.asso = 0.1, 
                     palette = "PRGn",
                     reverse = FALSE) {

  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("gtsummary package should be installed to use this function")
 
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt package should be installed to use this function")
  
  if (!requireNamespace("survey", quietly = TRUE))
    stop("survey package should be installed to use this function")  
  
  if(is.null(weights)) weights <- rep(1, times = length(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  x_name <- gsub("$", "_", deparse(substitute(x)), fixed = TRUE)
  y_name <- gsub("$", "_", deparse(substitute(y)), fixed = TRUE)
  
  if(na.rm==FALSE) {
    if(any(is.na(x))) {
      x <- factor(x, levels=c(levels(x), na.value))
      x[is.na(x)] <- na.value
      # x <- factor(x)
    }
    if(any(is.na(y))) {
      y <- factor(y, levels=c(levels(y), na.value))
      y[is.na(y)] <- na.value
      # y <- factor(y)      
    }
  } else {
    complete <- !(is.na(x) | is.na(y))
    x <- x[complete]
    y <- y[complete]
    weights <- weights[complete]
  }
  
  if(sort!="none") {
    temp <- MASS::corresp(~x+y,nf=1)
    if(sort %in% c("x","both")) x <- factor(x, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) y <- factor(y, levels=names(sort(temp$cscore)))
  }

  df <- data.frame(x, y)
  names(df) <- c(x_name, y_name)
  
  if(!is.null(xstrata)) {
    xstrata_name <- gsub("$", "_", deparse(substitute(xstrata)), fixed = TRUE)
    df <- cbind.data.frame(df, xstrata)
    names(df)[ncol(df)] <- xstrata_name
  }
  
  if(!is.null(ystrata)) {
    ystrata_name <- gsub("$", "_", deparse(substitute(ystrata)), fixed = TRUE)
    df <- data.frame(df, ystrata)
    names(df)[ncol(df)] <- ystrata_name
  }
  
  dfw <- survey::svydesign(ids = ~ 1, data = df, weights = ~ weights)
  dfw$variables$Total = 1

  if(stat == "prop") {
    stat <- "cell"
  } else if(stat == "rprop") {
    stat <- "row"
  } else if(stat == "cprop") {
    stat <- "col"
  }
  
  if(stat %in% c("cell", "row", "col")) {
    per <- stat
    sta <- "{p}"
    if(show.n) sta <- paste0(sta, " ({n})")
  } else if(stat == "freq") {
    per <- NULL
    sta <- "{n}"
    digits <- 0
  }

  if(is.null(xstrata) & is.null(ystrata)) {
    tab1 <- 
      gtsummary::tbl_svysummary(dfw,
                                by = gtsummary::all_of(y_name),
                                missing = "no", # ifelse(na.rm, "no", "ifany"),
                                # missing_text = na.value,
                                percent = per,
                                statistic = gtsummary::all_categorical() ~ sta,
                                digits = list(gtsummary::all_categorical() ~ c(digits,0))) |>
      gtsummary::add_overall(last = TRUE) |>
      gtsummary::modify_header(list(label ~ "",
                                    gtsummary::all_stat_cols(stat_0 = FALSE) ~ "{level}",
                                    stat_0 ~ "Total")) |>
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
      gtsummary::modify_spanning_header(gtsummary::all_stat_cols(stat_0 = FALSE) ~ paste0("**",
                                                                                          ifelse(is.null(attr(y,"label")), y_name, attr(y,"label")),
                                                                                          "**")) |>
      gtsummary::bold_labels()

  } else if (!is.null(xstrata) & is.null(ystrata)) {
    tab1 <- gtsummary::tbl_strata(
               dfw,
               strata = gtsummary::all_of(xstrata_name),
               .tbl_fun = ~ .x |> 
                   gtsummary::tbl_svysummary(include = c(x_name, Total),
                                             by = gtsummary::all_of(y_name),
                                             missing = "no", #ifelse(na.rm, "no", "ifany"),
                                             # missing_text = na.value,
                                             percent = per,
                                             statistic = gtsummary::all_categorical() ~ sta,
                                             digits = list(gtsummary::all_categorical() ~ c(digits,0))) |>
                   gtsummary::add_overall(last = TRUE) |>
                   gtsummary::modify_header(list(label ~ "",
                                                 gtsummary::all_stat_cols(stat_0 = FALSE) ~ "{level}",
                                                 stat_0 ~ "Total")) |>
                   gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
                   gtsummary::modify_spanning_header(gtsummary::all_stat_cols(stat_0 = FALSE) ~ paste0("**",
                                                                                                       ifelse(is.null(attr(y,"label")), y_name, attr(y,"label")),
                                                                                                       "**")) |>
                   gtsummary::bold_labels(),
               .combine_with = "tbl_stack",
               .header = paste0(xstrata_name, " = {strata}"))

  } else if (is.null(xstrata) & !is.null(ystrata)) {
    tab1 <- gtsummary::tbl_strata(
      dfw,
      strata = gtsummary::all_of(ystrata_name),
      .tbl_fun = ~ .x |> 
        gtsummary::tbl_svysummary(include = c(x_name, Total),
                                  by = gtsummary::all_of(y_name),
                                  missing = "no", #ifelse(na.rm, "no", "ifany"),
                                  # missing_text = na.value,
                                  percent = per,
                                  statistic = gtsummary::all_categorical() ~ sta,
                                  digits = list(gtsummary::all_categorical() ~ c(digits,0))) |>
        gtsummary::add_overall(last = TRUE) |>
        gtsummary::modify_header(list(label ~ "",
                                      gtsummary::all_stat_cols(stat_0 = FALSE) ~ "{level}",
                                      stat_0 ~ "Total")) |>
        gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
        #gtsummary::modify_spanning_header(gtsummary::all_stat_cols(stat_0 = FALSE) ~ paste0("**",y_name,"**")) |>
        gtsummary::bold_labels(),
      .combine_with = "tbl_merge",
      .header = paste0(ystrata_name, " = {strata}")
      ) |>
      gtsummary::modify_caption(paste0("**", ifelse(is.null(attr(y,"label")), y_name, attr(y,"label")), "**"))
    
  } else if (!is.null(xstrata) & !is.null(ystrata)) {
    tab1 <- gtsummary::tbl_strata(
      dfw,
      strata = gtsummary::all_of(xstrata_name),
      .tbl_fun = ~ .x |> 
        gtsummary::tbl_strata(
          strata = gtsummary::all_of(ystrata_name),
          .tbl_fun = ~ .x |> 
              gtsummary::tbl_svysummary(include = c(x_name, Total),
                                        by = gtsummary::all_of(y_name),
                                        missing = "no", #ifelse(na.rm, "no", "ifany"),
                                        # missing_text = na.value,
                                        percent = per,
                                        statistic = gtsummary::all_categorical() ~ sta,
                                        digits = list(gtsummary::all_categorical() ~ c(digits,0))) |>
              gtsummary::add_overall(last = TRUE) |>
              gtsummary::modify_header(list(label ~ "",
                                            gtsummary::all_stat_cols(stat_0 = FALSE) ~ "{level}",
                                            stat_0 ~ "Total")) |>
              gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
              #gtsummary::modify_spanning_header(gtsummary::all_stat_cols(stat_0 = FALSE) ~ paste0("**","{strata}","**")) |>
              gtsummary::bold_labels(),
          .combine_with = "tbl_merge",
          .header = paste0(ystrata_name, " = {strata}")
          ),
      .combine_with = "tbl_stack",
      .header = paste0(xstrata_name, " = {strata}")
      ) |>
      gtsummary::modify_caption(paste0("**", ifelse(is.null(attr(y,"label")), y_name, attr(y,"label")), "**"))
  }
    
  tab2 <- gtsummary::as_gt(tab1)
  
  tab2 <-
    tab2 |>
    ## bold labels of stratas
    gt::tab_style(
      style = gt::cell_text(weight = "bolder"),
      locations =  list(gt::cells_row_groups(),
                        gt::cells_column_spanners())
    ) |>
    ## delete decimals for 100%
    gt::text_replace(
      locations = gt::cells_body(),
      pattern = paste0("100,", paste0(rep("0",digits), collapse = "")),
      replacement = "100"
    ) |>
    ## line breaks when percents + n
    gt::text_replace(
      locations = gt::cells_body(),
      pattern = " \\(",
      replacement = "<br>("
    ) |>
    ## totals in italic
    gt::tab_style(
      style = list(gt::cell_text(style = "italic",
                                 weight = 350,
                                 color = "grey25"),
                   gt::cell_fill(color = "white")
      ),
      locations = list(
        gt::cells_body(columns = gt::starts_with("stat_0")),
        gt::cells_body(rows = which(gt::extract_body(tab2)$variable == "Total")),
        gt::cells_column_labels(columns = gt::starts_with("stat_0"))
      )
    )
  
  ## detailed x label
  if(!is.null(attr(x,"label"))) {
    tab2 <-
      tab2 |>
      gt::text_replace(
        locations = gt::cells_body(),
        pattern = paste0("^",x_name,"$"),
        replacement = attr(x,"label")
      )
  }

  ## detailed xstrata label
  if(!is.null(attr(xstrata,"label"))) {
    tab2 <- 
      tab2 |>
        gt::text_replace(
          locations = gt::cells_row_groups(),
          pattern = paste0("^", xstrata_name),
          replacement = attr(xstrata, "label")
      )
  }
  
  ## detailed ystrata label
  if(!is.null(attr(ystrata,"label"))) {
    tab2 <- 
      tab2 |>
        gt::text_replace(
          locations = gt::cells_column_spanners(),
          pattern = paste0("^", ystrata_name),
          replacement = attr(ystrata, "label")
        )
  }

  ## vertical line between merged tables
  if(!is.null(ystrata)) {
  tab2 <- 
    tab2 |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "left",
        color = "gray80",
        weight = gt::px(2),
        style = "solid"
      ),
      locations =  list(gt::cells_body(columns = "stat_1_2"))
                        # cells_column_labels(columns = "stat_1_2"),
                        # cells_column_spanners(2)
      ) 
  }
  
  ## Cramer V and color when no strata
  if(is.null(xstrata) & is.null(ystrata)) {
    if(show.cramer) {
      cram <- weighted.cramer(factor(x), factor(y), weights, na.rm = TRUE)
      tab2 <- 
        tab2 |>
        gt::tab_source_note(source_note = paste0("Cramer's V = ", round(cram,2))) |>
        gt::tab_style(style = gt::cell_text(size = "xx-small"),
                      locations = gt::cells_source_notes())
    }
    if(color.cells) {
      if (!requireNamespace("scales", quietly = TRUE))
        stop("scales package should be installed to use this function")
      if(measure == "phi") {
        asso <- phi.table(x, y, weights, na.rm = TRUE)
      } else if(measure == "std.residuals") {
        asso <- stdres.table(x, y, weights, na.rm = TRUE, residuals = "std")
      } else if(measure == "adj.residuals") {
        asso <- stdres.table(x, y, weights, na.rm = TRUE, residuals = "adj")
      } else if(measure == "pem") {
        asso <- pem.table(x, y, weights, na.rm = TRUE)$peml
      }
      couleurs <- scales::col_numeric(palette = palette, domain = limits, reverse = reverse)(as.numeric(asso))
      couleurs <- matrix(couleurs, nrow = nrow(asso), byrow = FALSE)
      for(i in 1:nrow(asso)) {
        for(j in 1:ncol(asso)) {
          col <- ifelse(abs(asso[i,j]) >= min.asso, couleurs[i,j], "white")
          tab2 <- gt::tab_style(tab2, 
                                style = gt::cell_fill(color = col),
                                locations = gt::cells_body(columns = paste0("stat_",j), rows = i+1)
          )
        }
      }
    }
  }
  
  ## Cramer V and color when x strata
  if(!is.null(xstrata) & is.null(ystrata)) {
    if(show.cramer) {
      cram <- 
        data.frame(x, y, weights) |>
          split(xstrata) |>
          sapply(function(z) with(z, weighted.cramer(factor(x), factor(y), weights, na.rm = na.rm))) |>
          round(2) |>
          paste0(collapse = " <br> ")
      tab2 <- 
        tab2 |>
        gt::tab_source_note(source_note = gt::html(paste0("Cramer's V = <br>", cram))) |>
        gt::tab_style(style = gt::cell_text(size = "xx-small"),
                      locations = gt::cells_source_notes())
    }
    if(color.cells) {
      if (!requireNamespace("scales", quietly = TRUE))
        stop("scales package should be installed to use this function")
      temp <- data.frame(x, y, weights) |> split(xstrata)
      for(k in 1:nlevels(xstrata)) {
        if(measure == "phi") {
          asso <- with(temp[[k]], phi.table(x, y, weights, na.rm = TRUE))
        } else if(measure == "std.residuals") {
          asso <- with(temp[[k]], stdres.table(x, y, weights, na.rm = TRUE, residuals = "std"))
        } else if(measure == "adj.residuals") {
          asso <- with(temp[[k]], stdres.table(x, y, weights, na.rm = TRUE, residuals = "adj"))
        } else if(measure == "pem") {
          asso <- with(temp[[k]], pem.table(x, y, weights, na.rm = TRUE)$peml)
        }
        # asso <- with(temp[[k]], assoc.twocat(x, y, weights, na.rm = TRUE)$local$phi)
        couleurs <- scales::col_numeric(palette = palette, domain = limits, reverse = reverse)(as.numeric(asso))
        couleurs <- matrix(couleurs, nrow = nrow(asso), byrow = FALSE)
        for(i in 1:nrow(asso)) {
          for(j in 1:ncol(asso)) {
            col <- ifelse(abs(asso[i,j]) >= min.asso, couleurs[i,j], "white")
            tab2 <- gt::tab_style(tab2,
                                  style = gt::cell_fill(color = col),
                                  locations = gt::cells_body(columns = paste0("stat_",j), rows = min(which(gt::extract_body(tab2)$tbl_id1==k))+i)
            )
          }
        }
      }
    }
  }
  
  ## Cramer V and color when y strata
  if(is.null(xstrata) & !is.null(ystrata)) {
    if(show.cramer) {
      cram <- 
        data.frame(x, y, weights) |>
        split(ystrata) |>
        sapply(function(z) with(z, weighted.cramer(factor(x), factor(y), weights, na.rm = na.rm))) |>
        round(2) |>
        paste0(collapse = " | ")
      tab2 <- 
        tab2 |>
        gt::tab_source_note(source_note = paste0("Cramer's V = ", cram)) |>
        gt::tab_style(style = gt::cell_text(size = "xx-small"),
                      locations = gt::cells_source_notes())
    }
    if(color.cells) {
      if (!requireNamespace("scales", quietly = TRUE))
        stop("scales package should be installed to use this function")
      temp <- data.frame(x, y, weights) |> split(ystrata)
      for(k in 1:nlevels(ystrata)) {
        if(measure == "phi") {
          asso <- with(temp[[k]], phi.table(x, y, weights, na.rm = TRUE))
        } else if(measure == "std.residuals") {
          asso <- with(temp[[k]], stdres.table(x, y, weights, na.rm = TRUE, residuals = "std"))
        } else if(measure == "adj.residuals") {
          asso <- with(temp[[k]], stdres.table(x, y, weights, na.rm = TRUE, residuals = "adj"))
        } else if(measure == "pem") {
          asso <- with(temp[[k]], pem.table(x, y, weights, na.rm = TRUE)$peml)
        }
        # asso <- with(temp[[k]], assoc.twocat(x, y, weights, na.rm = TRUE)$local$phi)
        couleurs <- scales::col_numeric(palette = palette, domain = limits, reverse = reverse)(as.numeric(asso))
        couleurs <- matrix(couleurs, nrow = nrow(asso), byrow = FALSE)
        for(i in 1:nrow(asso)) {
          for(j in 1:ncol(asso)) {
            col <- ifelse(abs(asso[i,j]) >= min.asso, couleurs[i,j], "white")
            tab2 <- gt::tab_style(tab2,
                                  style = gt::cell_fill(color = col),
                                  locations = gt::cells_body(columns = paste0("stat_",j,"_",k), rows = i+1)
            )
          }
        }
      }
    }
  }
  
  ## Cramer V and color when x and y stratas
  if(!is.null(xstrata) & !is.null(ystrata)) {
    if(show.cramer) {
      cram <- 
        data.frame(x, y, weights, ystrata) |>
        split(xstrata) |>
        sapply(function(z) split(z, z$ystrata) |>
                           sapply(function(zz) with(zz, weighted.cramer(factor(x), factor(y), weights, na.rm = na.rm))) |>
                                               round(2) |>
                                               paste0(collapse = " </td><td> ")) |>
        paste0(collapse = " </td></tr><tr><td> ")
      tab2 <- 
        tab2 |>
        gt::tab_source_note(source_note = gt::html(paste0("Cramer's V = <table><tbody><tr><td>", cram, "</td></tr></tbody></table>"))) |>
        gt::tab_style(style = gt::cell_text(size = "xx-small"),
                      locations = gt::cells_source_notes())
    }
    if(color.cells) {
      if (!requireNamespace("scales", quietly = TRUE))
        stop("scales package should be installed to use this function")
      temp <- data.frame(x, y, weights, xstrata) |> split(ystrata)
      for(k in 1:nlevels(ystrata)) {
        temp2 <- split(temp[[k]], temp[[k]]$xstrata)
        for(h in 1:nlevels(xstrata)) {
          if(measure == "phi") {
            asso <- with(temp2[[h]], phi.table(x, y, weights, na.rm = TRUE))
          } else if(measure == "std.residuals") {
            asso <- with(temp2[[h]], stdres.table(x, y, weights, na.rm = TRUE, residuals = "std"))
          } else if(measure == "adj.residuals") {
            asso <- with(temp2[[h]], stdres.table(x, y, weights, na.rm = TRUE, residuals = "adj"))
          } else if(measure == "pem") {
            asso <- with(temp2[[h]], pem.table(x, y, weights, na.rm = TRUE)$peml)
          }
          # asso <- with(temp2[[h]], assoc.twocat(x, y, weights, na.rm = TRUE)$local$phi)
          couleurs <- scales::col_numeric(palette = palette, domain = limits, reverse = reverse)(as.numeric(asso))
          couleurs <- matrix(couleurs, nrow = nrow(asso), byrow = FALSE)
          for(i in 1:nrow(asso)) {
            for(j in 1:ncol(asso)) {
              col <- ifelse(abs(asso[i,j]) >= min.asso, couleurs[i,j], "white")
              tab2 <- gt::tab_style(tab2,
                                    style = gt::cell_fill(color = col),
                                    locations = gt::cells_body(columns = paste0("stat_",j,"_",k), rows = min(which(gt::extract_body(tab2)$tbl_id1==h))+i)
            )
            }
          }
        }
      }
    }
  }
  
  return(tab2)
}
