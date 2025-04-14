# Function for creating text object of basic model ----

create.text.object <- function(varx, vary, waves) {
  wave_labels <- sprintf("%02d", waves)
  wmf <- waves[-1] # waves minus first
  wml <- waves[-length(waves)] # waves minus last

  bwcomp <- paste(
    # Between components
    paste0("RI_y =~ ", paste0("1*", vary, "_w", wave_labels, collapse = " + ")),
    paste0("RI_x =~ ", paste0("1*", varx, "_w", wave_labels, collapse = " + ")),
    # Within components
    paste0("dep", waves, " =~ 1*", vary, "_w", wave_labels, collapse = "\n"),
    paste0("indep", waves, " =~ 1*", varx, "_w", wave_labels, collapse = "\n"),
    # Construct the measurement error variances to zero
    paste0(vary, "_w", wave_labels, " ~~ 0*", vary, "_w", wave_labels, collapse = "\n"),
    paste0(varx, "_w", wave_labels, " ~~ 0*", varx, "_w", wave_labels, collapse = "\n"),
    sep = "\n"
  )

  varcov <- paste(
    # Estimate the covariance between the components within t=1
    paste0("dep", waves[1], " ~~ indep", waves[1]),
    # Estimate the covariances between the residuals of the within component.
    paste0("dep", wmf, " ~~ indep", wmf, collapse = "\n"),
    # Estimate the residual variances of the within component
    paste0("dep", wmf, " ~~ dep", wmf, collapse = "\n"),
    paste0("indep", wmf, " ~~ indep", wmf, collapse = "\n"),
    # Estimate the variance and covariance between RIs.
    "RI_x ~~ RI_x\nRI_y ~~ RI_y\nRI_x ~~ RI_y",
    # Set the correlation between the RI and components within t=1 to zero
    paste0(
      "RI_x ~~ 0*dep", waves[1], "\nRI_x ~~ 0*indep", waves[1],
      "\nRI_y ~~ 0*dep", waves[1], "\nRI_y ~~ 0*indep", waves[1]
    ),
    sep = "\n"
  )

  # Regression models

  a1 <- paste(
    paste0("dep", wmf, " ~ dep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ indep", wml, collapse = "\n"),
    sep = "\n"
  )

  a2 <- paste(
    paste0("dep", wmf, " ~ a*dep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ d*indep", wml, collapse = "\n"),
    sep = "\n"
  )

  b1 <- paste(
    paste0("dep", wmf, " ~ dep", wml, " + indep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ indep", wml, collapse = "\n"),
    sep = "\n"
  )

  b2 <- paste(
    paste0("dep", wmf, " ~ a*dep", wml, " + b*indep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ d*indep", wml, collapse = "\n"),
    sep = "\n"
  )

  c1 <- paste(
    paste0("dep", wmf, " ~ dep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ dep", wml, " + indep", wml, collapse = "\n"),
    sep = "\n"
  )

  c2 <- paste(
    paste0("dep", wmf, " ~ a*dep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ c*dep", wml, " + d*indep", wml, collapse = "\n"),
    sep = "\n"
  )

  d1 <- paste(
    paste0("dep", wmf, " ~ dep", wml, " + indep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ dep", wml, " + indep", wml, collapse = "\n"),
    sep = "\n"
  )

  d2 <- paste(
    paste0("dep", wmf, " ~ a*dep", wml, " + b*indep", wml, collapse = "\n"),
    paste0("indep", wmf, " ~ c*dep", wml, " + d*indep", wml, collapse = "\n"),
    sep = "\n"
  )

  return(list(
    bwcomp = bwcomp,
    varcov = varcov,
    a1 = a1,
    a2 = a2,
    b1 = b1,
    b2 = b2,
    c1 = c1,
    c2 = c2,
    d1 = d1,
    d2 = d2
  ))
}

# Function to compare GOF ----

gof.comp <- function(data,
                     pairs,
                     measures = c(
                       "CFI", "TLI", "RMSEA", "SRMR",
                       "AIC", "BIC", "aBIC", "par", "LL"
                     )) {
  comp <- list()
  for (i in 1:length(pairs)) {
    gof <- data
    nest <- pairs[[i]][1]
    full <- pairs[[i]][2]
    delta <- NULL
    for (k in measures) {
      delta[paste0(k, "_D")] <-
        gof[m == nest, get(k)] - gof[m == full, get(k)]
    }
    par_LLcorf_nest <- gof[m == nest, par] * gof[m == nest, LLcorrectf]
    par_LLcorf_full <- gof[m == full, par] * gof[m == full, LLcorrectf]
    delta["CD"] <- (par_LLcorf_nest - par_LLcorf_full) / delta["par_D"]
    delta["TRd"] <- (-2 * delta["LL_D"]) / delta["CD"]
    delta["TRd_df"] <- gof[m == full, "par"] - gof[m == nest, "par"]
    delta["TRd_pvalue"] <- pchisq(as.numeric(delta["TRd"]),
      as.numeric(delta["TRd_df"]),
      lower.tail = F
    )
    comp[[paste0(nest, " vs. ", full, sep = "")]] <- delta
  }
  comp <- data.table(comp = names(comp), dplyr::bind_rows(comp))
  return(comp)
}

  # b1 <- paste(
  #   paste0("dep", wmf, " ~ dep", wml, " + indep", wml, " + ", sex, " + ", edad, collapse = "\n"),
  #   paste0("indep", wmf, " ~ indep", wml, collapse = "\n"),
  #   sep = "\n"
  # )

  # b2 <- paste(
  #   paste0("dep", wmf, " ~ a*dep", wml, " + b*indep", wml, " + sx*", sex, " + ed*", edad, collapse = "\n"),
  #   paste0("indep", wmf, " ~ d*indep", wml, collapse = "\n"),
  #   sep = "\n"
  # )

  # c1 <- paste(
  #   paste0("dep", wmf, " ~ dep", wml, collapse = "\n"),
  #   paste0("indep", wmf, " ~ dep", wml, " + indep", wml, " + ", sex, " + ", edad, collapse = "\n"),
  #   sep = "\n"
  # )

  # c2 <- paste(
  #   paste0("dep", wmf, " ~ a*dep", wml, collapse = "\n"),
  #   paste0("indep", wmf, " ~ c*dep", wml, " + d*indep", wml, " + sx*", sex, " + ed*", edad, collapse = "\n"),
  #   sep = "\n"
  # )

  # d1 <- paste(
  #   paste0("dep", wmf, " ~ dep", wml, " + indep", wml, " + ", sex, " + ", edad, collapse = "\n"),
  #   paste0("indep", wmf, " ~ dep", wml, " + indep", wml, " + ", sex, " + ", edad, collapse = "\n"),
  #   sep = "\n"
  # )

  # d2 <- paste(
  #   paste0("dep", wmf, " ~ a*dep", wml, " + b*indep", wml, " + sx*", sex, " + ed*", edad, collapse = "\n"),
  #   paste0("indep", wmf, " ~ c*dep", wml, " + d*indep", wml, " + sx*", sex, " + ed*", edad, collapse = "\n"),
  #   sep = "\n"
  # )
