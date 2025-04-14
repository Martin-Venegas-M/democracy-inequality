# Function for creating text object for lavaan ----------------------------------------------
text_riclpm <- function(x, y, waves) {
  # Create between components
  RI_x <- paste0("RI_x =~ ", paste0("1*", x, "_w", sprintf("%02d", waves), collapse = " + "))
  RI_y <- paste0("RI_y =~ ", paste0("1*", y, "_w", sprintf("%02d", waves), collapse = " + "))

  # Create within components
  cx <- paste0("cx", waves, " =~ 1*", x, "_w", sprintf("%02d", waves), collapse = "\n")
  cy <- paste0("cy", waves, " =~ 1*", y, "_w", sprintf("%02d", waves), collapse = "\n")

  # Constrain measurement error variances to cero
  var_x <- paste0(x, "_w", sprintf("%02d", waves), " ~~ 0*", x, "_w", sprintf("%02d", waves), collapse = "\n")
  var_y <- paste0(y, "_w", sprintf("%02d", waves), " ~~ 0*", y, "_w", sprintf("%02d", waves), collapse = "\n")

  # Estimate lagged efects only for consecutive waves in the vector
  lag_cx <- paste0("cx", waves[-1], " ~ cx", waves[-length(waves)], " + cy", waves[-length(waves)], collapse = "\n")
  lag_cy <- paste0("cy", waves[-1], " ~ cx", waves[-length(waves)], " + cy", waves[-length(waves)], collapse = "\n")

  # Estimate covariance bewteen within components t = 1
  cov_t1 <- paste0("cx", waves[1], " ~~ cy", waves[1])

  # Estimate covariances between within residuals of within components
  cov_resid <- paste0("cx", waves[-1], " ~~ cy", waves[-1], collapse = "\n")

  # Estimate variance and covariance between RI
  cov_RI <- "RI_x ~~ RI_x\nRI_y ~~ RI_y\nRI_x ~~ RI_y"

  # Fix correlation bewteen RI and within components t = 1 to cero
  fix_RI <- paste0(
    "RI_x ~~ 0*cx", waves[1], "\nRI_x ~~ 0*cy", waves[1],
    "\nRI_y ~~ 0*cx", waves[1], "\nRI_y ~~ 0*cy", waves[1]
  )

  # Combine model
  modelo <- paste(RI_x, RI_y, cx, cy, var_x, var_y, lag_cx, lag_cy, cov_t1, cov_resid, cov_RI, fix_RI, sep = "\n\n")

  return(modelo)
}

# Example
# test_riclpm <- text_riclpm("brecha_perc", "f05_03", c(1:7))
# cat(modelo_riclpm)

# test <- sem(
#   test_riclpm,
#   data = elsoc,
#   estimator = "MLR",
#   missing = "FIML",
#   meanstructure = T,
#   int.ov.free = T
# )

# summary(test)

# Function for creating a table with significative regression effects -----------------------------------
reg_sig <- function(fit, x, y) {
  parameters <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~", pvalue < 0.05) %>%
    select(lhs, op, rhs, est, se, z, pvalue, std.all) %>%
    arrange(pvalue)

  fitmeasures <- fitMeasures(fit)

  parameters <- parameters %>% mutate(
    CFI = fitmeasures[["cfi"]],
    RMSEA = fitmeasures[["rmsea"]],
    varx := x, 
    vary := y 
  ) %>% relocate(vary, varx, everything())

  return(parameters)
}

# Example:
# tab_sig <- reg_sig(fit_riclpm_f05_03)
# print(tab_sig)

# Function for estimating RICLPM Model. It assumes data as "elsoc". ------------------------------------
estimate_riclpm <- function(text_object) {
  sem(
    text_object,
    data = elsoc,
    estimator = "MLR",
    missing = "FIML",
    meanstructure = T,
    int.ov.free = T
  )
}
