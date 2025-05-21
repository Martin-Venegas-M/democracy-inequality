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

# (UPGRADE) Function for creating text object for lavaan ------------------------------------
text_riclpm_v2 <- function(var_x, var_y, waves, inv_preds = NULL, constrain = FALSE) {
    # Transform waves to character and format "01"
    waves_chr <- sprintf("%02d", waves)

    # Create vector with variable names
    vx <- paste0(var_x, "_w", waves_chr) #? Here paste0() create the string by every element of the vector "waves_chr"
    vy <- paste0(var_y, "_w", waves_chr) 

    # Create string with between effects
    ri_x <- paste("RI_x =~", paste0("1*", vx, collapse = " + ")) #? paste0() pastes "*1" to every element in the vector "waves_chr, separating by " + "...
    ri_y <- paste("RI_y =~", paste0("1*", vy, collapse = " + ")) #? then, it pastes the corresponding RI at the beginning with paste()

    # Create vector with within effects
    cx <- paste0("cx", waves, " =~ 1*", vx)
    cy <- paste0("cy", waves, " =~ 1*", vy)

    # Constrain measurement error variances to zero
    vx0 <- paste0(vx, " ~~ 0*", vx)
    vy0 <- paste0(vy, " ~~ 0*", vy)

    # Relaciones autoregresivas
    waves_lag <- waves[-1] #? Remove firts wave 
    waves_ant <- waves[-length(waves)] #? Return wave - 1 of the "waves_lag" vector

    # Create regression
    if (constrain) { # ? If constrianing is requiered...
        coef_cx <- paste0(
            "cx", waves_lag, " ~ ",
            letters[1], "*cx", waves_ant, " + ",
            letters[2], "*cy", waves_ant
        )
        coef_cy <- paste0(
            "cy", waves_lag, " ~ ",
            letters[6], "*cx", waves_ant, " + ",
            letters[7], "*cy", waves_ant
        )

        if (!is.null(inv_preds)) {
            for (i in seq_along(inv_preds)) {
                coef_cx <- paste0(coef_cx, " + ", letters[2 + i], "*", inv_preds[i])
                coef_cy <- paste0(coef_cy, " + ", letters[7 + i], "*", inv_preds[i])
            }
        }
    } else { # ? At every other case...
        coef_cx <- paste0(
            "cx", waves_lag, " ~ ",
            "cx", waves_ant, " + ",
            "cy", waves_ant
        )
        coef_cy <- paste0(
            "cy", waves_lag, " ~ ",
            "cx", waves_ant, " + ",
            "cy", waves_ant
        )

        if (!is.null(inv_preds)) {
            inv_line <- paste(inv_preds, collapse = " + ")
            coef_cx <- paste0(coef_cx, " + ", inv_line)
            coef_cy <- paste0(coef_cy, " + ", inv_line)
        }
    }

    # Covariances between residuals
    covs <- paste0("cx", waves, " ~~ cy", waves)

    # Covariances bewteen RI y RI
    first_wave <- waves[1]

    ri_covs <- c(
        "RI_x ~~ RI_x", 
        "RI_y ~~ RI_y", 
        "RI_x ~~ RI_y",
        paste0("RI_x ~~ 0*cx", first_wave), 
        paste0("RI_x ~~ 0*cy", first_wave),
        paste0("RI_y ~~ 0*cx", first_wave), 
        paste0("RI_y ~~ 0*cy", first_wave)
    )

    # Resultado
    model_lines <- c(
        ri_x, ri_y,
        cx, cy,
        vx0, vy0,
        coef_cx, coef_cy,
        covs,
        ri_covs
    )

    return(paste(model_lines, collapse = "\n"))
}

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
