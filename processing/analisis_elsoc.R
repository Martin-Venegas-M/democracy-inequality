
# 0. Identification -------------------------------------------------------

#Title: Analysis code for a research paper on Democracy and Inequality
#Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
#Responsable: Researcher

# Executive Summary: This script contains the code to create the analysis code for Democracy and Inequality
# Date: April 1, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  lavaan,
  writexl
)

# 2. Load data and functions ----------------------------------------------

elsoc <- readRDS("input/data/proc_elsoc.RDS")
source("processing/func_sint.R")

# 3. Estimate RICLPM ------------------------------------------------------

# Function for estimating multiple dependent variables for one dependent variable
list_fits <- function(x, waves = c(1:7)) {
  fits <- list(
    fit1 = estimate_riclpm(text_riclpm(x, "c08_01", waves)),
    fit2 = estimate_riclpm(text_riclpm(x, "c08_02", waves)),
    fit3 = estimate_riclpm(text_riclpm(x, "c08_03", waves)),
    fit4 = estimate_riclpm(text_riclpm(x, "c08_04", waves))
  )

  return(fits)
}

fits_c18_11 <- list_fits("c18_11", waves = c(1:4, 6:7))

# 4. Create tabs for RICLPM -----------------------------------------------

vector_vary <- c(paste0("c08_0", rep(1:4)))

tab_c18_11 <- bind_rows(map2(.x = fits_c18_11, .y = vector_vary, .f = ~ reg_sig(.x, "c18_11", .y)))

# 5. Excel with estimations ----------------------------------------------

write_xlsx(list(
  "ACEPTACION DESIGUALDAD" = tab_c18_11
), "output/riclpm_democracy_inequality.xlsx")
