# 0. Identification -------------------------------------------------------

# Title: Analysis code for a research paper on Democracy and Inequality
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher

# Executive Summary: This script contains the code to create the analysis code for Democracy and Inequality
# Date: April 21, 2025

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

get_waves <- function(x, y, data = elsoc) {
  indep <- str_sub(names(data)[str_detect(names(data), x)], 10) %>% as.numeric()
  dep <- str_sub(names(data)[str_detect(names(data), y)], 10) %>% as.numeric()

  sort(intersect(indep, dep))
}

# Function for estimating multiple dependent variables for one independent variable

v_c08 <- c(paste0("c08_0", rep(1:4)))
v_c09 <- c(paste0("c09_0", rep(1:4)))

list_fits <- function(x, dependents = NULL, data = elsoc) {
  if (is.null(dependents)) {
    message("INCORPORE ALGUNA VARIABLE DEPENDIENTE!")
  } else {
    purrr::map(
      dependents,
      ~ estimate_riclpm(text_riclpm_v2(x, .x, get_waves(x, .x, data), inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01")))
    ) %>%
      set_names(paste0("fit", seq_along(dependents)))
  }
}

fits_c18_11 <- list_fits("c18_11", c(v_c08, v_c09))
# fits_c18_12 <- list_fits("c18_12", v_c08)
fits_d02_01 <- list_fits("d02_01", c(v_c08, v_c09))
fits_d02_02 <- list_fits("d02_02", c(v_c08, v_c09))
fits_d02_03 <- list_fits("d02_03", c(v_c08, v_c09))

# 4. Create tabs for RICLPM -----------------------------------------------

vector_vary <- c(v_c08, v_c09)

tab_c18_11 <- bind_rows(map2(.x = fits_c18_11, .y = vector_vary, .f = ~ reg_sig(.x, "c18_11", .y)))
# tab_c18_12 <- bind_rows(map2(.x = fits_c18_12, .y = v_c08, .f = ~ reg_sig(.x, "c18_12", .y)))
tab_d02_01 <- bind_rows(map2(.x = fits_d02_01, .y = vector_vary, .f = ~ reg_sig(.x, "d02_01", .y)))
tab_d02_02 <- bind_rows(map2(.x = fits_d02_02, .y = vector_vary, .f = ~ reg_sig(.x, "d02_02", .y)))
tab_d02_03 <- bind_rows(map2(.x = fits_d02_03, .y = vector_vary, .f = ~ reg_sig(.x, "d02_03", .y)))

# 5. Excel with estimations ----------------------------------------------

# relevant <- function(x) {
#   x %>% filter(
#     (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
#     (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
#   )

# }

write_xlsx(list(
  "ACEPTACION DESIGUALDAD" = tab_c18_11,
  #"HAY GRUPOS INFERIORES" = tab_c18_12,
  "JUST DIST PENSIONES" = tab_d02_01,
  "JUST DIST EDUC" = tab_d02_02,
  "JUST DIST SALUD" = tab_d02_03
), "output/riclpm_democracy_inequality_controls.xlsx")

# Check

get_waves("d02_03", "c08_02")
