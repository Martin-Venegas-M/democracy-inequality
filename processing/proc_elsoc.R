
# 0. Identification -------------------------------------------------------

#Title: Processing code for a research paper on democracy and subjetive inequality
#Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
#Responsable: Researcher

# Executive Summary: This script contains the code to create the database needed to elaborate the analyses on democracy and subjetive inequality
# Date: April 13, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman")  #if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog
  )

# 2. Load data ------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797986"))

# 3. Select variables -----------------------------------------------------

elsoc <- elsoc_wide_2016_2023 %>% dplyr::select(
  idencuesta,
  tipo_atricion,
  tipo_caso,
  starts_with("m0_edad"),
  starts_with("m0_sexo"),
  starts_with("m01_"),
  contains("c18_11"),
  contains("c18_12"),
  contains("c02"),
  contains("c08"),
  contains("c09"),
  contains("d02"),
  #contains("r13"),
  contains("r15")
  ) %>% mutate(
    across(everything(), ~ if_else(. %in% c(-999, -888, -777, -666, -Inf), NA, .))
  )

# 4. Save data ------------------------------------------------------------

saveRDS(elsoc, "input/data/proc_elsoc.RDS")
