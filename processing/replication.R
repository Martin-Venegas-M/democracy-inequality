# 0. Identification -------------------------------------------------------

# Title: Replication code for a research paper on Democracy and Inequality
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher

# Executive Summary: This script contains the code to create the replicati9on code for Democracy and Inequality
# Date: April 23, 2025

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

# 3. Gold standards -------------------------------------------------------

# Creating text object
text_gold_standard <- text_riclpm("d02_03", "c08_02", c(1:4, 6:7))

# Estimating model
fit_gold_standard <- estimate_riclpm(text_gold_standard)
# summary(fit_gold_standard)

# Create table
tab_gold_standard <- reg_sig(fit_gold_standard,"d02_03", "c08_02") %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  )

# 4. Replication ----------------------------------------------------------

# writeLines(text_gold_standard)

# Creating text object
text_replication <- "

RI_x =~ 1*d02_03_w01 + 1*d02_03_w02 + 1*d02_03_w03 + 1*d02_03_w04 + 1*d02_03_w06 + 1*d02_03_w07

RI_y =~ 1*c08_02_w01 + 1*c08_02_w02 + 1*c08_02_w03 + 1*c08_02_w04 + 1*c08_02_w06 + 1*c08_02_w07

cx1 =~ 1*d02_03_w01
cx2 =~ 1*d02_03_w02
cx3 =~ 1*d02_03_w03
cx4 =~ 1*d02_03_w04
cx6 =~ 1*d02_03_w06
cx7 =~ 1*d02_03_w07

cy1 =~ 1*c08_02_w01
cy2 =~ 1*c08_02_w02
cy3 =~ 1*c08_02_w03
cy4 =~ 1*c08_02_w04
cy6 =~ 1*c08_02_w06
cy7 =~ 1*c08_02_w07

d02_03_w01 ~~ 0*d02_03_w01
d02_03_w02 ~~ 0*d02_03_w02
d02_03_w03 ~~ 0*d02_03_w03
d02_03_w04 ~~ 0*d02_03_w04
d02_03_w06 ~~ 0*d02_03_w06
d02_03_w07 ~~ 0*d02_03_w07

c08_02_w01 ~~ 0*c08_02_w01
c08_02_w02 ~~ 0*c08_02_w02
c08_02_w03 ~~ 0*c08_02_w03
c08_02_w04 ~~ 0*c08_02_w04
c08_02_w06 ~~ 0*c08_02_w06
c08_02_w07 ~~ 0*c08_02_w07

cx2 ~ cx1 + cy1
cx3 ~ cx2 + cy2
cx4 ~ cx3 + cy3
cx6 ~ cx4 + cy4
cx7 ~ cx6 + cy6

cy2 ~ cx1 + cy1
cy3 ~ cx2 + cy2
cy4 ~ cx3 + cy3
cy6 ~ cx4 + cy4
cy7 ~ cx6 + cy6

cx1 ~~ cy1

cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx6 ~~ cy6
cx7 ~~ cy7

RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y

RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"
  
# Estimating model
fit_replication <- sem(
  text_replication,
  data = elsoc,
  estimator = "MLR",
  missing = "FIML",
  meanstructure = T,
  int.ov.free = T
)

# Create table
tab_replication <- reg_sig(fit_replication,"d02_03", "c08_02") %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  )

# 5. Test new function ---------------------------------------------------------

# Creating text object
text_new_function <- text_riclpm_v2("d02_03", "c08_02", c(1:4, 6:7))

# Estimating model
fit_new_function <- estimate_riclpm(text_new_function)

# Create table
tab_new_function <- reg_sig(fit_new_function,"d02_03", "c08_02") %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  )

# 6. Add (basic) controls ------------------------------------------------------

text_controls <- "

RI_x =~ 1*d02_03_w01 + 1*d02_03_w02 + 1*d02_03_w03 + 1*d02_03_w04 + 1*d02_03_w06 + 1*d02_03_w07

RI_y =~ 1*c08_02_w01 + 1*c08_02_w02 + 1*c08_02_w03 + 1*c08_02_w04 + 1*c08_02_w06 + 1*c08_02_w07

cx1 =~ 1*d02_03_w01
cx2 =~ 1*d02_03_w02
cx3 =~ 1*d02_03_w03
cx4 =~ 1*d02_03_w04
cx6 =~ 1*d02_03_w06
cx7 =~ 1*d02_03_w07

cy1 =~ 1*c08_02_w01
cy2 =~ 1*c08_02_w02
cy3 =~ 1*c08_02_w03
cy4 =~ 1*c08_02_w04
cy6 =~ 1*c08_02_w06
cy7 =~ 1*c08_02_w07

d02_03_w01 ~~ 0*d02_03_w01
d02_03_w02 ~~ 0*d02_03_w02
d02_03_w03 ~~ 0*d02_03_w03
d02_03_w04 ~~ 0*d02_03_w04
d02_03_w06 ~~ 0*d02_03_w06
d02_03_w07 ~~ 0*d02_03_w07

c08_02_w01 ~~ 0*c08_02_w01
c08_02_w02 ~~ 0*c08_02_w02
c08_02_w03 ~~ 0*c08_02_w03
c08_02_w04 ~~ 0*c08_02_w04
c08_02_w06 ~~ 0*c08_02_w06
c08_02_w07 ~~ 0*c08_02_w07

cx2 ~ cx1 + cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx3 ~ cx2 + cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx4 ~ cx3 + cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx6 ~ cx4 + cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx7 ~ cx6 + cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01

cy2 ~ cx1 + cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy3 ~ cx2 + cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy4 ~ cx3 + cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy6 ~ cx4 + cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy7 ~ cx6 + cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01

cx1 ~~ cy1

cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx6 ~~ cy6
cx7 ~~ cy7

RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y

RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"

# Estimating model
fit_controls <- estimate_riclpm(text_controls)

# Creating tab
tab_controls <- reg_sig(fit_controls,"d02_03", "c08_02") %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  )

# RESULTADO: CONTROLAR POR SEXO Y/O EDAD CONTROLAN EL EFECTO DE LA OLA 3 A LA 4
# El efecto se mantiene incluso controlando por educación en la ola 1

# 7. Add (basic) controls with new function --------------------------------------------------

# Creating text object
text_new_function_controls <- text_riclpm_v2("d02_03", "c08_02", c(1:4, 6:7), c("m0_edad_w01", "m0_sexo_w01", "m01_w01"))

# Estimating model
fit_new_function_controls <- estimate_riclpm(text_new_function_controls)

# Create table
tab_new_function_controls <- reg_sig(fit_new_function_controls,"d02_03", "c08_02") %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  )

# 8. Add (basic) controls and constrain ------------------------------------------------------

text_controls_constrained <- "

RI_x =~ 1*d02_03_w01 + 1*d02_03_w02 + 1*d02_03_w03 + 1*d02_03_w04 + 1*d02_03_w06 + 1*d02_03_w07

RI_y =~ 1*c08_02_w01 + 1*c08_02_w02 + 1*c08_02_w03 + 1*c08_02_w04 + 1*c08_02_w06 + 1*c08_02_w07

cx1 =~ 1*d02_03_w01
cx2 =~ 1*d02_03_w02
cx3 =~ 1*d02_03_w03
cx4 =~ 1*d02_03_w04
cx6 =~ 1*d02_03_w06
cx7 =~ 1*d02_03_w07

cy1 =~ 1*c08_02_w01
cy2 =~ 1*c08_02_w02
cy3 =~ 1*c08_02_w03
cy4 =~ 1*c08_02_w04
cy6 =~ 1*c08_02_w06
cy7 =~ 1*c08_02_w07

d02_03_w01 ~~ 0*d02_03_w01
d02_03_w02 ~~ 0*d02_03_w02
d02_03_w03 ~~ 0*d02_03_w03
d02_03_w04 ~~ 0*d02_03_w04
d02_03_w06 ~~ 0*d02_03_w06
d02_03_w07 ~~ 0*d02_03_w07

c08_02_w01 ~~ 0*c08_02_w01
c08_02_w02 ~~ 0*c08_02_w02
c08_02_w03 ~~ 0*c08_02_w03
c08_02_w04 ~~ 0*c08_02_w04
c08_02_w06 ~~ 0*c08_02_w06
c08_02_w07 ~~ 0*c08_02_w07

cx2 ~ a*cx1 + b*cy1 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx3 ~ a*cx2 + b*cy2 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx4 ~ a*cx3 + b*cy3 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx6 ~ a*cx4 + b*cy4 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx7 ~ a*cx6 + b*cy6 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01

cy2 ~ f*cx1 + g*cy1 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy3 ~ f*cx2 + g*cy2 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy4 ~ f*cx3 + g*cy3 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy6 ~ f*cx4 + g*cy4 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy7 ~ f*cx6 + g*cy6 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01

cx1 ~~ cy1

cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx6 ~~ cy6
cx7 ~~ cy7

RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y

RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"

# Estimating model
fit_controls_constrained <- estimate_riclpm(text_controls_constrained)

# Creating tab
tab_controls_constrained <- reg_sig(fit_controls_constrained,"d02_03", "c08_02") %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  )

# RESULTADO: NO HAY RESULTADOS CONSTRIÑENDO POR TIEMPO

# 9. Add (basic) controls and constrain with new function ------------------------------------

# Creating text object
text_new_function_controls_constrained <- text_riclpm_v2("d02_03", "c08_02", c(1:4, 6:7), c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = T)

# Estimating model
fit_new_function_controls_constrained <- estimate_riclpm(text_new_function_controls_constrained)

# Create table
tab_new_function_controls_constrained <- reg_sig(fit_new_function_controls_constrained,"d02_03", "c08_02") %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  )
