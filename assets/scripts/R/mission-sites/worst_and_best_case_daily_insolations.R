library(mars)
library(formattable)
library(here)

Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

source(here("utils", "insolation_utils.R"))


day_types = list(
  "clear_to_dusty" = list(
      "Ls_seq" = 1:360,
      "taus" = c(0.1, 0.4, 0.5, 1.0, 1.5)),
    "global_storm" = list(
      "Ls_seq" = 180:360, # We only expect large taus during global dust storm season.
      "taus" = c(2, 2.5, 3, 3.5, 4, 4.5, 5))
  )
  
  
############## 
# IANI CHAOS #
##############
roi = ROIs$IaniChaos

# Get data for clear to dusty days.
day_type = day_types$clear_to_dusty
df = get_worst_and_best_case_insolations(Ls_seq=day_type$Ls_seq,
                                         phi=roi$phi,longitude=roi$longitude,
                                         beta=roi$beta, taus=day_type$taus)
print(formattable(df))

# # Get data for global storm days.
# day_type = day_types$global_storm
# df = get_worst_and_best_case_insolations(Ls_seq=day_type$Ls_seq,
#                                          phi=roi$phi, longitude=roi$longitude,
#                                          beta=roi$beta, taus=day_type$taus)
# print(formattable(df))

##################
# ISMENIUS CAVUS #
# ##################
# roi = ROIs$IsmeniusCavus
# 
# # Get data for clear to dusty days.
# day_type = day_types$clear_to_dusty
# df = get_worst_and_best_case_insolations(Ls_seq=day_type$Ls_seq, 
#                                          hi=roi$phi, longitude=roi$longitude,
#                                          beta=roi$beta, taus=day_type$taus)
# print(formattable(df))
# 
# # Get data for global storm days.
# day_type = day_types$global_storm
# df = get_worst_and_best_case_insolations(Ls_seq=day_type$Ls_seq
#                                          phi=roi$phi, longitude=roi$longitude,
#                                          beta=roi$beta, taus=day_type$taus)
# print(formattable(df))
# 
