library(mars)
library(here)

Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

source(here("utils", "insolation_utils.R"))
source(here("utils", "power_utils.R"))
source(here("utils", "utils.R"))


get_worst_case_solar_array_area = function(Ls_seq, phi, longitude, beta_max, tau, t_traverse, P_traverse=Pprop_FlatTerrain, location_id){

  # Get worst case insolations for inclined and horizontal surfaces.
  H_wc = get_worst_case_daily_insolations(location_id=location_id, Ls_seq=Ls_seq, phi=phi, longitude=longitude, beta_max=beta_max, tau=tau)
  
  # Get solar array area for inclined surface.
  power_budget = get_power_budget_for_flat_traverse_sol(Ls=H_wc$inclined$Ls, phi=phi, t_traverse=t_traverse, P_traverse=P_traverse, optimal_pose=TRUE)
  area_inclined = get_solar_array_area(H_avail=H_wc$inclined$H, power_budget=power_budget)
  
  # Get solar array area for horizontal surface.
  power_budget = get_power_budget_for_flat_traverse_sol(Ls=H_wc$horizontal$Ls, phi=phi, t_traverse=t_traverse, P_traverse=P_traverse, optimal_pose=FALSE)
  area_horizontal = get_solar_array_area(H_avail=H_wc$horizontal$H, power_budget=power_budget)
  
  # Get solar array area
  result = list(
    "inclined" = area_inclined,
    "horizontal" = area_horizontal,
    "diff" =  area_horizontal - area_inclined,
    "gain" = round(((area_horizontal - area_inclined) / area_horizontal) * 100, 2)
  )

  # Return result.
  return(result)
}


####################
# COMMON VARIABLES #
####################
tau = 1
Ls_seq = 1:360
SA_surface_density = 3.7 # [kg/m2]
P_traverse = Pprop_FlatTerrain

##############
# IANI CHAOS #
##############
print("IANI CHAOS")

location_id = "IaniChaos"
roi = ROIs$IaniChaos
t_traverse_WorstCase = get_traverse_time(d_traverse=5)


# Get solar array area - worst case.
SA_area_WorstCase = get_worst_case_solar_array_area(Ls_seq=Ls_seq, phi=roi$phi, longitude=roi$longitude,
                                                    beta_max=roi$beta, tau=tau, t_traverse=t_traverse_WorstCase,
                                                    location_id=location_id, P_traverse=P_traverse)

print_solar_area_sizing_results(SA_area=SA_area_WorstCase, SA_surface_density=SA_surface_density)


##################
# ISMENIUS CAVUS #
##################
print("ISMENIUS CAVUS")

location_id = "IsmeniusCavus"
roi = ROIs$IsmeniusCavus
t_traverse_WorstCase = get_traverse_time(d_traverse=5)

# Get solar array area - worst case.
SA_area_WorstCase = get_worst_case_solar_array_area(Ls_seq=Ls_seq, phi=roi$phi, longitude=roi$longitude,
                                                    beta_max=roi$beta, tau=tau, t_traverse=t_traverse_WorstCase,
                                                    location_id=location_id, P_traverse=P_traverse)


print_solar_area_sizing_results(SA_area=SA_area_WorstCase, SA_surface_density=SA_surface_density)


################
# OBSERVATIONS #
################
# 1. Inclined surfaces do not affect solar array sizing so much.
#    This is because their performance nears that of horizontal 
#    SA surfaces during diffuse solar radiatons for high tau factors.

