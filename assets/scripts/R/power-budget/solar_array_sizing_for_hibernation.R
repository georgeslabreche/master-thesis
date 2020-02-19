# Warning: By Solar Area we actually mean Solar Cell Coverage Area. TODO: fix this.

library(mars)
library(here)

Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

source(here("utils", "insolation_utils.R"))
source(here("utils", "power_utils.R"))
source(here("utils", "utils.R"))



get_worst_case_solar_array_area = function(location_id, Ls_seq, phi, longitude, beta_max, tau){
  
  # Get worst case insolations for inclined and horizontal surfaces.
  H_wc = get_worst_case_daily_insolations(location_id=location_id, Ls_seq=Ls_seq, phi=phi, longitude=longitude, beta_max=beta_max, tau=tau)
  
  # Get solar array area with inclined surface.
  power_budget = get_power_budget_for_hibernation(Ls=H_wc$inclined$Ls, phi=phi)
  area_inclined = get_solar_array_area(H_avail=H_wc$inclined$H, power_budget=power_budget)
  
  # Get solar array area with horizontal surface.
  power_budget = get_power_budget_for_hibernation(Ls=H_wc$horizontal$Ls, phi=phi)
  area_horizontal = get_solar_array_area(H_avail=H_wc$horizontal$H, power_budget=power_budget)
  
  # Get solar array area
  area = list(
    "inclined" = area_inclined,
    "horizontal" = area_horizontal,
    "diff" =  area_horizontal - area_inclined,
    "gain" = round(((area_horizontal - area_inclined) / area_horizontal) * 100, 2)
  )
  
  # Return result.
  return(area)
}

set_hibernation_mode_power_draw = function(P){
  mode_powers$HibernationDay <<- P
  mode_powers$HibernationNight <<- P
}



#############
# VARIABLES #
#############
tau = 1
# TODO: Try with 1.5. For Iani Chaos, 1 gives us 1.8 m2 (vs 1.87 m2 for horiz)
# Check if optimal is not used for horiz?
# Define another location at around 15 degree North?

SA_surface_density = 3.7 # [kg/m2]


##############
# IANI CHAOS #
##############
print("IANI CHAOS")

location_id = "IaniChaos"
roi = ROIs$IaniChaos

# Overwrite hibernation power draw from 18 W to 17 W.
set_hibernation_mode_power_draw(17)

# Get solar array area - worst case.
SA_area_WorstCase = get_worst_case_solar_array_area(location_id=location_id,
                                                    Ls_seq=Ls_seq, phi=roi$phi, longitude=roi$longitude,
                                                    beta_max=roi$beta, tau=tau)

print_solar_area_sizing_results(SA_area=SA_area_WorstCase, SA_surface_density=SA_surface_density)


##################
# ISMENIUS CAVUS #
##################
print("ISMENIUS CAVUS")

location_id = "IsmeniusCavus"
roi = ROIs$IsmeniusCavus

# Overwrite hibernation power draw from 18 W to 15 W.
set_hibernation_mode_power_draw(15)

# Get solar array area - worst case.
SA_area_WorstCase = get_worst_case_solar_array_area(location_id=location_id,
                                                    Ls_seq=Ls_seq, phi=roi$phi, longitude=roi$longitude,
                                                    beta_max=roi$beta, tau=tau)

print_solar_area_sizing_results(SA_area=SA_area_WorstCase, SA_surface_density=SA_surface_density)
