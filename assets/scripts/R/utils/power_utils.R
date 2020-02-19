# Propulsion Power for flat terrain [W]
Pprop_FlatTerrain = 75

# Propulsion Power for upslope terrain [W]
Pprop_UpslopeTerrain = 150


fixed_day_mode_durations = list(
  "DTECommunication" = 35,
  "ScienceStopShort" = 60,
  "OptimalPose" = 10
)


mode_powers = list(
  "IdleDay" = 29,
  "DTECommunication" = 52,
  "ScienceStopShort" = 60,
  "ScienceStopLong" = 52,
  "OptimalPose" = 75,
  "IdleNight" = 20,
  "HibernationDay" = 18,
  "HibernationNight" = 18
)

# Power of traverse mode subsystem minus propulsion.
# Propulsion is not constant because Pprop_FlatTerrain != Pprop_UpslopeTerrain
traverse_mode_subsystem_powers = list(
  "DHS" = 25,
  "PCDU" = 7,
  "GNC" = 3,
  "Comms" = 3
) 

# Solar cell performance ratio components.
PR_components = list(
  "RedShift" = 0.03,
  "Shadowing" = 0.05,
  "DustDeposition" = 0.3
)

# Solar cell performance ratio.
PR  = 1 - (sum(unlist(PR_components)))

# Solar cell efficiency at EOL.
e = 0.22

# Solar cell packing efficienćy.
packing_eff = 0.85


get_idle_mode_durations = function(Ls, phi, beta, gamma_c, t_fixed_day_modes, t_day_variable=0){
  # Number of daylight hours.
  # We basically want to know how much daylight we have during a traverse Sol
  # so that we can plan out the budget's time allocation for modes with variable durations,
  # i.e. Traverse mode, Idle Day mode, and Idle Night mode.
  Td = T_d(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c)
  
  # Convert daylight hours to minutes.
  Td_minutes = round(Td * 60, 2)
  
  # Get duration of idle day mode.
  IdleDay_duration = Td_minutes - t_fixed_day_modes- t_day_variable
  
  # Get duration of idle night mode based on idle day
  IdleNight_duration = (24 - Td) * 60
  
  result = list(
    "day" = IdleDay_duration,
    "night" = IdleNight_duration
  )
  
  return(result)
}

get_power_budget_for_hibernation = function(Ls, phi, terrain_beta=0, terrain_gamma_c=0){
  # Get duration of idle day and idle night.
  # This makes not difference for hibernation Sols because the same power draws occuring during day and night.
  # Nevertheless, we still distinguish betwen daytime length and nightime length in case this changes in the future.
  t_day = T_d(Ls=Ls, phi=phi, beta=terrain_beta, gamma_c=terrain_gamma_c) * 60
  t_night = 24*60 - t_day
  
  modes = c("Hibernation - Day", "Hibernation - Night")
  P_modes = c(mode_powers$HibernationDay, mode_powers$HibernationNight)
  t_modes = c(t_day, t_night)
  
  E_modes = P_modes * (t_modes / 60)
  
  df = data.frame(
    "Mode" = modes,
    "Power" = P_modes,
    "Duration" = t_modes,
    "Energy" = round(E_modes, 2)
  )
}

get_power_budget_for_idle_sol = function(Ls, phi, terrain_beta=0, terrain_gamma_c=0, optimal_pose=TRUE){
  # Total duration for fixed day modes during a traverse Sol.
  t_fixed_day_modes = NULL
  
  # Depends on whether or not optimal pose is included.
  # We wouldn't bother getting optimal pose during dusty days because of scattered light.
  # In other words, when tau factor is high enough it doesn't matter if the solar array surface is horizontal or inclined.
  if(isTRUE(optimal_pose)){
    t_fixed_day_modes = fixed_day_mode_durations$DTECommunication + fixed_day_mode_durations$OptimalPose
  }else{
    t_fixed_day_modes = fixed_day_mode_durations$DTECommunication
  }
  
  # Get duration of idle day and idle night.
  t_idle = get_idle_mode_durations(Ls=Ls, phi=phi, beta=terrain_beta, gamma_c=terrain_gamma_c, t_fixed_day_modes=t_fixed_day_modes)
  
  if(isTRUE(optimal_pose)){
    modes = c("Idle - Day", "DTE Communication", "Optimal Pose", "Idle - Night")
    P_modes = c(mode_powers$IdleDay, mode_powers$DTECommunication, mode_powers$OptimalPose, mode_powers$IdleNight)
    t_modes = c(t_idle$day, fixed_day_mode_durations$DTECommunication, fixed_day_mode_durations$OptimalPose, t_idle$night)
    
  }else{
    modes = c("Idle - Day", "DTE Communication", "Idle - Night")
    P_modes = c(mode_powers$IdleDay, mode_powers$DTECommunication, mode_powers$IdleNight)
    t_modes = c(t_idle$day, fixed_day_mode_durations$DTECommunication, t_idle$night)
  }
  
  E_modes = P_modes * (t_modes / 60)
  
  df = data.frame(
    "Mode" = modes,
    "Power" = P_modes,
    "Duration" = t_modes,
    "Energy" = round(E_modes, 2)
  )
}


get_power_budget_for_science_stop_long_sol = function(Ls, phi, terrain_beta=0, terrain_gamma_c=0, t_science_mode=NA, optimal_pose=TRUE){
  
  # Total duration for fixed day modes during a traverse Sol.
  t_fixed_day_modes = NULL
  
  # Depends on whether or not optimal pose is included.
  # We wouldn't bother getting optimal pose during dusty days because of scattered light.
  # In other words, when tau factor is high enough it doesn't matter if the solar array surface is horizontal or inclined.
  if(isTRUE(optimal_pose)){
    t_fixed_day_modes = fixed_day_mode_durations$DTECommunication + fixed_day_mode_durations$OptimalPose
  }else{
    t_fixed_day_modes = fixed_day_mode_durations$DTECommunication
  }
  
  # Get duration of idle day and idle night.  
  t_idle = get_idle_mode_durations(Ls=Ls, phi=phi, beta=terrain_beta, gamma_c=terrain_gamma_c, t_fixed_day_modes=t_fixed_day_modes, t_day_variable=t_science_mode)
  
  if(isTRUE(optimal_pose)){
    modes = c("Idle - Day", "DTE Communication", "Science Stop - Long", "Optimal Pose", "Idle - Night")
    P_modes = c(mode_powers$IdleDay, mode_powers$DTECommunication, mode_powers$ScienceStopShort, mode_powers$OptimalPose, mode_powers$IdleNight)
    t_modes = c(t_idle$day, fixed_day_mode_durations$DTECommunication, t_science_mode, fixed_day_mode_durations$OptimalPose, t_idle$night)
    
  }else{
    modes = c("Idle - Day", "DTE Communication", "Science Stop - Long", "Idle - Night")
    P_modes = c(mode_powers$IdleDay, mode_powers$DTECommunication, mode_powers$ScienceStopShort, mode_powers$IdleNight)
    t_modes = c(t_idle$day, fixed_day_mode_durations$DTECommunication, t_science_mode, t_idle$night)
  }
  
  E_modes = P_modes * (t_modes / 60)
  
  df = data.frame(
    "Mode" = modes,
    "Power" = P_modes,
    "Duration" = t_modes,
    "Energy" = round(E_modes, 2)
  )
}


# TODO: rename to get_traverse_sol_power_budget
# TODO: rename t_traverse to t_traverse_mode
#' get_traverse_power_budget
#'
#' @param Ls areocentric longitude.
#' @param phi planetary latitude.
#' @param beta terrain surface beta. 
#' @param gamma_c terrain surface gamma_c.
#' @param propulsion_power  power draw from propulsion [W].
#' @param t_traverse duration of traverse [min].
#'
#' @return
get_power_budget_for_traverse_sol = function(Ls, phi, beta=0, gamma_c=0, propulsion_power, t_traverse=NA, optimal_pose=TRUE){
  
  # Total duration for fixed day modes during a traverse Sol.
  t_fixed_day_modes = fixed_day_mode_durations$DTECommunication + fixed_day_mode_durations$ScienceStopShort + fixed_day_mode_durations$OptimalPose

  # Get duration of idle day and idle night.  
  t_idle = get_idle_mode_durations(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, t_fixed_day_modes=t_fixed_day_modes, t_day_variable=t_traverse)
  
  # Traverse mode power [W].
  P_taverse_mode = propulsion_power + sum(unlist(traverse_mode_subsystem_powers))
  
  if(isTRUE(optimal_pose)){
    modes = c("Idle - Day", "DTE Communication", "Traverse", "Science Stop - Short", "Optimal Pose", "Idle - Night")
    P_modes = c(mode_powers$IdleDay, mode_powers$DTECommunication, P_taverse_mode, mode_powers$ScienceStopShort, mode_powers$OptimalPose, mode_powers$IdleNight)
    t_modes = c(t_idle$day, fixed_day_mode_durations$DTECommunication, t_traverse, fixed_day_mode_durations$ScienceStopShort, fixed_day_mode_durations$OptimalPose, t_idle$night)
  
  }else{
    modes = c("Idle - Day", "DTE Communication", "Traverse", "Science Stop - Short", "Idle - Night")
    P_modes = c(mode_powers$IdleDay, mode_powers$DTECommunication, P_taverse_mode, mode_powers$ScienceStopShort, mode_powers$IdleNight)
    t_modes = c(t_idle$day, fixed_day_mode_durations$DTECommunication, t_traverse, fixed_day_mode_durations$ScienceStopShort, t_idle$night)
  }
  

  E_modes = P_modes * (t_modes / 60)
  
  df = data.frame(
    "Mode" = modes,
    "Power" = P_modes,
    "Duration" = t_modes,
    "Energy" = round(E_modes, 2)
  )
}


get_power_budget_for_flat_traverse_sol = function(Ls, phi, t_traverse=NA, P_traverse=Pprop_FlatTerrain, optimal_pose){
  df = get_power_budget_for_traverse_sol(Ls=Ls, phi=phi, propulsion_power=P_traverse, t_traverse=t_traverse, optimal_pose)
  return(df)
}

get_power_budget_for_upslope_traverse_sol = function(Ls, phi, beta, gamma_c, t_traverse=NA, P_traverse=Pprop_UpslopeTerrain, optimal_pose){
  df = get_power_budget_for_traverse_sol(Ls=Ls, phi=phi, beta, gamma_c, propulsion_power=P_traverse, t_traverse=t_traverse, optimal_pose)
  return(df)
}


# [Wh]
get_required_energy = function(energies, system_margin=0.20){
  E_tot = sum(energies)
  E_tot_with_margin = E_tot * (1 + system_margin)
  
  return(E_tot_with_margin)
}

# [m2]
get_solar_cell_coverage_area = function(E_required, H_available){
  area = E_required / (PR * e * H_available)
  return(round(area, 2))
}

# [m2]
get_solar_array_area_from_cell_area = function(solar_cell_coverage_area, solar_cell_packing_efficiency=packing_eff){
  area = solar_cell_coverage_area / packing_eff
  return(round(area, 2))
}


get_solar_array_area = function(H_avail, power_budget){
  
  # Get total energy required.
  E_required = get_required_energy(energies=power_budget$Energy)
  
  # Get solar cell coverage area.
  solar_cell_area = get_solar_cell_coverage_area(E_required=E_required, H_available=H_avail)
  
  # Get Solar array area.
  solar_array_area = get_solar_array_area_from_cell_area(solar_cell_area)
}

print_solar_area_sizing_results = function(SA_area, SA_surface_density){
  
  SA_mass_inclined = SA_area$inclined * SA_surface_density
  SA_mass_horizontal = SA_area$horizontal * SA_surface_density
  SA_mass_diff = round(SA_area$diff * SA_surface_density, 2)
  SA_mass_gain = round(((SA_mass_horizontal - SA_mass_inclined) / SA_mass_horizontal) * 100, 2)
  
  print("====================================")
  
  print(paste("Solar array area required when tau = ", tau, " is ", SA_area$inclined, " m² with INCLINED surface.", sep=""))
  print(paste("Solar array area required when tau = ", tau, " is ", SA_area$horizontal, " m² with a HORIZONTAL surface.", sep=""))
  print(paste("Using an inclined surface configuration reduces the solar array area by ", SA_area$diff, " m². This is a ", SA_area$gain, " % decrease.", sep=""))
  
  print("")
  
  print(paste("Inclined solar array mass is ", SA_mass_inclined, " kg.", sep=""))
  print(paste("Horizontal solar array mass is ", SA_mass_horizontal, " kg.", sep=""))
  print(paste("Using an inclined surface configuration reduces the solar array mass by ", SA_mass_diff, " kg. This is a ", SA_mass_gain, " % decrease.", sep=""))
  
  print("")
}