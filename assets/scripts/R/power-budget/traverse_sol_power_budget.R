library(here)

source(here("utils", "insolation_utils.R"))
source(here("utils", "power_utils.R"))
source(here("utils", "utils.R"))

get_flat_traverse_power_budget_worst_case = function(location_id, phi, tau, beta_best, t_traverse, P_traverse=Pprop_FlatTerrain){
  # Load daily insolation lookup tables.
  H_df = get_daily_insolation_lookup_table(location_id=location_id, tau=tau, beta_max=beta_best)
  
  Ls_worst = H_df$Ls[H_df$H_best == min(H_df$H_best)]
  gamma_c_best =H_df$gamma_c_best[H_df$H_best == min(H_df$H_best)]
  
  # Traverse Sol power and energy requirements.
  df_energy_worst_case = get_power_budget_for_flat_traverse_sol(Ls=Ls_worst, phi=phi, t_traverse=t_traverse, P_traverse=P_traverse)
  
  return(df_energy_worst_case)
}

# 20% System margin.
system_margin = 0.2
tau = 1
t_traverse = get_traverse_time(d_traverse=5)
P_traverse = 75

##############
# Iani Chaos #
##############

# Params.
location_id = "IaniChaos"
phi = -2
beta_best = -10

# Get Traverse Sol power budget.
df_energy_worst_case = get_flat_traverse_power_budget_worst_case(location_id=location_id, phi=phi, tau=tau, beta_best=beta_best,
                                                                 t_traverse=t_traverse, P_traverse=P_traverse)

# Display result.
print(df_energy_worst_case)

# Total energy required.
E_tot = sum(df_energy_worst_case$Energy)
E_tot_with_marging = E_tot * (1 + system_margin)

# Display result
print("Total energy required to traverse minimum required distance on worst case Sol is:")
print(paste(E_tot, " Wh WITHOUT system margin.", sep=""))
print(paste(E_tot_with_marging, " Wh WITH system margin.", sep=""))


##################
# Ismenius Cavus #
##################
print("")

# Params.
location_id = "IsmeniusCavus"
phi = 34
beta_best = 10

# Get Traverse Sol power budget.
df_energy_worst_case = get_flat_traverse_power_budget_worst_case(location_id=location_id, phi=phi, tau=tau, beta_best=beta_best,
                                                                 t_traverse=t_traverse, P_traverse=P_traverse)

# Display result.
print(df_energy_worst_case)

# Total energy required.
E_tot = sum(df_energy_worst_case$Energy)
E_tot_with_marging = E_tot * (1 + system_margin)

# Display result

print("Total energy required to traverse minimum required distance on worst case Sol is:")
print(paste(E_tot, " Wh WITHOUT system margin.", sep=""))
print(paste(E_tot_with_marging, " Wh WITH system margin.", sep=""))
