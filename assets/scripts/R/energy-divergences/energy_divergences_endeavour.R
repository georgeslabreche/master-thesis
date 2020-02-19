library(here)

# Disable f function warnings.
Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
source(here("energy-divergences", "get_energy_divergences.R"))

# Selected Sols and the traverse direction:
#   Sol 4493: Due North-East. Orientation: +110°. 
#   Sol 4568: Due South-East. Orientation: +15°.
#   Sol 4582: Due South-West. Orientation: -30°.
#   Sol 4623: Due West. Orientation: -80°.
#   Sol 4630: Due South-West. Orientation: -20°.
sols = c(4493, 4568, 4582, 4623, 4630)
gammas = c(110, 15, -28, -80, -20)

#sols = c(4493, 4582, 4623, 4630)
#gammas = c(120, -30, -80, -20)

#gammas = c(120, -35, -40, -20)

sols = c(4493, 4582, 4623, 4630)
gammas = c(110, -28, -80, -20)

for(i in 1:length(sols)){
  energy_divergences = get_energy_divergences(
    Loss_shadowing = 0.05,
    sol_filter = sols[i], # Calculate energy divergences for sols of interest.
    beta = 13, # Assume 15 deg slope angle.
    gamma_c = gammas[i] # Apply orientations as observed on MER Opportunity traverse archive.
  )
  print(energy_divergences)
}

cat("\n\n")

for(i in 1:length(sols)){
  energy_divergences = get_energy_divergences(
    sol_filter = sols[i]
  )
  print(energy_divergences)
}