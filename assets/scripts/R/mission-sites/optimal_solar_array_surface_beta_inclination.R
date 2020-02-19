# Calculate optimal surface inclination abgle beta with two different approaches:
# 1 - Based on direct beam irradiance at noon. 
# 2 - Based on solar insolation.

library(here)
library(mars)
library(RColorBrewer)

Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "appendix_beta_optimal"

# Plot colors.
cols = brewer.pal(n=11, name="RdYlBu")
colors = cols[c(1:5, 8:11)]
colors_legend = cols[c(1, 1, 2:5, 8:11)]

ROIs = list(
  "IaniChaos" = list(
    "phi" = -2,
    "longitude" = -17,
    "ylim" = c(1000, 5600)
  ), # https://trek.nasa.gov/mars/#v=0.1&x=-16.962890308581592&y=-2.10937496065263&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
  "IsmeniusCavus" = list(
    "phi" = 34,
    "longitude" = 17,
    "ylim" = c(0, 4900)
  ) # https://trek.nasa.gov/mars/#v=0.1&x=17.138671555302636&y=33.92578061716317&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
)

orientations = list(
  "NorthernHemisphere" = list(
    "South" = 0,
    "SouthEast" = -45,
    "East" = -90,
    "NorthEast" = -135,
    "North" = 180,
    "NorthWest" = 135,
    "West" = 90,
    "SouthWest" = 45),
  "SouthernHemisphere" = list(
    "South" = 180,
    "SouthEast" = -135,
    "East" = -90,
    "NorthEast" = -45,
    "North" = 0,
    "NorthWest" = 45,
    "West" = 90,
    "SouthWest" = 135)
  )

roi = ROIs$IsmeniusCavus

Ls_seq = 1:360
phi = roi$phi
longitude = roi$longitude
ylim = roi$ylim 
tau = 0.4

orient = NULL
if(phi > 0){
  orient = orientations$NorthernHemisphere
}else if(phi < 0){
  orient = orientations$SouthernHemisphere 
}else{
  stop("No orientation defined for location at exact equator.")
}


if(!exists("H_df")){
  
  # Two options to determine beta optimal.
  # 1 - Optimal surface inclination angle based on direct beam irradiance at noon. 
  # 2 - Optimal surface inclination angle based on solar insolation.
  beta_determinant = 1

  # Initialize data frame.
  NA_seq = rep(NA, max(Ls_seq))
  H_df = data.frame(
    "Ls" = Ls_seq,
    "beta_irradiance" = NA_seq,
    "beta_insolation" = NA_seq,
    "beta" = NA_seq,
    "Hh" = NA_seq,
    "Hi_S" = NA_seq,
    "Hi_SE" = NA_seq,
    "Hi_E" = NA_seq,
    "Hi_NE" = NA_seq,
    "Hi_N" = NA_seq,
    "Hi_NW" = NA_seq,
    "Hi_W" = NA_seq,
    "Hi_SW" = NA_seq)

  Ls_index = 1
  for(Ls in Ls_seq){
   
    # Optimal surface inclination angle based on direct beam irradiance at noon.
    delta = declination(Ls=Ls, unit=2)
    H_df$beta_irradiance[Ls_index] = phi - delta

    # Optimal surface inclination angle based on solar insolation.
    H_df$beta_insolation[Ls_index] = optimal_angle(Ls=Ls, phi=phi, unit=2)
    
    # Which of the two calclated beta optimals will be used.
    beta = round(ifelse(beta_determinant == 1, H_df$beta_irradiance[Ls_index],  H_df$beta_insolation[Ls_index]), 2)
    H_df$beta[Ls_index] = beta
    
    # Insolation on horizontal surface.
    H_df$Hh[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=0, gamma_c=0)
    
    # Insolation on inclined surface for different orientations.
    H_df$Hi_S[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$South)
    H_df$Hi_SE[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$SouthEast)
    H_df$Hi_E[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$East)
    H_df$Hi_NE[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$NorthEast)
    H_df$Hi_N[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$North)
    H_df$Hi_NW[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$NorthWest)
    H_df$Hi_W[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$West)
    H_df$Hi_SW[Ls_index] = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=orient$SouthWest)
    
    Ls_index = Ls_index + 1
  }  
}

# Plot title.
title = NULL
beta_based_on = ifelse(beta_determinant == 1, "direct beam irradiance at noon", "solar insolation")
if(phi == ROIs$IaniChaos$phi){
  title = paste("Iani Chaos tau ", tau, " and beta optimal based on ", beta_based_on, sep="")
  
}else if(phi == ROIs$IsmeniusCavus$phi){
  title = paste("Ismenius Cavus tau ", tau, " and beta optimal based on ", beta_based_on, sep="")
  
}else{
  stop("Unexpected planetary latitude value was selected.")
}

# Plot start convenience function.
plot_start(PLOT_OUTPUT_KEY, plot_title=title)
lwd = 1

plot(Ls_seq, H_df$Hh, ylim=ylim, type="l", lty=3, lwd=lwd, col=colors[1],
     xlab="Areocentric Longitude [deg]", ylab="Insolation [Wh/m²]")
lines(Ls_seq, H_df$Hi_S, lwd=lwd, col=colors[2])
lines(Ls_seq, H_df$Hi_SE, lwd=lwd, col=colors[3])
lines(Ls_seq, H_df$Hi_E, lwd=lwd, col=colors[4])
lines(Ls_seq, H_df$Hi_NE, lwd=lwd, col=colors[5])
lines(Ls_seq, H_df$Hi_N, lwd=lwd, col=colors[6])
lines(Ls_seq, H_df$Hi_NW, lwd=lwd, col=colors[7])
lines(Ls_seq, H_df$Hi_W, lwd=lwd, col=colors[8])
lines(Ls_seq, H_df$Hi_SW, lwd=lwd, col=colors[9])

legend("topright", ncol=5,
       legend=c("Flat", "", "S", "SE", "E", "NE", "N", "NW", "W", "SW"),
       col=colors_legend,
       lty=c(3, 0, 1, 1, 1, 1, 1, 1, 1, 1), cex=0.6, lwd=lwd)

plot_end()
