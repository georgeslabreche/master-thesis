# Diurnal variation of global, beam, and diffuse irradiance on Mars horizontal surface at different planetary latitudes.
library(mars)
library(here)
library(whisker)
library(RColorBrewer)

# Source functions.
source("utils.R")
source(here("mars-solar-radiation", "diurnal_irradiance_plot.R"))

# Mars environment parameters.
Ls = 248 # Ls for Periphelion.
phis = c(-34, 34) # Planetary latitudes.
tau = 0.4 # Optical depth tau factor.
al = 0.27 # Mean albedo on Mars is 0.27. Source: NASA-TM-105216.

# Legend labels and colors.
G_eqs_labels = c("Global", "Beam", "Diffuse")

# Color blind friendly colors.
cols = brewer.pal(n=11, name="RdYlBu")
G_eqs_cols = c(
  cols[2], #d73027 - red
  cols[4], #fdae61 - orange,
  cols[9]) #74add1 - blue


phi_index = 1
for(phi in phis){
  title_template = "Gh, Gbh, Gdh variation {{index}} for Ls {{Ls}}, phi {{phi}}, tau {{tau}}, and albedo {{al}}"
  title_data = list(index=phi_index, Ls=Ls, phi=phi, tau=tau, al=al)
  title = whisker.render(title_template, title_data)
  
  # Convenience plot start function.
  plot_start("marsenv", title)
  
  # FIXME: Not working with smooth=TRUE
  diurnal_irradiance_plot(Ls=Ls, phi=phi, tau=tau, al=al, T_step=0.25, points=FALSE, smooth=FALSE, cols=G_eqs_cols, lwd=2)
  
  # Add a legend to first plot.
  if(phi_index == 1){
    legend("topright",
           G_eqs_labels,
           col = G_eqs_cols,
           cex=0.7, lwd=1,
           lty=c(1, 2, 3))
  }
  plot_end()
  phi_index = phi_index + 1
}
