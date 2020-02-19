# Variation of global, beam, and diffuse insolation on Mars horizontal surface as a function of areocentric longitude at Endeavour Crater.
library(mars)
library(whisker)
library(RColorBrewer)

# Source functions.
source("utils.R")
source(here("mars-solar-radiation", "diurnal_insolation_plot.R"))

Ls = 71 # Ls for Aphelion
phis = c(-34, 34) # Planetary latitudes
tau = 0.4 # Optical depth tau factor.
al = 0.27 # Mean albedo on Mars is 0.27. Source: NASA-TM-105216.

# Legend labels and colors.
I_eqs_labels = c("Global", "Beam", "Diffuse")

# Color blind friendly colors.
cols = brewer.pal(n=11, name="RdYlBu")
I_eqs_cols = c(
  cols[2], #d73027 - red
  cols[4], #fdae61 - orange,
  cols[9]) #74add1 - blue

# plot_type options:
#   1 - Line.
#   2 - Stacked bars.
#   3 - Besides bars.
plot_type = 3

# Set to TRUE to only calculate and plot global beam data.
# If TRUE will not work with plot_type 2 (stacked bars).
global_only = FALSE 

phi_index = 1
for(phi in phis){
  
  # Plot title.
  title_template = "{{insolation}} variation {{index}} for Ls {{Ls}}, phi {{phi}}, tau {{tau}}, and albedo {{al}}"
  title_data = list(insolation=if(plot_type == 2) "Ibh, and Idh" else "Ih, Ibh, and Idh",
                    index=phi_index, Ls=Ls, phi=phi, tau=tau, al=al)
  title = whisker.render(title_template, title_data)
  
  # Convenience plot start function.
  plot_start("marsenv", title)
  
  # Plot.
  diurnal_insolation_plot(Ls=Ls, phi=phi, tau=tau, al=al,
                          T_begin=12, T_finish=19, T_step=1,
                          sub="", xlim=c(1, 24), ylim=c(0, 500),
                          points=FALSE, smooth=FALSE, plot_type=plot_type, cols=I_eqs_cols, global_only=global_only)

  # Add a legend to first plot.
  if(phi_index == 1){
    
    # Pick legend colors based on plot type and beams plotted.
    if(isTRUE(global_only)){
      col = I_eqs_cols[1]
      labels = I_eqs_labels[1]
    }else{
      col = if (plot_type == 2) {I_eqs_cols[2:3]} else {I_eqs_cols[1:3]}
      labels = if (plot_type == 2) {I_eqs_labels[2:3]} else {I_eqs_labels[1:3]}
    }
    
    
    # Draw legend.
    legend("topright",
          labels,
          col=col,
          pch=ifelse(plot_type != 1, 22, NA),
          lty=ifelse(plot_type != 1, 0, 1),
          pt.bg=col, cex=0.7, lwd=3)
  }
  
  plot_end()
  
  phi_index = phi_index + 1
}

