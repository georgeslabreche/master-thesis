# Diurnal variation of global irradiance on Mars inclined surface for different orientations for different areocentric longitudes.

# Load libraries.
library(mars)
library(whisker)
library(RColorBrewer)

source("utils.R")

# Disable warnings from the mars library.
Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)


orientations = list(
  "NorthernHemisphere" = list(
    "South" = 0,
#    "SouthEast" = -45,
    "East" = -90,
#    "NorthEast" = -135,
    "North" = 180,
#    "NorthWest" = 135,
    "West" = 90),
#    "SouthWest" = 45),
  "SouthernHemisphere" = list(
    "South" = 180,
#    "SouthEast" = -135,
    "East" = -90,
#    "NorthEast" = -45,
    "North" = 0,
#    "NorthWest" = 45,
    "West" = 90)
#    "SouthWest" = 135)
)

Ls = 248 # Periphelion.
phi = 34 # Ismenius Cavus.
tau = 0.4 # Optical depth.
al = 0.27 # Mean albedo on Mars is 0.27. Source: NASA-TM-105216.
betas = seq(0, 80, 20) # Slope inclination angle.

if(phi >= 0){
  gamma_orientation_angles = orientations$NorthernHemisphere
}else{
  gamma_orientation_angles = orientations$SouthernHemisphere
}

ylim = c(0, 500)

# Color blind friendly colors.
cols = brewer.pal(n=11, name="RdYlBu")
cols = rev(c(
  cols[1], #a50026 - dark red.
  cols[2], #d73027 - red.
  cols[4], #fdae61 - orange.
  cols[9], #74add1 - blue.
  cols[11])) #313695 - dark blue.

####################################################################################
# Title: Variation of Mars solar irradiance Gβ for different slope orientations γ. #
# Subtitle: Effect of solar time with slope angle β as a parameter.                #
####################################################################################
plot_a = function(Ls, phi, tau, al, Ts_range, betas, orientations_filter, xTs=TRUE, lwd=2){
  
  gamma_c_index = 1
  
  for(gamma_c in gamma_orientation_angles){
    orientation_name = names(gamma_orientation_angles)[gamma_c_index]
    
    if(orientation_name %in% orientations_filter){
      
      # Plot title.
      title_template = "Gi variation for Ls {{Ls}}, phi {{phi}}, tau {{tau}}, gamma_c {{gamma_c}}, and albedo {{al}}."
      title_data = list(Ls=Ls, phi=phi, tau=tau, gamma_c=orientation_name, al=al)
      title = whisker.render(title_template, title_data)
      
      # Convenience plot start function.
      plot_start("marsenv", title)
      
      beta_index = 1
      for(beta in betas){
        
        x = c()
        y = c()
        
        for(T_s in Ts_range){
          if(isTRUE(xTs)){
            x = c(x, T_s)
          }else{
            z = Z(Ls=Ls, phi=phi, Ts=T_s)
            x = c(x, z)
          }
          
          G = G_i(Ls=Ls, phi=phi, longitude=NULL, Ts=T_s, tau=tau, al=al, beta=beta, gamma_c=gamma_c)
          y = c(y, G)
        }
        
        if(beta_index == 1){
          plot(x, y,
               xlab=if(isTRUE(xTs)) "Solar Time [h]" else "Z [deg]",
               ylab="Irradiance [W/m²]",
               xlim=if(isTRUE(xTs)) c(Ts_range[1], Ts_range[length(Ts_range)]) else c(0, 75),
               ylim=ylim,
               #sub=paste("γ = ", gamma_c, "°", sep=""),
               font.sub=2,
               cex.sub=1.2,
               type="l",
               lty=beta_index,
               lwd=lwd,
               col=cols[beta_index])
          
          if(orientation_name == "South"){
            legend("topright",
                   title="β",
                   paste(betas, "°" , sep=""),
                   col=cols,
                   cex=0.7, lwd=1,
                   lty=1:length(betas))
          }
          
        }else{
          lines(x, y,
                type="l",
                col=cols[beta_index],
                lty=beta_index,
                lwd=lwd)
        }
        
        beta_index = beta_index + 1
      }
      
      plot_end()
      
      gamma_c_index = gamma_c_index + 1
    }
  }
}


#######
# Plot effect of solar time.
#######
# Solar time range.
Ts_range = seq(7, 17, 0.25)

# Plot for clear day.
plot_a(Ls=Ls, phi=phi, tau=tau, al=al, Ts_range=Ts_range, betas=betas,
      xTs=TRUE)

# # Plot for dusting day tau factor 1.
# plot_a(Ls=Ls, phi=phi, tau=1, al=al, Ts_range=Ts_range, betas=betas,
#        orientations_filter=c("South"), xTs=TRUE)
# 
# # Plot for very dusting day tau factor 2.
# plot_a(Ls=Ls, phi=phi, tau=2, al=al, Ts_range=Ts_range, betas=betas,
#        orientations_filter=c("South"), xTs=TRUE)