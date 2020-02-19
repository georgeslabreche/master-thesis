# Function to plot variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

library(here)
library(mars)
library(RColorBrewer)


# Store all irradiance equations and their labels
G_eqs = c(G_h, G_bh, G_dh)
G_eqs_labels = c("Global irradiance", "Beam irradiance", "Diffuse irradiance")

# Color blind friendly colors.
cols = brewer.pal(n=11, name="RdYlBu")
G_eqs_cols = c(
  cols[2], #d73027 - red
  cols[4], #fdae61 - orange,
  cols[9]) #74add1 - blue

#' Title
#'
#' @param Ls 
#' @param phi 
#' @param tau 
#' @param al Mean albedo on Mars i sabout 0.27. Source: NASA-TM-105216.
#' @param T_step 
#' @param T_min 
#' @param T_max 
#' @param sub 
#' @param xlim 
#' @param ylim 
#' @param points 
#' @param smooth 
#' @param cols 
#' @param lwd 
#' @param pch 
#'
#' @return
#' @export
#'
#' @examples
diurnal_irradiance_plot = function(Ls, phi, tau, al=0.27, T_step=1, T_min=0, T_max=24, sub="", xlim=c(0, 24), ylim=c(0, 700), points=TRUE, smooth=TRUE, cols=G_eqs_cols, lwd=1, pch=3){
  
  # Get solar time sequence.
  Ts_range = constrain_solar_time_range(Ls=Ls, phi=phi, Ts_start=T_min, Ts_end=T_max, beta=NULL, gamma_c=NULL)
  Ts_seq = seq(Ts_range$Ts_start, Ts_range$Ts_end, T_step)
  
  # Flag
  new_plot_initialized = FALSE
  
  G_index = 1
  for(G_eq in G_eqs){
    G_seq = c()
    lty = NULL
    
    # Calculate the irradiance for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    for(T_solar in Ts_seq){

      if(G_index == 1){
        # Global irradiance.
        G = G_h(Ls=Ls, phi=phi, longitude=NULL, Ts=T_solar, tau=tau, al=al)
        lty = 1
        
      }else if(G_index == 2){
        # Beam irradiance.
        G = G_bh(Ls=Ls, phi=phi, Ts=T_solar, tau=tau)
        lty = 2
        
      }else if(G_index == 3){
        # Diffuse irradiance.
        G = G_dh(Ls=Ls, phi=phi, longitude=NULL, Ts=T_solar, tau=tau, al=al)
        lty = 3
        
      }else{
        stop("Unexpected index for irradiance formula.")
      }
      
      G_seq = c(G_seq, G)
    }
    
    # Plot
    if(length(Ts_seq) == length(G_seq)){
      if(G_index == 1){
        plot(if(isTRUE(points)) Ts_seq else NULL, 
             if(isTRUE(points)) G_seq else NULL, 
             xlab="Solar Time [h]", ylab="Irradiance [W/m²]",
             xlim=xlim, ylim=ylim,
             pch=pch,
             lwd=lwd,
             col=cols[G_index],
             sub=sub,
             font.sub=2,
             cex.sub=1.2)
        
        if(isTRUE(smooth)){
          smooth_line = smooth.spline(Ts_seq, G_seq, spar=0.35)
          lines(smooth_line, col=cols[G_index], lwd=lwd, lty=lty)
          
        }else{
          lines(Ts_seq, G_seq, col=cols[G_index], lwd=lwd, lty=lty)
        }
        
        new_plot_initialized = TRUE
      }else{
        # Make sure we are plotting on a new plot when needed.
        tryCatch({
          if(new_plot_initialized == TRUE){
            if(isTRUE(smooth)){
              smooth_line = smooth.spline(Ts_seq, G_seq, spar=0.35)
              lines(smooth_line, col=cols[G_index], lwd=lwd, lty=lty)
              
            }else{
              lines(Ts_seq, G_seq, col=cols[G_index], lwd=lwd, lty=lty)
            }
            
            points(if(isTRUE(points)) Ts_seq else NULL, 
                   if(isTRUE(points)) G_seq else NULL, 
                   pch=pch,
                   col=cols[G_index])
          }else{
            stop("New plot has not been initialized.")
          }
        },
        warning = function(w) {
          # Do nothing
        },
        error = function(e) {
          # Enter here when following error occurs: plot.new has not been called yet
          plot(Ts_seq, G_seq,
               xlab="Solar Time [h]", ylab="Irradiance [W/m²]",
               xlim=xlim, ylim=ylim,
               pch=pch,
               lwd=lwd,
               col=cols[G_index],
               sub=sub,
               font.sub=2,
               cex.sub=1.2)
          
          if(isTRUE(smooth)){
            smooth_line = smooth.spline(Ts_seq, G_seq, spar=0.35)
            lines(smooth_line, col=cols[G_index], lwd=lwd, lty=lty)
            
          }else{
            lines(Ts_seq, G_seq, col=cols[G_index], lwd=lwd, lty=lty)
          }
          
          new_plot_initialized = TRUE
        },
        finally = {
          # Do nothing
        })
        
      }
    }else{
      # Using the f_89 or f_90 lookup table based implementations of the net flux function may
      # result feeding that function a rounded Z parameter that does not exist in the lookup tables.
      # This will result in an error which we are handling here by not plotting the affected irradiance type.
      message(paste("Could not calculate ", G_eqs_labels[G_index] , " for latitude ϕ=", phi, "°.", sep=""))
    }
    
    if(G_index == 3){
      G_index = 1
    }else{
      G_index = G_index + 1
    }
  }
} 