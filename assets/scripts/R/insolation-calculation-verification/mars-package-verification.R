library(mars)
library(here)
library(RColorBrewer)

source(here("utils", "plot_utils.R"))

# Color blind friendly colors.
colors = brewer.pal(n=11, name="RdYlBu")
cols = colors[c(10, 2, 3)]

Sys.setenv(NET_FLUX_FUNCTION_TYPE = "polynomial")
Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

# Spacecraft properties.
spacecrafts = list(
  "VL1" = list(
    "latitude" = 22.3,
    "longitude" = -49.97,
    "beta_optimal" = 6.5
  ),
  "VL2" = list(
    "latitude" = 47.7,
    "longitude" = 134.29,
    "beta_optimal" = 22
  )
)

if(!exists("expected_data")){
  # Expected data
  base_url = "https://raw.githubusercontent.com/georgeslabreche/mars/master/tests/testthat/data/"
  expected_data = list(
    "VL1" = list(
      "tau" = read.csv(paste(base_url, "discretized_tau_at_vl1_fig_3_1991_update.csv", sep="")),
      "insolation" =
        list(
          "horizontal" = read.csv(paste(base_url, "daily_insolation_on_horizontal_surface_at_vl1_table_v_update_1990.csv", sep="")),
          "beta_equals_phi" = read.csv(paste(base_url, "daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl1_table_ii_1993.csv", sep="")),
          "beta_optimal" = read.csv(paste(base_url, "daily_insolation_on_optimal_inclined_angle_beta_at_vl1_table_iii_1993.csv", sep=""))
        )
    ),
    "VL2" = list(
      "tau" = read.csv(paste(base_url, "discretized_tau_at_vl2_fig_3_1991_update.csv", sep="")),
      "insolation" =
        list(
          "horizontal" = read.csv(paste(base_url, "daily_insolation_on_horizontal_surface_at_vl2_table_v_update_1990.csv", sep="")),
          "beta_equals_phi" = read.csv(paste(base_url, "daily_insolation_on_an_inclined_surface_for_beta_equals_phi_at_vl2_table_ii_1993.csv", sep="")),
          "beta_optimal" = read.csv(paste(base_url, "daily_insolation_on_optimal_inclined_angle_beta_at_vl2_table_iii_1993.csv", sep=""))
        )
    )
  )  
}


#' Calculate and plot differences between expected and calculated daily insolations surface.
#'
#' @param title 
#' @param insolation_type 
#' @param spacecraft 
#' @param Ls_seq 
#' @param beta_equals_phi 
#'
#' @return
plot_insolation_differences = function(insolation_type, spacecraft, Ls_seq, beta_equals_phi=FALSE){
  
  # Location.
  phi = spacecrafts[[spacecraft]][["latitude"]]
  longitude = spacecrafts[[spacecraft]][["longitude"]]
  
  # Orientation.
  gamma_c = 0
  
  # Inclination.
  beta = NULL
  
  # Optical depth as measured by the spacecraft (discretized).
  measured_taus = expected_data[[spacecraft]][["tau"]]
  
  # Get the expected insolations as measured by the spacecraft.
  # Also set the inclination angle when appropriate.
  expected_insolations = NULL
  if(insolation_type %in% c("Hbh", "Hdh", "Hh")){
    expected_insolations = expected_data[[spacecraft]][["insolation"]][["horizontal"]]
    
  }else if(insolation_type %in% c("Hb", "Hd", "Hal", "H")){
    if(isTRUE(beta_equals_phi)){
      expected_insolations = expected_data[[spacecraft]][["insolation"]][["beta_equals_phi"]]
      beta = phi
      
    }else{
      expected_insolations = expected_data[[spacecraft]][["insolation"]][["beta_optimal"]]
      beta = spacecrafts[[spacecraft]][["beta_optimal"]]
    }
  }else{
    stop("Invalid insolation type.")
  }
  
  calculated_insolations = c()
  taus = c()
  diffs = c()
  diffs_pc = c()
  
  for(Ls in Ls_seq){
    # Measured tau.
    tau_measured = measured_taus[measured_taus$Ls == Ls, "tau"]
    
    # Expected insolation.
    insolation_expected = expected_insolations[expected_insolations$Ls == Ls, insolation_type]
    
    # Calculated insolation.
    insolation_calculated = NULL
    
    # Horizontal surface.
    if(insolation_type == "Hbh"){ # Beam daily insolation on a horizontal surface.
      insolation_calculated = H_bh(Ls=Ls, phi=phi, tau=tau_measured)
      
    }else if(insolation_type == "Hdh"){ # Diffuse daily insolation on a horizontal surface.
      insolation_calculated = H_dh(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, al=0.1)
      
    }else if(insolation_type == "Hh"){ # Global daily insolation on a horizontal surface.
      insolation_calculated = H_h(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, al=0.1)
      
    }
    # Incline surface.
    else if(insolation_type == "Hb"){ # Beam daily insolation on an inclined surface.
      insolation_calculated = H_bi(Ls=Ls, phi=phi, tau=tau_measured, beta=beta, gamma_c=gamma_c)
      
    }else if(insolation_type == "Hd"){ # Diffuse daily insolation on an inclined surface.
      insolation_calculated = H_di(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c)
      
    }else if(insolation_type == "Hal"){ # Albedo daily insolation on an inclined surface.
      insolation_calculated = H_ali(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c)
      
    }else if(insolation_type == "H"){ # Global daily insolation on an inclined surface.
      insolation_calculated = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau_measured, beta=beta, gamma_c=gamma_c)
      
    }else{
      stop("Invalid insolation type.")
    }
    
    # Collect data to be plotted later.
    calculated_insolations = c(calculated_insolations, insolation_calculated)
    taus = c(taus, tau_measured)
    
    diff = insolation_expected-insolation_calculated
    diffs = c(diffs, diff)
    
    diff_pc = ((insolation_expected-insolation_calculated) / insolation_calculated) * 100
    diffs_pc = c(diffs_pc, diff_pc)
  }
  
  # Plot the results.

  #######################################
  # Expected vs calculated insolations. #
  #######################################
  
  # Title.
  plot_title = paste(insolation_type, "Exp. & Calc. at", spacecraft)
  
  # Add inclination to title.
  if(!is.null(beta)){
    plot_title = paste(plot_title, " with beta = ", beta, " deg", sep="")
  }
  
  # Plot data.
  plot_data(x=Ls_seq, y=expected_insolations[, insolation_type], y2=calculated_insolations,
            title=plot_title, xlab="Ls [deg]", ylab=paste(insolation_type, "[Wh/m2]"), type="l")
  
  ###################
  # Optical depths. #
  ###################
  
  # # Title.
  # plot_title = paste("Measured Tau at", spacecraft)
  # 
  # # Plot data.
  # plot_data(x=Ls_seq, y=taus, title=plot_title,
  #           xlab="Ls [deg]", ylab="tau", type="l")
  
  ###################################################################
  # Difference between expected and calculated insolations [Wh/m2]. #
  ###################################################################
  
  # # Title.
  # plot_title = paste(insolation_type, "Diff. Bet. Exp. & Calc. at", spacecraft)
  # if(!is.null(beta)){
  #   plot_title = paste(plot_title," with beta = ", beta, " deg [Wh/m2]", sep="")
  # }else{
  #   plot_title = paste(plot_title, "[Wh/m2]")
  # }
  # 
  # # Plot data.
  # plot_data(x=taus, y=diffs, title=plot_title,
  #           xlab="tau", ylab="difference [Wh/m2]")

  ###############################################################
  # Difference between expected and calculated insolations [%]. #
  ###############################################################
  
  # Title.
  plot_title = paste(insolation_type, "Diff. Bet. Exp. & Calc. at", spacecraft)
  if(!is.null(beta)){
    plot_title = paste(plot_title," with beta = ", beta, " deg [%]", sep="")
  }else{
    plot_title = paste(plot_title, "[%]")
  }
  
  # Plot data.
  plot_data(x=taus, y=diffs_pc, title=plot_title,
            xlab="tau", ylab="difference [%]")
  
  # # Display min and max diffs.
  # print(paste("Min diff: ", round(min(diffs_pc), 2), " %", sep=""))
  # print(paste("Max diff: ", round(max(diffs_pc), 2), " %", sep=""))
  # 
  # # Display values of tau with highest diff.
  # threshold = 2.5
  # large_diff_taus = c()
  # index = 1
  # for(d in diffs_pc){
  #   if(d >= threshold){
  #     large_diff_taus = c(large_diff_taus, taus[index])
  #   }
  #   
  #   index = index + 1
  # }
  # 
  # print(paste("Taus with diff. larger than ", threshold, ":", sep=""))
  # print(large_diff_taus)
  # print(paste("Min tau: ", round(min(large_diff_taus), 2), sep=""))
  # print(paste("Max tau: ", round(max(large_diff_taus), 2), sep=""))
}


#' Plot data in a window or written in an image file.
#' The plots destination depends on values set in the config file.
#'
#' @param x 
#' @param y 
#' @param y2 
#' @param title 
#' @param xlab 
#' @param ylab 
#' @param type
#'
#' @return
#' @export
#'
#' @examples
plot_data = function(x, y, y2=NULL, title, xlab, ylab, type=NULL){
  
  # Convenience plot start function.
  plot_start("insolationverification", title)
  
  # Determine color and line for first plot.
  # If we are plotting the percentage difference then use cols[3].
  col = cols[1]
  if(is.null(y2)){
    col = cols[3]
  }
  
  # Plot data
  plot(x=x, y=y,
       xlab=xlab, ylab=ylab,
       col=col, type=type, lwd=1, pch=4, lty=3)
  
  if(!is.null(y2)){
    lines(x=x, y=y2,
          col=cols[2], lwd=1, lty=6)
    
    legend("topright", legend=c("Published", "Calculated"),
           col=c(cols[1], cols[2]), lty=c(3, 6), lwd=1, cex=0.7)
  }
  
  plot_end()
}

#' Calculate and plot differences between expected and calculated daily insolations on a horizontal surface.
#'
#' @param location 
#' @param global_only
#'
#' @return
plot_insolation_on_horizontal_surface = function(location, global_only=TRUE){
  
  Ls_seq = seq(0, 355, 5)
  
  # Global insolation.
  plot_insolation_differences(
    insolation_type = "Hh",
    spacecraft = location,
    Ls_seq = Ls_seq)
  
  if(!isTRUE(global_only)){
    # Beam insolation.
    plot_insolation_differences(
      insolation_type = "Hbh",
      spacecraft = location,
      Ls_seq = Ls_seq)
    
    # Diffuse insolation.
    plot_insolation_differences(
      insolation_type = "Hdh",
      spacecraft = location,
      Ls_seq = Ls_seq)
  }

}

#' Calculate and plot differences between expected and calculated daily insolations on a inclined surface.
#'
#' @param location 
#' @param beta_equals_phi 
#' @param global_only
#'
#' @return
plot_insolation_on_inclined_surface = function(location, beta_equals_phi=FALSE, global_only=TRUE){
  
  Ls_seq = seq(0, 355, 5)
  if(isTRUE(beta_equals_phi)){
    Ls_seq = c(Ls_seq, 360)
  }
  
  # Global insolation.
  plot_insolation_differences(
    insolation_type = "H",
    spacecraft = location,
    Ls_seq = Ls_seq,
    beta_equals_phi = beta_equals_phi)
  
  if(!isTRUE(global_only)){
    # Beam insolation.
    plot_insolation_differences(
      insolation_type = "Hb",
      spacecraft = location,
      Ls_seq = Ls_seq,
      beta_equals_phi = beta_equals_phi)
    
    # Diffuse insolation.
    plot_insolation_differences(
      insolation_type = "Hd",
      spacecraft = location,
      Ls_seq = Ls_seq,
      beta_equals_phi = beta_equals_phi)
    
    # Albedo insolation.
    plot_insolation_differences(
      insolation_type = "Hal",
      spacecraft = location,
      Ls_seq = Ls_seq,
      beta_equals_phi = beta_equals_phi)    
  }
}

# Plot data.
# Only plot global insolation values (ignore beam, diffuse, and albedo).
global_only = TRUE

############################################
# Insolation on horizontal surface at VL1. #
############################################
plot_insolation_on_horizontal_surface(location="VL1", global_only=global_only)

############################################
# Insolation on horizontal surface at VL2. #
############################################
plot_insolation_on_horizontal_surface(location="VL2", global_only=global_only)

##########################################################
# Insolation on inclined surface with beta = phi at VL1. #
##########################################################
plot_insolation_on_inclined_surface(location="VL1", beta_equals_phi=TRUE, global_only=global_only)

##########################################################
# Insolation on inclined surface with beta = phi at VL2. #
##########################################################
plot_insolation_on_inclined_surface(location="VL2", beta_equals_phi=TRUE, global_only=global_only)

##########################################################
# Insolation on inclined surface with beta = phi at VL1. #
##########################################################
plot_insolation_on_inclined_surface(location="VL1", beta_equals_phi=FALSE, global_only=global_only)

##########################################################
# Insolation on inclined surface with beta = phi at VL2. #
##########################################################
plot_insolation_on_inclined_surface(location="VL2", beta_equals_phi=FALSE, global_only=global_only)
