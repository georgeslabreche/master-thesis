# FIXME: In many instances this script refers to SA Area when it actually means solar cell coverage area.

library(here)
library(whisker)
library(RColorBrewer)

source(here("utils", "insolation_utils.R"))
source(here("utils", "power_utils.R"))

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "solar_array"

# Colors for plotting.
cols = brewer.pal(n=10, name="RdYlBu")[c(1, 3, 8, 9, 10, 4)]
cols_area_targets = cols[c(1, 6)]

get_max_traverse_time_by_solving_power_budget = function(H, SA_area, Ls, phi, P_traverse, optimal_pose){
  
  # Available/Generated Energy.
  E_generated = H * e * PR * SA_area
  
  # How much Energy is left over for Traverse and Idle - Day modes.
  df_power = get_power_budget_for_traverse_sol(Ls=Ls, phi=phi, propulsion_power=P_traverse, optimal_pose)
  E_leftover_for_traverse_and_idle_day = E_generated - sum(df_power$Energy[!is.na(df_power$Energy)])
  
  # Prepare equation system to solve t_traverse and t_idle_day
  A = matrix(c(df_power$Power[df_power$Mode == "Traverse"], 1, 
               df_power$Power[df_power$Mode == "Idle - Day"], 1),
             2, 2)
  
  # Solve.
  t_leftover_for_traverse_and_idle_day = 24 - (sum(df_power$Duration[!is.na(df_power$Duration)]) /60 )
  
  b = matrix(c(E_leftover_for_traverse_and_idle_day,
               t_leftover_for_traverse_and_idle_day),
               2, 1)
  solution = solve(A, b)
  
  # Get values.
  t_traverse = solution[1,1]
  t_idle_day = solution[2,1]
  
  if(t_traverse < 0 && t_idle_day < 0){
    stop("Error! Both t_traverse and t_idle_day are negative. This is impossible.")
  }
  
  # Make sure there are no negative durations.
  if(t_idle_day < 0){
    t_traverse = t_traverse + t_idle_day
    t_idle_day = 0
  }
  
  if(t_traverse < 0){
    t_idle_day = t_idle_day + t_traverse
    t_traverse = 0
  }
  
  return(t_traverse)
}


get_max_traverse_times = function(location_id, Ls_seq, phi, beta_max,
                                  SA_area, P_traverse,
                                  Ls_clear_Sols, Ls_dusty_Sols, tau_clear, tau_dusty,
                                  draw_plots=FALSE,
                                  ylim_energy=NULL, ylim_traverse_time=NULL){
  
  # Load daily insolation lookup tables.
  H_df_clear_Sols = get_daily_insolation_lookup_table(location_id=location_id, tau=tau_clear, beta_max=beta_max, Ls_filter=Ls_clear_Sols)
  H_df_dusty_Sols = get_daily_insolation_lookup_table(location_id=location_id, tau=tau_dusty, beta_max=beta_max, Ls_filter=Ls_dusty_Sols)
 

  t_max_inclined_traverses = c()
  t_max_horizontal_traverses = c()
  t_daylight = c()
  
  E_generated_inclined = c()
  E_generated_horizontal = c()
  
  for(Ls in Ls_seq){
    tau = NA
    H_df = NULL
    
    # Clear Sols.
    if(Ls %in% Ls_clear_Sols){
      tau = tau_clear
      H_df = H_df_clear_Sols
      
    }else{
      # Dusty / stormy Sols.
      tau = tau_dusty
      H_df = H_df_dusty_Sols
    }
    
    H_best = H_df$H_best[H_df$Ls == Ls]
    H_horiz = H_df$H_horiz[H_df$Ls == Ls]
    
    # Solve power budget and keep track of maximum traverse times.
    # For inclined surface.
    t_max_inclined = get_max_traverse_time_by_solving_power_budget(H=H_best, SA_area=SA_area,
                                                                   Ls=Ls, phi=phi,
                                                                   P_traverse=P_traverse,
                                                                   optimal_pose=TRUE)
    
    t_max_inclined_traverses = c(t_max_inclined_traverses, t_max_inclined)
    
    E_generated = H_best * e * PR * SA_area
    E_generated_inclined =  c(E_generated_inclined, E_generated)
    
    # For horiyontal surface.
    t_max_horizontal = get_max_traverse_time_by_solving_power_budget(H=H_horiz, SA_area=SA_area,
                                                                     Ls=Ls, phi=phi,
                                                                     P_traverse=P_traverse,
                                                                     optimal_pose=FALSE)
    
    t_max_horizontal_traverses = c(t_max_horizontal_traverses, t_max_horizontal)
    
    E_generated = H_horiz * e * PR * SA_area
    E_generated_horizontal =  c(E_generated_horizontal, E_generated)
    
    # Get daylight duration on flat terrain minus non traverse time.
    t_d = T_d(Ls=Ls, phi=phi, beta=0, gamma_c=0) - sum(unlist(fixed_day_mode_durations)) / 60
    t_daylight = c(t_daylight, t_d)
  }
  
  if(isTRUE(draw_plots)){
    E_df = data.frame(
      "Ls" = Ls_seq,
      "E_inclined" = E_generated_inclined,
      "E_horizontal" = E_generated_horizontal
    )
    
    plot_title = paste(location_id, " Daily Generated Energy for Solar Cell Coverage Area ", SA_area, "m2", sep="")
    plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title)
    
    plot(1, type="n", ylim=ylim_energy, xlim=c(1, 360),
         ylab="Energy Generated [Wh/m²]", xlab="Areocentric Longitude, Ls [deg]")
    
    points(E_df$Ls[E_df$Ls %in% Ls_clear_Sols], E_df$E_inclined[E_df$Ls %in% Ls_clear_Sols], pch=20, cex=0.5, col=cols[1])  
    points(E_df$Ls[E_df$Ls %in% Ls_dusty_Sols], E_df$E_inclined[E_df$Ls %in% Ls_dusty_Sols], pch=20, cex=0.5, col=cols[1]) 
    
    points(E_df$Ls[E_df$Ls %in% Ls_clear_Sols], E_df$E_horizontal[E_df$Ls %in% Ls_clear_Sols], pch=20, cex=0.5, col=cols[2])  
    points(E_df$Ls[E_df$Ls %in% Ls_dusty_Sols], E_df$E_horizontal[E_df$Ls %in% Ls_dusty_Sols], pch=20, cex=0.5, col=cols[2])
    
    legend("topright",
           legend=c("SA inclined", "SA horizontal"),
           col=c(cols[1], cols[2]),
           lty=1,
           cex=0.8,
           lwd=1)
    
    plot_end()
  } 
  
  if(isTRUE(draw_plots)){
    plot_title = paste(location_id, " ", P_traverse, "W Max Traverse Durations for Solar Cell Coverage Area ", SA_area, "m2", sep="")
    plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title)
    
    plot(Ls_seq, t_max_inclined_traverses, type="l",
         "ylab"="Time Duration [hr]", xlab="Areocentric Longitude, Ls [deg]",
         col=cols[1], ylim=ylim_traverse_time, lwd=2, lty=2)
    
    lines(Ls_seq, t_max_horizontal_traverses, col=cols[2], lwd=2, lty=4)
    
    lines(Ls_seq, t_daylight, col=cols[4], lwd=2, lty=3)
    
    legend("topright",
           legend=c("Available daylight traverse time", "Max traverse time - SA inclined", "Max traverse time - SA horizontal"),
           col=c(cols[4], cols[1], cols[2]),
           lty=c(3, 2, 4),
           cex=0.7,
           lwd=1)
    
    
    plot_end()
  }
  
  t_max_traverses = list(
    "inclined" = t_max_inclined_traverses,
    "horizontal" = t_max_horizontal_traverses)
  
  return(t_max_traverses)
}

# By default the rover's minimum nominal speed is 2 cm/s (0.02 m/s).
get_max_traverse_distances = function(location_id, phi, beta_max, SA_area, avg_slippage_rate,
                                      P_traverse, v_rover_nominal, # m/s 
                                      Ls_seq, Ls_clear_Sols,  Ls_dusty_Sols,
                                      tau_clear=tau_clear, tau_dusty=tau_dusty,
                                      draw_plots=FALSE, verbose=FALSE,
                                      ylim_energy=NULL, ylim_traverse_time=NULL){
  

  # The maximum traverse time possible per Sol, in minutes.
  
  t_max_traverses = get_max_traverse_times(Ls_seq=Ls_seq, location_id=location_id,
                                           phi=phi, beta_max=beta_max,
                                           SA_area=SA_area, P_traverse=P_traverse,
                                           Ls_clear_Sols=Ls_clear_Sols, Ls_dusty_Sols=Ls_dusty_Sols,
                                           tau_clear=tau_clear, tau_dusty=tau_dusty,
                                           draw_plots=draw_plots,
                                           ylim_energy, ylim_traverse_time)
  
  
  get_distance_covered = function(t_max_traverses){
    
    # Traverse time cumulated during the given traverse period.
    t_traverse = sum(t_max_traverses) * 60 * 60 # [seconds]
    
    # Distance covered.
    distance_covered = v_rover_nominal * t_traverse * 1e-3 # [km]
    
    # Account for slippage, assumption is 10% of total.
    distance_covered = distance_covered * (1 - avg_slippage_rate)    
  }
  
  distance_covered_inclined = get_distance_covered(t_max_traverses=t_max_traverses$inclined)
  distance_covered_horizontal = get_distance_covered(t_max_traverses=t_max_traverses$horizontal)
  
  distance_diff = round(distance_covered_inclined - distance_covered_horizontal, 2)
  distance_gain = round((distance_diff / distance_covered_horizontal)*100, 2)
  
  
  t_max_distances = list(
    "d_tot_inclined" = round(distance_covered_inclined, 2),
    "d_tot_horizontal" = round(distance_covered_horizontal, 2),
    "d_tot_diff" = distance_diff,
    "d_tot_gain" = distance_gain
  )
  
  # Verbosity.
  if(isTRUE(verbose)){
    verbose_template = "At {{location_id}} with solar cell coverage area {{SA_area}} sqm:
      \t d_tot_inclined = {{d_tot_inclined}} km/MY
      \t d_tot_horizontal = {{d_tot_horizontal}} km/MY
      \t d_tot_diff = {{d_tot_diff}} km/MY
      \t d_tot_gain = {{d_tot_gain}} %\n\n"
    
    verbose_data = list(
      location_id = location_id,
      SA_area = SA_area,
      d_tot_inclined = t_max_distances$d_tot_inclined,
      d_tot_horizontal = t_max_distances$d_tot_horizontal,
      d_tot_diff = t_max_distances$d_tot_diff,
      d_tot_gain = t_max_distances$d_tot_gain
    )
    
    verbose_text = whisker.render(verbose_template, verbose_data)
    cat(verbose_text)
  }
  
  return(t_max_distances)
}

plot_distance_inclined_surface_gains = function(location_id, phi, beta_max,
                                                SA_area, avg_slippage_rate,
                                                P_traverse, v_rover_nominal,
                                                Ls_seq, Ls_clear_Sols, Ls_dusty_Sols,
                                                tau_clear=tau_clear, tau_dusty=tau_dusty,
                                                ylim){
  gains = c()
  for(Ls in Ls_seq){
    d = get_max_traverse_distances(location_id=location_id, phi=phi, beta_max=beta_max,
                                   SA_area=SA_area, avg_slippage_rate=avg_slippage_rate,
                                   P_traverse=P_traverse, v_rover_nominal=v_rover_nominal,
                                   Ls_seq=Ls, Ls_clear_Sols=Ls_clear_Sols, Ls_dusty_Sols=Ls_dusty_Sols,
                                   tau_clear=tau_clear, tau_dusty=tau_dusty,
                                   draw_plots=FALSE, verbose=FALSE)
    
    # Avoid the result of divide by zero for Sols where there is no traversals
    gain = d$gain
    if(is.nan(gain)){
      gain = NA
    }

    gains = c(gains, gain)
  }
  
  dev.new()
  plot(Ls_seq, gains, type="l", col="red", main=location_id, ylim=ylim)  
}

plot_traverse_distance_gains_for_different_taus = function(location_id, SA_area_targets, SA_area_alts=NULL, phi, beta_max, taus, ylim, draw_plots, verbose){

  
  # The SA_area_targets lines are the SA area required by design.
  # The other lines are alternative SA area sizes we use to compare against for the sake of context.
  SA_areas = sort(c(SA_area_targets, SA_area_alts))
  
  # Initialize results list.
  results = vector(mode="list", length=length(SA_areas))
  for(index in length(results)){
    results[[index]] = list("SA_area" = NULL, "df" = NULL)
  }
  
  SA_area_index = 1
  for(SA_area_current in SA_areas){
    #print(paste("Fetching data for SA area = ", SA_area_current, sep=""))
    
    df_dmax = data.frame(
      "tau" = taus,
      "d_tot_inclined" = rep(NA, length(taus)),
      "d_tot_horizontal" = rep(NA, length(taus)),
      "d_tot_diff" = rep(NA, length(taus)),
      "d_tot_gain" = rep(NA, length(taus))
    )
    
    tau_index = 1
    for(tau in taus){
      
      t_max_distances = get_max_traverse_distances(location_id=location_id, phi=phi, beta_max=beta_max,
                                                   SA_area=SA_area_current, avg_slippage_rate=avg_slippage_rate,
                                                   P_traverse=P_traverse, v_rover_nominal=v_rover_nominal,
                                                   Ls_seq=Ls_seq, Ls_clear_Sols=Ls_clear_Sols, Ls_dusty_Sols=Ls_dusty_Sols,
                                                   tau_clear=tau, tau_dusty=tau,
                                                   draw_plots=draw_plots, verbose=verbose)
      
      df_dmax[tau_index,]$d_tot_inclined = t_max_distances$d_tot_inclined
      df_dmax[tau_index,]$d_tot_horizontal= t_max_distances$d_tot_horizontal
      df_dmax[tau_index,]$d_tot_diff = t_max_distances$d_tot_diff 
      df_dmax[tau_index,]$d_tot_gain = t_max_distances$d_tot_gain
      
      tau_index = tau_index + 1
    }
    
    #print(df_dmax)
    
    # # Get color for plot line.
    # # For the alternative SA area, the tau factor value is used as the color index so
    # # this will not work as expected if there are float values for alternative SA areas.
    # col = ifelse(SA_area_current %in% SA_area_targets, cols[1], cols[SA_area_current])
    # 
    # # Plot with only the target SA area size
    # if(SA_area_current %in% SA_area_targets){
    #   plot_title = paste(location_id, " ", P_traverse, "W Traverse Gains for ", SA_area_current, "m2 SA Area", sep="")
    #   plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title)
    #   
    #   plot(df_dmax$tau, df_dmax$d_tot_gain, type="l",
    #        xlab="Optical Depth, τ", ylab="Traverse Distance Gain [%]",
    #        col=col, lwd=2)
    #   
    #   plot_end()      
    # }

    results[[SA_area_index]]$SA_area = SA_area_current
    results[[SA_area_index]]$df = df_dmax
    
    SA_area_index = SA_area_index + 1
  }
  
  ## All data has been gathered, let's plot.
  
  # Plot Gains for target SA areas
  for(index in 1:length(results)){
    SA_area = results[[index]]$SA_area
    
    if(SA_area %in% SA_area_targets){
      df = results[[index]]$df
      
      # # Get color for plot line.
      # # For the alternative SA area, the tau factor value is used as the color index so
      # # this will not work as expected if there are float values for alternative SA areas.
      # col = ifelse(SA_area_current %in% SA_area_targets, cols[1], cols[SA_area_current])

      # Plot with only the target SA area size
      plot_title = paste(location_id, " ", P_traverse, "W Traverse Gains for ", SA_area, "m2 Solar Cell Coverage Area", sep="")
      plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title)

      plot(df$tau, df$d_tot_gain, type="l",
           xlab="Optical Depth, τ", ylab="Traverse Distance Gain [%]",
           col=cols[1], lwd=2)

      plot_end()

    }
  }
  
  # Plot gains for all SA areas. Emphasize target SA areas.
  col_legend = c()
  lty_legend = c()

  for(index in 1:length(results)){
    # Fetch data to be plotted for the current SA area.
    SA_area = results[[index]]$SA_area
    df = results[[index]]$df
    
    # Create plot window.
    if(index == 1){
    
      plot_title = paste(location_id, " ", P_traverse, "W Traverse Gains for Different Solar Cell Coverage Areas", sep="")
      plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title)
    
      plot(1, type="n", xlab="Optical Depth, τ", ylab="Traverse Distance Gain [%]",
           xlim=c(min(taus), max(taus)), ylim=ylim)
    
    }
    
    # Emphasize target SA areas.
    col = ifelse(SA_area %in% SA_area_targets, cols_area_targets[match(SA_area, SA_area_targets)], cols[SA_area])
    col_legend = c(col_legend, col)
    
    lty = ifelse(SA_area %in% SA_area_targets, 1, 3)
    lty_legend = c(lty_legend, lty)
    
    # Plot data.
    lines(df$tau, df$d_tot_gain, col=col, lwd=2, lty=lty)
  }
  
  # Legend.
  legend("topright",
         title="Area [m²]", # Solar cell coverage area
         legend=SA_areas,
         col=col_legend,
         lty=lty_legend,
         cex=0.7,
         lwd=1)

  # Draw second plot.
  plot_end()
  
}

generate_plots = function(location_id, phi, beta_max, SA_area_targets, SA_area_alts, taus,
                          ylim_energy, ylim_traverse_duration,
                          ylim_traverse_distance_gains){
  # --------------------------------------- #
  # 1 - Daily maximum durations of traverse #
  # --------------------------------------- #
  draw_plots = TRUE
  verbose = TRUE
  
  for(SA_area in SA_area_targets){
    t_max_distances = get_max_traverse_distances(location_id=location_id, phi=phi, beta_max=beta_max,
                                                 SA_area=SA_area, avg_slippage_rate=avg_slippage_rate,
                                                 P_traverse=P_traverse, v_rover_nominal=v_rover_nominal,
                                                 Ls_seq=Ls_seq, Ls_clear_Sols=Ls_clear_Sols, Ls_dusty_Sols=Ls_dusty_Sols,
                                                 tau_clear=tau_clear, tau_dusty=tau_dusty,
                                                 draw_plots=draw_plots, verbose=verbose,
                                                 ylim_energy=ylim_energy, ylim_traverse_time=ylim_traverse_duration)
  }

  
  # ------------------------ #
  # 2 - Daily traverse gains #
  # ------------------------ #
  # plot_distance_inclined_surface_gains(location_id=location_id, phi=phi, beta_max=beta_max,
  #                                      SA_area=SA_area, avg_slippage_rate=avg_slippage_rate,
  #                                      P_traverse=P_traverse, v_rover_nominal=v_rover_nominal,
  #                                      Ls_seq=Ls_seq, Ls_clear_Sols=Ls_clear_Sols, Ls_dusty_Sols=Ls_dusty_Sols,
  #                                      tau_clear=tau_clear, tau_dusty=tau_dusty,
  #                                      ylim = c(0, 100))
  
  # ---------------------------------------- #
  # 3 - Traverse gains for different SA area #
  # ---------------------------------------- #
  draw_plots = FALSE
  verbose = FALSE

  plot_traverse_distance_gains_for_different_taus(location_id=location_id,
                                                  SA_area_targets=SA_area_targets, SA_area_alts=SA_area_alts,
                                                  phi=phi, beta_max=beta_max, taus=taus,
                                                  draw_plots=draw_plots, verbose=verbose,
                                                  ylim=ylim_traverse_distance_gains)
}

####################
# Common variables #
####################

traverse_Sol_rate = 1/3 # This can be much smaller whith hybrid approach.

# Traverse Sols
Ls_seq = seq(1, 360, 1/traverse_Sol_rate)

# Period withs clear days.
Ls_clear_Sols = c(1:184, 316:360)

# Preiods with dusty days.
Ls_dusty_Sols = 185:315

tau_clear = 0.4
tau_dusty = 1

# Rover performance variables
avg_slippage_rate = 0.15
P_traverse = 75 # Watts
v_rover_nominal = 0.02 # m/s

taus = seq(0.3, 1.7, 0.1)

##############
# IANI CHAOS #
##############
print("IANI CHAOS")

location_id = "IaniChaos"
phi = -2
beta_max = -10

SA_area_targets = c(2.3, 1.5)
SA_area_alts = c(2, 3, 4) # MUST be integers from 1 to 4 due to plot line color logic.

ylim_energy = c(500, 3000)
ylim_traverse_duration = c(0, 15)
ylim_traverse_distance_gains = c(0, 50)

generate_plots(location_id=location_id, phi=phi, beta_max=beta_max,
               SA_area_targets=SA_area_targets, SA_area_alts=SA_area_alts, taus=taus,
               ylim_energy=ylim_energy, ylim_traverse_duration=ylim_traverse_duration,
               ylim_traverse_distance_gains=ylim_traverse_distance_gains)

##################
# ISMENIUS CAVUS #
##################
print("ISMENIUS CAVUS")

location_id = "IsmeniusCavus"
phi = 34
beta_max = 10

SA_area_targets = c(4.1, 2.4)
SA_area_alts = c(2, 3, 4) # MUST be integers from 1 to 4 due to plot line color logic.


ylim_energy = c(500, 3000)
ylim_traverse_duration = c(0, 15)
ylim_traverse_distance_gains = c(0, 35)

# Generate plots. 
generate_plots(location_id=location_id, phi=phi, beta_max=beta_max,
               SA_area_targets=SA_area_targets, SA_area_alts=SA_area_alts, taus=taus,
               ylim_energy=ylim_energy, ylim_traverse_duration=ylim_traverse_duration,
               ylim_traverse_distance_gains=ylim_traverse_distance_gains)
