# Load library.
library(here)

# Utils.
source(here("locomotion-power-draws", "utah-utils.R"))

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "locomotion_power_draws"


#######################################################
# Plot local minima, maxima, and media of power draw. #
#######################################################
plot_maxima_and_minima = function(x, y, threshold, title='', include_legend=TRUE, ylim=c(0, 220), xtick=1:16){
  data_indices = inflect(y, threshold=threshold)
  local_maxima_indices = data_indices$maxima
  local_minima_indices = data_indices$minima
  local_media_indices = data_indices$media
  
  # Plot start convenience function.
  plot_start(PLOT_OUTPUT_KEY, plot_title=title)
  
  # Measured power draw line.
  plot(x=x,
       y=y,
       xaxt='n',
       xlab='Odometry Distance [m]',
       ylab='Power [W]',
       ylim=ylim,
       type='l', col=colors$measured, lwd=1)
  
  axis(side=1, at=xtick, labels=xtick)
  
  # Local minima power draw line
  lines(x=x[local_minima_indices], 
        y=y[local_minima_indices],
        col=colors$minima, lwd=1, lty=2)
  
  # Local maxima power draw line.
  lines(x=x[local_maxima_indices],
        y=y[local_maxima_indices],
        col=colors$maxima, lwd=1, lty=6)
  
  # Media power draw line.
  lines(x=x[local_media_indices], 
        y=y[local_media_indices],
        col=colors$media, lwd=1, lty=3)
  
  if(isTRUE(include_legend)){
    legend('topright', legend=c('Measured', 'Local Minima', 'Local Maxima', 'Media'),
           col=c(colors$measured, colors$minima, colors$maxima, colors$media),
           lty=c(1, 2, 6, 3), lwd=c(1, 1, 1, 1), cex=0.7)
  }
  
  plot_end()
}

plot_power_draws_with_slope_angles = function(data_selected, data_power_draw, plot_minima_locomotion_power_draw = TRUE){
  # We want to a plot with 2 x-axes (power draw and slope angle).
  
  if(isTRUE(plot_minima_locomotion_power_draw)){
    plot_title ='Minima Locomotion Power Draws on Upslope Terrain'
  }else{
    plot_title ='Power Draws on Upslope Terrain'
  }
  
  plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title, mar=c(5, 4, 4, 4) + 0.3, height=4, width=9)
  
  if(isTRUE(plot_minima_locomotion_power_draw)){
    data_indices = inflect(y=data_power_draw$Locomotion, threshold=100)
    local_minima_indices = data_indices$minima
    
    plot(x=data_selected$odoPos_x[local_minima_indices],
         y=data_power_draw$Locomotion[local_minima_indices],
         xaxt='n',
         xlab='Odometry Distance [m]',
         ylab='Power [W]',
         ylim=c(0, 200),
         type='l', col=colors$locomotion, lwd=2)
    
  }else{
    # Plot locomotion power draw.
    plot(x=data_selected$odoPos_x,
         y=data_power_draw$Locomotion,
         xaxt='n',
         xlab='Odometry Distance [m]',
         ylab='Power [W]',
         ylim=c(0, 250),
         type='l', col=colors$locomotion, lwd=1)
    
    # Plot drive power draw.
    lines(x=data_selected$odoPos_x,
          y=data_power_draw$Drive,
          col=colors$drive, lwd=1)
    
    # Plot Suspension power draw.
    lines(x=data_selected$odoPos_x,
          y=data_power_draw$Suspension,
          col=colors$suspension, lwd=1)
  }
  
  # Customize x-axis.
  xtick = 1:16
  axis(side=1, at=xtick, labels=xtick)
  
  # Customize y-axis.
  ytick = seq(0, 250, 50)
  axis(side=2, at=ytick, labels=ytick)
  
  # Allow a second plot on the same graph.
  # This is for the second x-axis (slope angle).
  par(new=TRUE)
  plot(x=data_selected$odoPos_x, y=get_slope_angles(data_selected),
       xlab="", ylab="", ylim=c(0,30),
       axes=FALSE, type='l', col=colors$slope_angle,
       lwd=2, lty=3)
  
  mtext("Slope Angle [deg]", side=4, padj=3.5)
  axis(4, ylim=c(0,30), las=1)
  
  if(isTRUE(plot_minima_locomotion_power_draw)){
    legend('topright', legend=c('Power Draw', 'Slope Angle'),
           col=c(colors$locomotion, colors$slope_angle), lty=c(1,3), cex=0.8, lwd=1)
  }else{
    # Legend - Power Draw.
    legend('topleft',
           title='Power Draw',
           legend=c('Locomotion', 'Drive', 'Suspension'),
           col=c(colors$locomotion, colors$drive, colors$suspension),
           lty=1, lwd=1, cex=0.8)
    
    # Legend - Slope Angles.
    legend('topright', legend=c('Slope Angle'),
           col=colors$slope_angle, lty=3, cex=0.8, lwd=1)
  }
  
  # Write out the plot.
  plot_end()  
}

# Build power draw data frame.
data_power_draw_flat_1 = build_power_draw_df(data_list$FLAT_1)
data_power_draw_flat_2 = build_power_draw_df(data_list$FLAT_2)
data_power_draw_steep_1 = build_power_draw_df(data_list$STEEP_1)

#############################
# Plot all power draw data. #
#############################
# dev.new()
# plot.ts(data_power_draw_steep_1, col='grey')

# ############################################################
# # Plot SherpaTT Power draws - Measurements for STEEP 1 run #
# ############################################################
# Locomotion.
plot_maxima_and_minima(x=data_list$STEEP_1$odoPos_x, y=data_power_draw_steep_1$Locomotion, threshold=100,
                       title='Locomotion Power Draw on Upslope Terrain', include_legend=FALSE)

# Drive.
plot_maxima_and_minima(x=data_list$STEEP_1$odoPos_x, y=data_power_draw_steep_1$Drive, threshold=100,
                       title='Drive Power Draw on Upslope Terrain', include_legend=FALSE)

# Suspension.
plot_maxima_and_minima(x=data_list$STEEP_1$odoPos_x, y=data_power_draw_steep_1$Suspension, threshold=100,
                       title='Suspension Power Draw on Upslope Terrain', include_legend=TRUE)


# #######################################################################
# # Plot SherpaTT Power draws - Measurements for FLAT 1 and FLAT 2 runs #
# #######################################################################
# Locomotion.
plot_maxima_and_minima(x=data_list$FLAT_1$odoPos_x, y=data_power_draw_flat_1$Locomotion, threshold=100,
                       title='Locomotion Power Draw on Flat Terrain 1', include_legend=FALSE,
                       ylim=c(15, 120), xtick=1:20)

# Locomotion.
plot_maxima_and_minima(x=data_list$FLAT_2$odoPos_x, y=data_power_draw_flat_2$Locomotion, threshold=100,
                       title='Locomotion Power Draw on Flat Terrain 2', include_legend=TRUE,
                       ylim=c(15, 120), xtick=1:20)

###########################################################
# Plot suspension, drive, and locomotion power draw data. #
# Include slope angles.                                   #
###########################################################
plot_power_draws_with_slope_angles(data_selected=data_list$STEEP_1, data_power_draw=data_power_draw_steep_1,
                                   plot_minima_locomotion_power_draw=TRUE)

# plot_power_draws_with_slope_angles(data_selected=data_list$STEEP_1, data_power_draw=data_power_draw_steep_1,
#                                    plot_minima_locomotion_power_draw=FALSE)


