# Function to plot variation of global, beam, and diffuse irradiance on Mars horizontal surface.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
library(RColorBrewer)

I_eqs_labels = c("Global insolation", "Beam insolation", "Diffuse insolation")

# Color blind friendly colors.
cols = brewer.pal(n=11, name="RdYlBu")
I_eqs_cols = c(
  cols[2], #d73027 - red
  cols[4], #fdae61 - orange,
  cols[9]) #74add1 - blue

# TODO: Parametize cols
diurnal_line = function(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, pch=3, lwd=3, new_plot, cols=I_eqs_cols){
  # Label the unit correctly based on the desired insolation time range.
  ylab_h = if(T_step==1) "" else T_step
  
  # Plot
  if(isTRUE(new_plot)){
    plot(if(isTRUE(points)) Ts+T_step else NULL,
         if(isTRUE(points)) I_seq else NULL,
         xlab="Solar Time Range [h]", ylab=paste("Insolation [Wh/m²", "-", ylab_h, "h]", sep=""),
         xlim=xlim, ylim=ylim,
         pch=pch,
         lwd=lwd,
         col=cols[I_index],
         sub=sub,
         font.sub=2,
         cex.sub=1.2)
  }
  
  if(isTRUE(smooth)){
    smooth_line = smooth.spline(Ts+T_step, I_seq, spar=0.35)
    lines(smooth_line, col=cols[I_index], lwd=lwd)
    
  }else{
    lines(Ts+T_step, I_seq, col=cols[I_index], lwd=lwd)
  }
  
  if(!isTRUE(new_plot)){
    points(if(isTRUE(points)) Ts+T_step else NULL, 
           if(isTRUE(points)) I_seq else NULL, 
           pch=pch,
           col=I_eqs_cols[I_index])
  }
}

plot_diurnal_line = function(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, pch=3, lwd=3, cols=I_eqs_cols){
  if(length(Ts) == length(I_seq)){
    if(I_index == 1){
      
      # New Plot
      diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, pch, lwd, TRUE, cols)
      
    }else{
      # Make sure we are plotting on a new plot when needed.
      tryCatch({
        diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, pch, lwd, FALSE, cols)
      },
      warning = function(w) {
        # Do nothing
      },
      error = function(e) {
        # Enter here when following error occurs: plot.new has not been called yet.
        # Plot
        diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, pch, lwd, TRUE, cols)
      },
      finally = {
        # Do nothing
      })
    }
      
    
  }else{
    # Using the f_89 or f_90 lookup table based implementations of the net flux function may
    # result feeding that function a rounded Z parameter that does not exist in the lookup tables.
    # This will result in an error which we are handling here by not plotting the affected irradiance type.
    message(paste("Could not calculate ", I_eqs_labels[I_index] , " for latitude ϕ=", phi, "°.", sep=""))
  }
}

plot_diurnal_stacked_bars = function(data_matrix, T_step, sub, ylim, x_labels, beside, lwd=1, cols=I_eqs_cols, global_only=global_only){
  
  # For stacked bars, only want to plot beam and diffuse. Not global.
  data = if(isTRUE(beside)) data_matrix else data_matrix[-1,]
  
  # Constrain label and color vectors accordingly
  if(isTRUE(global_only)){
    col = cols[1]
  }else{
    col = if(isTRUE(beside)) cols[1:3] else cols[2:3]
  }
 
  
  colnames(data) = x_labels
  #rownames(data) = bar_labels
  
  # There is no need to plot zero insolation.
  # Filter out data points where global insolation is 0.
  if(isTRUE(beside)){
    data = data[, data[1,] > 0]
  }else{
    data = data[, data[1,] + data[2,] > 0]
  }
  
  # Plot
  # Label the unit correctly based on the desired insolation time range.
  ylab_h = if(T_step==1) "" else T_step
  
  barplot(data, col=col,
          beside=beside,
          ylim=ylim,
          xlab="Solar Time Range [h]",
          ylab=paste("Insolation [Wh/m²", "-", ylab_h, "h]", sep=""),
          lwd=lwd,
          las=2,
          sub=sub,
          font.sub=2,
          cex.sub=1.2)
}

# plot_type options:
#   1 - Line.
#   2 - Stacked bars.
#   3 - Besides bars.
diurnal_insolation_plot = function(Ls, phi, tau, al, T_begin=0, T_finish=24, T_step=1, sub="", xlim=c(0, 24), ylim, points=TRUE, smooth=TRUE,  pch=3, lwd=NULL, plot_type, cols=I_eqs_cols, global_only=FALSE){
  # Empty data matrix that will contain calculate insolation values..
  
  data_matrix = NULL
  
  # If we only want data from global beam.
  if(isTRUE(global_only)){
    data_matrix = matrix(NA, nrow = 1, ncol = (T_finish-T_begin)/T_step)
  }
  # If we want data from all beams.
  else{
    data_matrix = matrix(NA, nrow = 3, ncol = (T_finish-T_begin)/T_step)
  }
  
  
  for(I_index in 1:nrow(data_matrix)){

    # Calculate the insolation for each given parameters.
    # Store the results in a sequence that will be used in the plot() function.
    Ts = seq(T_begin, T_finish-T_step, T_step)
    x_labels = c()
    T_range_index = 1
    
    for(T_start in Ts){
      T_end = T_start + T_step
      
      x_labels = c(x_labels, paste(T_start, "-", T_end, sep=""))
      
      if(I_index == 1){
        # Global insolation
        I = I_h(Ls=Ls, phi=phi, longitude=NULL, tau=tau, Ts_start=T_start, Ts_end=T_end, al=al)
        
      }else if(I_index == 2){
        # Beam insolation
        I = I_bh(Ls=Ls, phi=phi, tau=tau, Ts_start=T_start, Ts_end=T_end)
        
      }else if(I_index == 3){
        # Diffuse insolation
        I = I_dh(Ls=Ls, phi=phi, longitude=NULL, tau=tau, Ts_start=T_start, Ts_end=T_end, al=al)
        
      }else{
        stop("Unexpected index for insolation formula.")
      }

      # Populate data matrix.
      data_matrix[I_index, T_range_index] = I
      
      T_range_index = T_range_index + 1
    }
    
    # Plot
    if(plot_type == 1){
      I_seq = data_matrix[I_index,]
      plot_diurnal_line(Ts, I_seq, T_step, I_index, sub, xlim, ylim, points, smooth, pch, lwd=ifelse(is.null(lwd), 3, lwd), cols=cols)
    }
  }
  
  if(plot_type == 2){
    plot_diurnal_stacked_bars(data_matrix, T_step, sub, ylim, x_labels, FALSE, lwd=ifelse(is.null(lwd), 1, lwd), cols=cols, global_only=global_only)
    
  }else if(plot_type == 3){
    plot_diurnal_stacked_bars(data_matrix, T_step, sub, ylim, x_labels, TRUE, lwd=ifelse(is.null(lwd), 1, lwd), cols=cols, global_only=global_only)
  }
} 