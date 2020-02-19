# Variation of global, beam, and diffuse insolation on Mars horizontal surface as a function of areocentric longitude at Endeavour Crater.

# Load libraries
library(mars)
library(whisker)
library(RColorBrewer)

source("utils.R")

# Legend labels and colors.
H_eqs_labels = c("Global", "Beam", "Diffuse")

cols = brewer.pal(n=11, name="RdYlBu")
H_eqs_cols = c(
  cols[2], #d73027 - red
  cols[4], #fdae61 - orange,
  cols[9]) #74add1 - blue

# Function parameters.
phi = -2
taus = c(0.5, 1, 2, 3)
Ls_seq = 1:360
al = 0.27 # Mean albedo on Mars is 0.27. Source: NASA-TM-105216. 

ylim = c(100, 5000)

# Plot type options:
#   1 - Line.
#   2 - Stacked bars.
#   3 - Besides bars.
plot_type = 1

tau_index = 1
for(tau in taus){
  
  # Plot title
  title_template = "{{insolation}} as a function of Ls for τ={{tau}}, ϕ={{phi}}°, and albedo {{al}}"
  title_data = list(insolation=if(plot_type == 2) "Hbh and Hdh" else "Hh, Hbh, and Hdh",
                    phi=phi,
                    tau=tau,
                    al=al)
  
  title = whisker.render(title_template, title_data)
  
  # Convenience plot start function.
  plot_start("marsenv", title)
  
  # Empty data matrix that will contain calculate insolation values..
  data_matrix = matrix(NA, nrow=3, ncol=length(Ls_seq))
  
  for(H_index in 1:3){
    for(Ls in Ls_seq){
      
      if(H_index == 1){
        # Global.
        H = H_h(Ls=Ls, phi=phi, longitude=NULL, tau=tau, al=al)
        
      }else if(H_index == 2){
        # Beam.
        H = H_bh(Ls=Ls, phi=phi, tau=tau)
        
      }else if(H_index == 3){
        # Diffuse.
        H = H_dh(Ls=Ls, phi=phi, longitude=NULL, tau=tau, al=al)
        
      }else{
        stop("Unexpected index for insolation formula.")
      }
      
      # Populate data matrix.
      data_matrix[H_index, Ls] = H
    }
  }
  
  beside = if(plot_type == 2) FALSE else TRUE
  
  # For stacked bars, only want to plot beam and diffuse. Not global.
  data = if(isTRUE(beside)) data_matrix else data_matrix[-1,]
  
  # Constrain label and color vectors accordingly
  bar_labels = if(isTRUE(beside)) H_eqs_labels else H_eqs_labels[-1]
  col = if(isTRUE(beside)) H_eqs_cols else H_eqs_cols[-1]
  
  colnames(data) = Ls_seq
  rownames(data) = bar_labels
  
  if(plot_type == 1){
    plot(Ls_seq, data['Global',],
         xlab="Areocentric Longitude, Ls [deg]",
         ylab=paste("Insolation [Wh/m²-deg]", sep=""),
         ylim=ylim,
         type="l",
         col=H_eqs_cols[1],
         font.sub=2,
         cex.sub=1.2,
         lwd=2,
         lty=1)
    
    lines(Ls_seq, data['Beam',], col=H_eqs_cols[2], lwd=2, lty=2)
    lines(Ls_seq, data['Diffuse',], col=H_eqs_cols[3], lwd=2, lty=3)
    
  }else{
    barplot(data, col=col,
            beside=beside,
            xlab="Ls [deg]",
            ylab="Insolation [Wh/m²-deg]",
            xaxt='n',
            las=2,
            font.sub=2,
            cex.sub=1.2)
  }
  
  # Add a legend to last plot
  if(tau_index == 1){
    if(plot_type == 1){
      legend("topleft",
             H_eqs_labels,
             col = H_eqs_cols,
             cex=0.7, lty=c(1,2,3), lwd=1)
    }else{
      legend("topleft",
             if(plot_type == 2) H_eqs_labels[-1] else H_eqs_labels,
             fill = if(plot_type == 2) H_eqs_cols[-1] else H_eqs_cols,
             cex=0.7)
    }       
  }
  
  plot_end()
  tau_index = tau_index + 1

}
