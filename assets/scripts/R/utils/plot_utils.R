library(configr)
library(slugify)

config = read.config('config.yml')
global_plot_title = NULL

plot_start = function(output_dir_key, plot_title, mar=NULL, width=NULL, height=NULL, units="in", res=1200, pointsize=8){
  
  # Save plot title into global variable so that it may be re-used in plot_end.
  global_plot_title <<- plot_title
  
  # If writing plot to file, open the png file.
  if(isTRUE(config$write)){
    
    # Some plot title cleansing.
    global_plot_title <<- gsub("τ", "tau", global_plot_title)
    global_plot_title <<- gsub("ϕ", "phi", global_plot_title)
    
    # Create filename based on slugified plot title.
    filename = paste(config$base_path, config$output[output_dir_key], slugify(global_plot_title, space_replace="-"), ".png", sep="")
    
    # Some filename cleansing.
    filename = gsub("--", "-", filename)
    filename = gsub("-.png", ".png", filename)
    
    
    # Now open the png file.
    png(filename,
        width     = ifelse(is.null(width), 3.25, width),
        height    = ifelse(is.null(height), 3.25, height),
        units     = units,
        res       = res,
        pointsize = pointsize)
    
    # Remove some whitespace where title would be rendered.
    # Plots that will be included in a document will have their title in the caption.
    mar = if(is.null(mar)) c(5,4,2,2) else mar
    par(mar=mar+0.1)
    
  }else{
    dev.new()
    # If not writing plot to file, then just open a plotting window.
    if(!is.null(mar)){
      par(mar=mar)
    }
  }
}

plot_end = function(write_to_file=config$write){
  
  # If writing plot to file, close the png file.
  if(isTRUE(write_to_file)){
    dev.off()
    
  }else{
    # Add title to plot if its not being written to file.
    # If being written to file then title will be added in destination doc.
    title(global_plot_title)
  }
}