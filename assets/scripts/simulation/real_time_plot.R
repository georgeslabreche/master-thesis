xmin = 0
xmax = 500

dev.new()
while(TRUE) {
  # Data is always updated so always reload the file.
  df = read.csv(file = '/home/dfki.uni-bremen.de/glabreche/Dev/ade_dev/bundles/sherpa_tt/scripts/power/data.csv')
          
  flush.console()
  
  # Update xlim.
  if(max(df$step) >= xmax){
    xmin = max(df$step)-xmax
  }
  xlim = c(xmin, max(df$step))

  
  plot(df$step, round(df$power, 2), type='l', xlim=xlim, ylim=c(140, 150), lwd=3, col='red',
       xlab="Elapsed Time [s]", ylab="Generated Power [W]")

  Sys.sleep(.05)
}