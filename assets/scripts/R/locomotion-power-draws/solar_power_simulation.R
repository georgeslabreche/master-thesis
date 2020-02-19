# Reference for yaw, pitch, roll calculations.
# http://students.iitk.ac.in/roboclub/2017/12/21/Beginners-Guide-to-IMU.html

# Load library.
library(here)

# Utils.
source(here("locomotion-power-draws", "utah-utils.R"))

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "simulation"

  
# Get ground truth data.
df = data_list$STEEP_3

# Calculate yaw, pitch, and roll angles from IMU data.
attitudes = get_attitude(
  acc_X=df$imu_acc_x,
  acc_Y=df$imu_acc_y,
  acc_Z=df$imu_acc_z,
  mag_X=df$imu_mag_x,
  mag_Y=df$imu_mag_y)

# Plot.
plot_title = "SherpaTT Utah Attitude"
plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title)

plot(x=df$time,
     y=attitudes$pitch,
     type='l', col='red', lwd=3)

plot_end()