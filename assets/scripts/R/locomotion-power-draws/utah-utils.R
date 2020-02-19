# Load libraries.
library(here)
library(formattable)
library(RColorBrewer)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "locomotion_power_draws"

# Colors for plotting.
cols = brewer.pal(n=11, name="RdYlBu")
colors = list(
  measured = cols[7], #e0f3f8
  minima = cols[4], #fdae61
  maxima = cols[1], #a50026
  media = 'black',
  locomotion = cols[1], #a50026
  drive = cols[4], #fdae61
  suspension = cols[9], #74add1
  slope_angle = 'black'
)

data_dir = "locomotion-power-draws/data"

# Load data if it hasn't been done already-
if(exists('data_flat_terrain_1') == FALSE){
  filepath_flat_terrain_1 = here(data_dir, 'flatTerrain_01-withGPS.csv')
  data_flat_terrain_1 = read.csv(filepath_flat_terrain_1, header=TRUE)
  
  # Drop data pertaining to initial spike on motor start.
  data_flat_terrain_1 = data_flat_terrain_1[data_flat_terrain_1$odoPos_x >= 1 & data_flat_terrain_1$odoPos_x <= 20,]
}

if(exists('data_flat_terrain_2') == FALSE){
  filepath_flat_terrain_2 = here(data_dir, 'flatTerrain_02-withGPS.csv')
  data_flat_terrain_2 = read.csv(filepath_flat_terrain_2, header=TRUE)
  data_flat_terrain_2$odoPos_x = -data_flat_terrain_2$odoPos_x
  
  # Drop data pertaining to initial spike on motor start.
  data_flat_terrain_2 = data_flat_terrain_2[data_flat_terrain_2$odoPos_x >= 1 & data_flat_terrain_2$odoPos_x <= 20,]
}

if(exists('data_steep_terrain_1') == FALSE){
  filepath_steep_terrain_1 = here(data_dir, 'steepTerrain_01-withGPS.csv')
  data_steep_terrain_1 = read.csv(filepath_steep_terrain_1, header=TRUE)
  
  # Drop data pertaining to initial spike on motor start.
  data_steep_terrain_1 = data_steep_terrain_1[data_steep_terrain_1$odoPos_x >= 1,]
}

if(exists('data_steep_terrain_2') == FALSE){
  filepath_steep_terrain_2 = here(data_dir, 'steepTerrain_02-withGPS.csv')
  data_steep_terrain_2 = read.csv(filepath_steep_terrain_2, header=TRUE)
  
  # Drop data pertaining to initial spike on motor start.
  data_steep_terrain_2 = data_steep_terrain_2[data_steep_terrain_2$odoPos_x >= 1,]
}

if(exists('data_steep_terrain_3') == FALSE){
  filepath_steep_terrain_3 = here(data_dir, 'steepTerrain_03_heavySlip-withGPS.csv')
  data_steep_terrain_3 = read.csv(filepath_steep_terrain_3, header=TRUE)
  
  # Drop data pertaining to initial spike on motor start.
  data_steep_terrain_3 = data_steep_terrain_3[data_steep_terrain_3$odoPos_x >= 1,]
  
}

# List of available datasets
data_list = list(
  FLAT_1 = data_flat_terrain_1,
  FLAT_2 = data_flat_terrain_2,
  STEEP_1 = data_steep_terrain_1,
  STEEP_2 = data_steep_terrain_2,
  STEEP_3 = data_steep_terrain_3
)

get_attitude = function(acc_X, acc_Y, acc_Z, mag_X, mag_Y){
  # yaw = atan(acc_Z / sqrt(acc_X^2 + acc_Z^2))
  attitude = list(
    yaw = (180/pi) * atan2(-mag_Y, mag_X),
    pitch = (180/pi) * atan2(acc_X, sqrt(acc_Y^2 + acc_Z^2)),
    roll = (180/pi) * atan2(acc_Y, sqrt(acc_X^2 + acc_Z^2)))
  
  return(attitude)
}

# This function gets the slop angles values associated with each measurement.
# Slope angle values are based on Table 5: Slope angles for Steep-Slope tests
# from Cordes et al. 2018.
get_slope_angles = function(df, body_pitch_angle=FALSE){
  
  if(isTRUE(body_pitch_angle)){
    attitudes = get_attitude(df$imu_acc_x, df$imu_acc_y, df$imu_acc_z, df$imu_mag_x, df$imu_mag_y)
    return(attitudes$pitch)
  }else{
    slope_angles = c()
    
    for(odoPos_x in df$odoPos_x){
      
      if(odoPos_x <= 1){
        slope_angles = c(slope_angles, 9.5)
        
      }else if(odoPos_x <= 3){
        slope_angles = c(slope_angles, 10)
        
      }else if(odoPos_x <= 4){
        slope_angles = c(slope_angles, 11)
        
      }else if(odoPos_x <= 5){
        slope_angles = c(slope_angles, 15)
        
      }else if(odoPos_x <= 6){
        slope_angles = c(slope_angles, 16)
        
      }else if(odoPos_x <= 7){
        slope_angles = c(slope_angles, 28)
        
      }else if(odoPos_x <= 8){
        slope_angles = c(slope_angles, 22)
        
      }else if(odoPos_x <= 9){
        slope_angles = c(slope_angles, 25)
        
      }else if(odoPos_x <= 11){
        slope_angles = c(slope_angles, 28)
        
      }else if(odoPos_x <= 13){
        slope_angles = c(slope_angles, 20)
        
      }else if(odoPos_x <= 14){
        slope_angles = c(slope_angles, 15)
        
      }else if(odoPos_x <= 16){
        slope_angles = c(slope_angles, 10)
        
      }else{
        slope_angles = c(slope_angles, 0)
      }    
    }
    
    return(slope_angles)    
  }
}

# List of suspension motor column names.
joints_suspension = list(
  Pan='Pan',
  InnerLeg='IL', # Inner Leg
  OuterLeg='OL') # Outer Let

# List of drive motor column names.
joints_drive = list(
  WheelSteering='WS', # Wheel Steering
  WheelDrive='WD' # Wheel Driving.
)

# All motor column names.
joints_locomotion = c(joints_suspension, joints_drive)

legs = list(
  FrontLeft='fl',
  FrontRight='fr',
  RearLeft='rl',
  RearRight='rr')


get_propulsion_nodes = function(measurement, joints){
  nodes = c()
  for(joint in joints){
    for(leg in legs){
      node_id = paste(measurement, '_', joint, '_', leg, sep='')
      nodes = c(nodes, node_id)
    }
  }
  
  return(nodes)  
}

get_suspension_nodes = function(measurement){
  get_propulsion_nodes(measurement, joints_suspension)
}

get_drive_nodes = function(measurement){
  get_propulsion_nodes(measurement, joints_drive)
}

get_locomotion_nodes = function(measurement){
  get_propulsion_nodes(measurement, joints_locomotion)
}


get_leg_propulsion_nodes = function(measurement, leg, joints){
  nodes = c()
  for(joint in joints){
    node_id = paste(measurement, '_', joint, '_', leg, sep='')
    nodes = c(nodes, node_id)
  }
  return(nodes)
}

get_leg_suspension_nodes = function(measurement, leg){
  get_leg_propulsion_nodes(measurement, leg, joints_suspension)
}

get_leg_drive_nodes = function(measurement, leg){
  get_leg_propulsion_nodes(measurement, leg, joints_drive)
}

get_leg_locomotion_nodes = function(measurement, leg){
  get_leg_propulsion_nodes(measurement, leg, joints_locomotion)
}

get_leg_power_nodes = function(leg_location){
  get_leg_locomotion_nodes('PWM', leg_location)
}

get_leg_current_nodes = function(leg_location){
  get_leg_locomotion_nodes('current', leg_location)
}

# This function build the dataframe containing all required power data.
build_power_draw_df = function(df){
  row_count = nrow(df)
  
  power_draw_df = data.frame(
    'Odometry' = numeric(row_count),
    'Locomotion' = numeric(row_count),
    'Suspension' = numeric(row_count),
    'Drive' = numeric(row_count),
    'Pan' = numeric(row_count),
    'IL' = numeric(row_count),
    'OL' = numeric(row_count),
    'WS' = numeric(row_count),
    'WD' = numeric(row_count))
  
  for(joint in joints_locomotion){
    for(leg in legs){
      pwd_col_name = paste('PWM_', joint, '_', leg, sep='')
      current_col_name = paste('current_', joint, '_', leg, sep='')
      
      power_draw_df[joint] = power_draw_df[joint] + (48 * abs(df[pwd_col_name]) * abs(df[current_col_name]))
    }
  }
  
  power_draw_df$Drive = power_draw_df$WS + power_draw_df$WD
  power_draw_df$Suspension = power_draw_df$Pan + power_draw_df$IL + power_draw_df$OL
  power_draw_df$Locomotion = power_draw_df$Drive + power_draw_df$Suspension
  
  power_draw_df$Odometry = df$odoPos_x
  
  return(power_draw_df)
}

# Get the local minima, maxima, and media of a given vector of values.
# This function is a modified verion of the one found here: https://stackoverflow.com/a/43061365
# The modification consists of including media.
inflect = function(y, threshold = 1){
  up   = sapply(1:threshold, function(n) c(y[-(seq(n))], rep(NA, n)))
  down = sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), y[-seq(length(y), length(y) - abs(n) + 1)]))
  a    = cbind(y,up,down)
  list(minima = which(apply(a, 1, min) == a[,1]),
       maxima = which(apply(a, 1, max) == a[,1]),
       media = which(apply(a, 1, median) == a[,1]))
}


