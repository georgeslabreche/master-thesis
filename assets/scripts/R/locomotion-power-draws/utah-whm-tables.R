library(here)
library(formattable)

# Utils.
source(here("locomotion-power-draws", "utah-utils.R"))

# Selected dataset for analysis.
data_selected = data_list$STEEP_1

# Build power draw data frame.
data_power_draw = build_power_draw_df(data_selected)

# Get power draws maxima, minima, and media indices.
data_indices = inflect(y=data_power_draw$Locomotion, threshold=100)
local_maxima_indices = data_indices$maxima
local_minima_indices = data_indices$minima
local_media_indices = data_indices$media

# Locoal minima, maxima, and media power draws
locomotion_power_draw_minima_df = data.frame(
  "Odometry" = data_power_draw$Odometry[local_minima_indices],
  "Locomotion" =  data_power_draw$Locomotion[local_minima_indices])

locomotion_power_draw_maxima_df = data.frame(
  "Odometry" = data_power_draw$Odometry[local_maxima_indices],
  "Locomotion" =  data_power_draw$Locomotion[local_maxima_indices])

locomotion_power_draw_media_df = data.frame(
  "Odometry" = data_power_draw$Odometry[local_media_indices],
  "Locomotion" =  data_power_draw$Locomotion[local_media_indices])

render_summary_power_draw_table = function(){
  round_num = 0
  
  # Get angles.
  slope_angles = get_slope_angles(data_selected)
  
  power_draw_df = data.frame(
    Node = c("Locomotion (Measured)", "Drive (Measured)", "Suspension (Measured)",
             "Locomotion (Minima)", "Locomotion (Maxima)", "Locomotion (Media)",
             "Slope Angle"),
    Min = round(c(min(data_power_draw$Locomotion), # Measured.
                  min(data_power_draw$Drive),      # Measured.
                  min(data_power_draw$Suspension), # Measured.
                  min(data_power_draw$Locomotion[local_minima_indices]), # Minima.
                  min(data_power_draw$Locomotion[local_maxima_indices]), # Maxima.
                  min(data_power_draw$Locomotion[local_media_indices]),  # Media.
                  min(slope_angles)), round_num),
    Max = round(c(max(data_power_draw$Locomotion), # Measured.
                  max(data_power_draw$Drive),      # Measured.
                  max(data_power_draw$Suspension), # Measured.
                  max(data_power_draw$Locomotion[local_minima_indices]), # Minima.
                  max(data_power_draw$Locomotion[local_maxima_indices]), # Maxima.
                  max(data_power_draw$Locomotion[local_media_indices]), # Media.
                  max(slope_angles)), round_num),
    Mean = round(c(mean(data_power_draw$Locomotion), # Measured.
                   mean(data_power_draw$Drive),      # Measured.
                   mean(data_power_draw$Suspension), # Measured.
                   mean(data_power_draw$Locomotion[local_minima_indices]), # Minima.
                   mean(data_power_draw$Locomotion[local_maxima_indices]), # Maxima.
                   mean(data_power_draw$Locomotion[local_media_indices]), # Media.
                   mean(slope_angles)), round_num))
  
  
  # Print table.
  print(formattable(power_draw_df))
}

render_slope_segment_power_draw_table = function(locomotion_power_draw_df){

  slope_range_power_draws_df = data.frame(
    SlopeRange = c("1 < x <= 3", "3 < x <= 4", "4 < x <= 5", "5 < x <= 6", "6 < x <= 7", "7 < x <= 8", "8 < x <= 9", "9 < x <= 11", "11 < x <= 13", "13 < x <= 14", "14 < x <= 16"),
    Min = round(c(min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 1 & locomotion_power_draw_df$Odometry <= 3,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 3 & locomotion_power_draw_df$Odometry <= 4,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 4 & locomotion_power_draw_df$Odometry <= 5,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 5 & locomotion_power_draw_df$Odometry <= 6,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 6 & locomotion_power_draw_df$Odometry <= 7,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 7 & locomotion_power_draw_df$Odometry <= 8,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 8 & locomotion_power_draw_df$Odometry <= 9,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 9 & locomotion_power_draw_df$Odometry <= 11,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 11 & locomotion_power_draw_df$Odometry <= 13,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 13 & locomotion_power_draw_df$Odometry <= 14,]$Locomotion),
            min(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 14 & locomotion_power_draw_df$Odometry <= 16,]$Locomotion)), 0),
    Max = round(c(max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 1 & locomotion_power_draw_df$Odometry <= 3,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 3 & locomotion_power_draw_df$Odometry <= 4,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 4 & locomotion_power_draw_df$Odometry <= 5,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 5 & locomotion_power_draw_df$Odometry <= 6,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 6 & locomotion_power_draw_df$Odometry <= 7,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 7 & locomotion_power_draw_df$Odometry <= 8,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 8 & locomotion_power_draw_df$Odometry <= 9,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 9 & locomotion_power_draw_df$Odometry <= 11,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 13 & locomotion_power_draw_df$Odometry <= 14,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 14 & locomotion_power_draw_df$Odometry <= 16,]$Locomotion),
            max(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 11 & locomotion_power_draw_df$Odometry <= 13,]$Locomotion)), 0),
    Mean = round(c(mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 1 & locomotion_power_draw_df$Odometry <= 3,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 3 & locomotion_power_draw_df$Odometry <= 4,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 4 & locomotion_power_draw_df$Odometry <= 5,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 5 & locomotion_power_draw_df$Odometry <= 6,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 6 & locomotion_power_draw_df$Odometry <= 7,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 7 & locomotion_power_draw_df$Odometry <= 8,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 8 & locomotion_power_draw_df$Odometry <= 9,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 9 & locomotion_power_draw_df$Odometry <= 11,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 11 & locomotion_power_draw_df$Odometry <= 13,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 13 & locomotion_power_draw_df$Odometry <= 14,]$Locomotion),
             mean(locomotion_power_draw_df[locomotion_power_draw_df$Odometry > 14 & locomotion_power_draw_df$Odometry <= 16,]$Locomotion)), 0))
  
  print(formattable(slope_range_power_draws_df))
}

##################
# Render tables. #
##################
#render_summary_power_draw_table()
render_slope_segment_power_draw_table(locomotion_power_draw_media_df)
