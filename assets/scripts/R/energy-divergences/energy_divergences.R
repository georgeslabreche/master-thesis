# Plot the divergence between predicted and measured energy aswatt-hours and percentage increase/descrease.
#
# Ground truth of Opportunity energy [Wh] was scraped from the rover's status update page: 
#   https://mars.nasa.gov/mer/mission/rover-status/opportunity
#
#
# A. MARS YEAR 33
# ===============
#
# Why is there such high divergence for second part of Mars Year 33?
#
# From Sol 4480 (Ls 213, 30-AUG-2016) to Sol 4671 (Ls 333, 15-MAR-2017), Opportunty was venturing
# further in the rim of Endeavour Crater. See the traverse map below.
#
# Traverse Map: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol4659_1.jpg
#
# Sols 4474 - 4480:
# The rover continued to head towards the gap on Sol 4479 (Aug. 29, 2016), with a 51 feet (15.4-meter) drive.
# Atmospheric opacity (tau) has increased due to regional dust storms. The elevated tau reduces the available solar energy, so the drive was not very long. 
#
# Sols 4481 - 4487:
# Opportunity has officially left Marathon Valley. On Sol 4482 (Sept. 1, 2016), we approached the 'Lewis and Clark Gap' and with the successful drive on Sol 4484 (Sept. 3, 2016)
# we passed through and are now on a course that will take us into Endeavour Crater. Regional dust storms are beginning to occur in our general vicinity leading to higher general atmospheric opacity (tau), but Opportunity has not yet experienced any storm activity directly. 
#
#
# B. OUTLIERS
# ===========
#
# Comments on top 5 outliers (For dust factor adjustment of 0%):
#   1. What happened in Sol 2204 (Ls 74, 6-APR-2010) of Mars Year 29?
#      Huge divergence in energy for Sols 2185,  2199, 2204 (peak), 2211, 2218, and 2226.
#
#     Traverse map:
#       Sol 2199: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2199_1.jpg
#       Sol 2204: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2204_1.jpg
#       Sol 2206: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2206_1.jpg
#
#     Some sort of arc turn. Maybe this arc turn was well synched with the panels.
#
#     "The rover attempted a drive on Sol 2202 (April 4, 2010). That drive stoppedafter the initial arc 
#     turn due to elevated current draw in the motors on the right side of the rover. The rover is between 
#     two ripples with the space in between forming a bowl. The rover had to push harder on the right to
#     make the sharp turn."
#
#     "With everything looking okay, another drive on Sol 2204 (April 6, 2010), was commanded. It too
#     began with a short, sharp arc. This time the drive stopped after a short distance because of wheel
#     slip exceeding the limit of 40 percent. Again ground controllers assessed the conditions and found no
#     problems. With these sharp turns, the rover's wheels must impart more thrust. When the wheel thrust
#     exceeds the shear strength of the terrain, slip occurs."
#
#   2.  What happened in Sol 2519 (Ls 242, 23-FEB-2011) of Mars Year 29? Divergence of -25%.
#
#       Traverse map:
#         Sol 2478: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2478_1.jpg
#         Sol 2519: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2519_1.jpg
#         Sol 2520: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2520_1.jpg
#
#       Occured towards the end of Santa Maria crater exploration:
#
#       "One objective is to position the rover further counter-clockwise around the southeast region of 
#        Santa Maria to collect more wide-baseline stereo imaging of the crater interior."
#
#   3.  Sol 3901 (Ls 271, 13-JAN-2015) of Mars Year 32?
#
#       Traverse Map: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol3908_1.jpg
#
#       Occured while was coming down a summit: 
#
#       "Rover Reaches the Summit of 'Cape Tribulation' After Several Drives this Week.'"
#
#   4.  Sol 3820 (Ls 217, 20-OCT-2014) of Mars Year 32?
#       
#       Traverse Map: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol3825_1.jpg
#
#       On the west rim of Endeavour Crater heading towards "Marathon Valley":
#
#       "Regional dust storms have elevated the atmospheric opacity with tau values 
#        above 2 for several sols." - Calculated with Tau Factor 1.75.
#
#   5.  Sol 3894 (Ls 277, 6-JAN-2015) of Mars Year 32?
#
#       Traverse Map: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol3908_1.jpg#
#
#       "With the drive on Sol 3894 (Jan. 6, 2015), Opportunity is now on the summit of "Cape Tribulation,"
#        the highest point so far on the western rim of Endeavour Crater. This point is 443 feet (135 meters)
#        above the plain of "Botany Bay" before the rover started climbing the rim."
#
#
# C. POSSIBLE DIVERGENCE CAUSES
# =============================
#   - Shadow
#   - Tau factor changes throughout the day (approach of a local storm, e.g. Sol 3820).
#   - Non-uniform dust factor on solar arrays.
#   - Slopes?
#   - Turns
library(here)
library(RColorBrewer)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))


# Function to plot a vector of data into different groups of lines.
source(here("utils", "grouped_lines.R"))

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
source(here("energy-divergences", "get_energy_divergences.R"))

# Which approach to use when calculating adjustements.
# 1 - No adjustments
# 2 - Adjustments with outliers.
# 3 - Adjustments without outliers.
approach = 3
energy_divergences = NULL
title_suffix = ""

if(approach == 1){
  # This key sets the plot image file write directory path.
  PLOT_OUTPUT_KEY = "power_and_energy_predictions"
  
  # Calculate divergences.
  energy_divergences = get_energy_divergences()
  
}else if(approach == 2){
  # This key sets the plot image file write directory path.
  PLOT_OUTPUT_KEY = "narrowed_energy_prediction_error_margin_range"
  
  # Suffix to add to plots using data from this approach.
  title_suffix = " - Adjusted"
  
  # Calculate divergences.
  energy_divergences = get_energy_divergences(
    Loss_shadowing = 0.07,
    DustFactor_adjustment = 0.091
  )
  
}else if(approach == 3){
  # This key sets the plot image file write directory path.
  PLOT_OUTPUT_KEY = "narrowed_energy_prediction_error_margin_range"
  
  # Suffix to add to plots using data from this approach.
  title_suffix = " - Adjusted Without Outliers"
  
  # Calculate divergences.
  energy_divergences = get_energy_divergences(
    Loss_shadowing = 0.05,
    DustFactor_adjustment = 0.054
  )
}


# Get data frame with divergence data.
# Uncomment the function parameters for adjusted energy divergences
# to narrow down error margin range from -33% / +7% to -10% / 25%.


# Create sequence of the Mars Years we have data for.
mars_years = unique(energy_divergences$MarsYear)


# Create legend sequences that will be used in the plots:
#
#   From MY28 to MY34 (i.e. data from Sol 1939 to 5100).
#   From MY28 to MY34, minus MY33.
#
# Why minus MY33? That's the martian year with the most drastis
# divergences due to the rover venturing in the rim of Endeavour Crater.
legend_MY28_to_MY34 = paste("MY", mars_years, sep="")
legend_MY28_to_MY32 = paste("MY", mars_years[!mars_years %in% c(33, 34)], sep="")

# Create color sequence that will be used for the legends.
# Color gradient options: BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn
cols_all = brewer.pal(n=11, name="RdYlBu")[c(5,4,3,1,10,9,7)]
cols_MY28_to_MY32 = brewer.pal(n=11, name="RdYlBu")[c(5,4,3,1,10)]

# Colors for plot comparing predicted vs measured energy.
cols_predicted_vs_measured = brewer.pal(n=11, name="RdYlBu")[c(2,10)]

# Plot divergences between predicted and measured.
# Positives values denote predictions that are greater than what was measured.
# Negative values denote predictions that are lesser than what was measured.
plot_divergences = function(x, y, i, ylab, ylim, ilim=NULL,
                            title=NULL, cols=NULL,
                            legend, legend_position="topleft"){
  
  plot_start(PLOT_OUTPUT_KEY, title)
  
  # Prepare empty plot.
  plot(1,
       xlab="Areocentric Longitude, Ls [deg]",
       ylab=ylab,
       xlim=c(0, 360),
       ylim=ylim,
       type="l")

  # Draw lines.
  grouped_lines(x=x, y=y, x_floor=0, x_ceil=360, i=i, ilim=ilim,
                cols=cols, lwd=1, lty=NULL)
  
  # Draw legend
    legend(legend_position,
           legend=legend,
           col=cols, lty=1:length(legend_MY28_to_MY32), lwd=1, cex=0.7)
  
  plot_end()
}


###############################################
# Plot measured Tau Factor from MY28 to MY34. #
###############################################
# plot_divergences(
#   x=energy_divergences$Ls,
#   y=energy_divergences$TauFactor,
#   i=energy_divergences$Sol,
#   ylim=c(0, 2),
#   ylab="Tau Factor",
#   cols=cols_all,
#   title="Opportunity's Measured Tau Factor",
#   legend=legend_MY28_to_MY34)


############################################################
# Plot measured Solar Array Dust Factor from MY28 to MY34. #
############################################################
# plot_divergences(
#   x=energy_divergences$Ls,
#   y=energy_divergences$SADustFactor,
#   i=energy_divergences$Sol,
#   ylim=c(0.4, 1),
#   ylab="Solar Array Dust Factor",
#   cols=cols_all,
#   title="Opportunity's Measured Solar Array Dust Factor",
#   legend=legend_MY28_to_MY34)

##############################################################
# Plot predicted and measured energy, one plot per Mars Year #
##############################################################

# Remove data from years 28 and 31, not enough data to be interesting.
mars_years_to_plot = mars_years[!mars_years %in% c(28, 31)]

year_index = 1
for (mars_year in mars_years_to_plot) {
  year_energy_divergences = energy_divergences[energy_divergences$MarsYear == mars_year,]

  title = paste("Predicted VS Measured Energy MY", mars_year, title_suffix, sep="")
  plot_start(PLOT_OUTPUT_KEY, title)

  # Measured energy [Wh].
  plot(x=year_energy_divergences$Ls,
       y=year_energy_divergences$WhMeasured,
       xlab="Ls [deg]",
       ylab="Energy [Wh]",
       xlim=c(0, 360),
       ylim=c(200, 800),
       type="l",
       lwd=1,
       col=cols_predicted_vs_measured[1])

  # Predicted energy [Wh].
  lines(x=year_energy_divergences$Ls,
        y=year_energy_divergences$WhPredicted,
        lty=3,
        lwd=1,
        col=cols_predicted_vs_measured[2])

  # Draw legend.
  if(year_index == 1){
    legend("topleft",
           c("Measured", "Predicted"),
           col=cols_predicted_vs_measured,
           cex=1, lty=c(1,3), lwd=1)
  }
  
  plot_end()

  year_index = year_index + 1
}


#########################################################
# Plot predicted and measured energy from MY28 to MY34. #
#########################################################

# # Plot predicted energy [Wh].
# plot_divergences(
#   x=energy_divergences$Ls,
#   y=energy_divergences$WhPredicted,
#   i=energy_divergences$Sol,
#   ylim=c(200, 800),
#   ylab="Measured Energy [Wh]",
#   cols=cols_all,
#   title=paste("Opportunity's Predicted Energy", title_suffix, sep=""),
#   legend=legend_MY28_to_MY34)

# # Plot measured energy [Wh].
# plot_divergences(
#   x=energy_divergences$Ls,
#   y=energy_divergences$WhMeasured,
#   i=energy_divergences$Sol,
#   ylim=c(200, 800),
#   ylab="Measured Energy [Wh]",
#   cols=cols_all,
#   title="Opportunity's Measured Energy",
#   legend=legend_MY28_to_MY34)

#############################################
# Plot energy divergence from MY28 to MY34. #
#############################################
# title = paste("Energy Prediction Divergences from MY28 to MY34", title_suffix, sep="")
#
# # Plot divergence in Wh.
# plot_divergences(
#   x=energy_divergences$Ls,
#   y=energy_divergences$WhDiff,
#   i=energy_divergences$Sol,
#   ylim=c(-100, 220),
#   ylab="Energy Divergence [Wh]",
#   cols=cols_all,
#   title=paste(title, "[Wh]"),
#   legend=legend_MY28_to_MY34)

# # Plot divergence in percentage difference.
# # Negative: over-prediction. Positive: Under-prediction.
# plot_divergences(
#   x=energy_divergences$Ls,
#   y=energy_divergences$WhDiffPercentage,
#   i=energy_divergences$Sol,
#   ylim=c(-60, 10),
#   ylab="Energy Divergence [%]",
#   cols=cols_all,
#   title=paste(title, "[%]"),
#   legend=legend_MY28_to_MY34,
#   legend_position="bottomleft")


##################################################################################
# Plot energy divergence without MY33 and MY34.                                  #
# MY33 - Year with largest divergence, going down crater so on inclined surface. #
# MY34 - Last year, also going down crater.                                      #
##################################################################################
title = paste("Energy Prediction Divergences from MY28 to MY32", title_suffix, sep="")
energy_divergences_MY28_to_MY32 = energy_divergences[energy_divergences$MarsYear != 33 & energy_divergences$MarsYear != 34,]

# Remove the outliers.
if(approach == 3){
  energy_divergences_MY28_to_MY32 = energy_divergences_MY28_to_MY32[energy_divergences_MY28_to_MY32$WhDiffPercentage <= 5 & energy_divergences_MY28_to_MY32$WhDiffPercentage >= -15, ]
}

# # Plot divergence in Wh.
# plot_divergences(
#   x=energy_divergences_MY28_to_MY32$Ls,
#   y=energy_divergences_MY28_to_MY32$WhDiff,
#   i=energy_divergences_MY28_to_MY32$Sol,
#   ylim=c(-50, 110),
#   ylab="Energy Divergence [Wh]",
#   cols=cols_MY28_to_MY32,
#   title=paste(title, "[Wh]"),
#   legend=legend_MY28_to_MY32)

# Plot divergence in percentage difference.
# Negative: over-prediction. Positive: Under-prediction.
plot_divergences(
  x=energy_divergences_MY28_to_MY32$Ls,
  y=energy_divergences_MY28_to_MY32$WhDiffPercentage,
  i=energy_divergences_MY28_to_MY32$Sol,
  ylim=if(approach == 1) ylim=c(-35, 10) else if(approach == 2) c(-10, 25) else c(-17, 10),
  ylab="Energy Divergence [%]",
  cols=cols_MY28_to_MY32,
  title=paste(title, "[%]"),
  legend=legend_MY28_to_MY32,
  legend_position="bottomright")

