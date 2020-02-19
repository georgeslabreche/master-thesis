##################################################
# Binning divergence percentages into 5% ranges. #
##################################################

# Load libraries.
library(here)
library(RColorBrewer)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "requirements_and_design_drivers"

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
source(here("energy-divergences", "get_energy_divergences.R"))

# Plot color.
cols = brewer.pal(n=11, name="RdYlBu")[c(6,5,6,4,3,2,1,3,4)]

# Get divergences for all Martian years except MY33 and MY34.
energy_divergences = get_energy_divergences()
energy_divergences = energy_divergences[energy_divergences$MarsYear != 33 & energy_divergences$MarsYear != 34,]

# To verify that datasets are not overlapping.
measurement_count = length(energy_divergences$Sol)
total_grouped_count = 0

floor = -35
ceiling = 10
step = 5

# Collect data for the barplot.
percentages = c()
counts = c()
xlabels = c()

count = length(energy_divergences[energy_divergences$WhDiffPercentage < floor,]$Sol)
print(paste("Less than ", floor, "%: ", count, " (", round((count/ measurement_count)*100, 1), "%)", sep=""))

total_grouped_count = count

for(lower in seq(floor, ceiling-step, step)){
  upper = lower + step
  
  count = length(energy_divergences[energy_divergences$WhDiffPercentage > lower & energy_divergences$WhDiffPercentage < upper,]$Sol)
  counts = c(counts, count)
  
  percentage = round((count/ measurement_count)*100, 1)
  percentages = c(percentages, percentage)
  
  label = paste(lower, "% / ", upper, "%", sep="")
  xlabels = c(xlabels, label)
  
  print(paste(label, ": ", count, " (", percentage, "%)", sep=""))
  
  total_grouped_count = total_grouped_count + count
}

count = length(energy_divergences[energy_divergences$WhDiffPercentage > ceiling,]$Sol)
print(paste("Greater than ", ceiling, "%: ", count, " (", round((count/ measurement_count)*100, 1), "%)", sep=""))

total_grouped_count = total_grouped_count + count

if(total_grouped_count == measurement_count){
  print("No data overlaps.")
  
  # Plot the bar chart 
  plot_start(PLOT_OUTPUT_KEY, plot_title="Binned Error Margins", mar=c(5,6,2,2))

  xx = barplot(percentages, names.arg=xlabels,
          xlab="Share of Total Measurements [%]", ylab="",
          col=cols, #border=cols,
          horiz=TRUE, las=1, cex.axis=0.8, cex.names=0.8,
          xlim=c(0, 45))
  
  text(x=percentages, y=xx, 
       label=paste(percentages, '%', sep=''),
       pos=4, cex=1)
  
  plot_end()
  
}else{
  print(paste("Warning, ", total_grouped_count - measurement_count, " measurements overlap.", sep=""))
}

# Print divergences that go beyond -20%.
print(energy_divergences[energy_divergences$WhDiffPercentage <= -20, ])

# Print local maxima outliers.
print(energy_divergences[energy_divergences$Sol %in% c(2204, 2218, 2519, 3901), ])


