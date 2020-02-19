# Using MER Opportunity data, figure out which dust factor adjustments need to be
# applied for different target error margin bounds.
library(here)
library(whisker)

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
source(here("energy-divergences", "get_energy_divergences.R"))

result_message_template = 'For a {{DustFactor_adjustment}}% Dust Factor adjustment and a {{loss_shadowing}}% Shadowing Loss there are {{out_of_bounds}} out of bound measurements:
> {{upper_bound}}%: {{out_of_upper_bound}}
< {{lower_bound}}%: {{out_of_lower_bound}}
'

target_error_margin_lowest = -0.10
target_error_margin_highest = 0.25

# Remove data points with differences greater than 5% and lesser than -15%, which is less than 10% of the data.
ignore_outliers = TRUE

print(paste("Target error margin range: ", target_error_margin_lowest*100, "% / ", target_error_margin_highest*100, "%.", sep=""))

# Try to fit the results into the target error marging boundaries but trying differ combinations of Shadow/Other Losses and Dust Factor adjusmation coefficient.
for(ls in c(0.04, 0.06, 0.07)){

  prev_out_of_bound_count = NULL
  print(paste("Loss Shadowing set to ", ls*100, "%.", sep=""))
  
  for(dfa in seq(0.01, 0.15, 0.001)){
    
    energy_divergences = get_energy_divergences(
      Loss_shadowing=ls,
      DustFactor_adjustment=dfa)
    
    # Let's drop MY33 and MY34 since it diverges so much compared to other years (because inclination down crater!).
    energy_divergences = energy_divergences[energy_divergences$MarsYear != 33 & energy_divergences$MarsYear != 34,]
    
    # Remove the outliers.
    if(isTRUE(ignore_outliers)){
      energy_divergences = energy_divergences[energy_divergences$WhDiffPercentage <= 5 & energy_divergences$WhDiffPercentage >= -15, ]
    }
    
    # Count how many measurements are outside of the target margin of error range.
    divs_out_of_upper_bound = energy_divergences[energy_divergences$WhDiffPercentage > target_error_margin_highest*100, ]
    divs_out_of_lower_bound = energy_divergences[energy_divergences$WhDiffPercentage < target_error_margin_lowest*100, ]
    
    out_of_bounds = length(divs_out_of_upper_bound$Ls) + length(divs_out_of_lower_bound$Ls)
    
    if(!is.null(prev_out_of_bound_count)){
      if(out_of_bounds > prev_out_of_bound_count){
        print(paste("No parameters found to narrow error marging. Stopped at Dust Factor adjustment ", dfa*100, "% and Shadowing Loss ", ls*100, "%", sep=""))
        break # Move to next Shadowing Loss.
      }else{
        # Set prev_out_of_bound_count for next iteration.
        prev_out_of_bound_count = out_of_bounds
      }
    }else{
      # First time setting prev_out_of_bound_count.
      prev_out_of_bound_count = out_of_bounds
    }
    
    
    # Organiye results into an object
    results = list(
      DustFactor_adjustment=dfa*100,
      loss_shadowing = ls*100,
      upper_bound = target_error_margin_highest*100,
      lower_bound = target_error_margin_lowest*100,
      out_of_bounds = out_of_bounds,
      out_of_upper_bound = length(divs_out_of_upper_bound$Ls),
      out_of_lower_bound =  length(divs_out_of_lower_bound$Ls)
    )
    
    if(results$out_of_bounds == 0){
      # Print result.
      cat(whisker.render(result_message_template, results))
      break # Move to next Shadowing Loss.
    }
    
  }
}