library(mars)
library(here)

data_dir = "data/mission-sites"

# Cache for traverse power budgets.
# So we don't have to rebuild the dataframe every time (it takes a while).
# IMPORTANT: Environment needs to be cleared if these power budgets need to be recalculated.
if(!exists("PB_CACHE")){
  PB_CACHE = list()
}

ROIs = list(
  "IaniChaos" = list(
    "phi" = -2,
    "longitude" = -17,
    "beta" = -10 # Negative because in Southern hemisphere.
  ), # https://trek.nasa.gov/mars/#v=0.1&x=-16.962890308581592&y=-2.10937496065263&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
  "IsmeniusCavus" = list(
    "phi" = 34,
    "longitude" = 17,
    "beta" = 10
  ) # https://trek.nasa.gov/mars/#v=0.1&x=17.138671555302636&y=33.92578061716317&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
)

# Get worst and best case insolation from cache or calculate them.
cache_fetch_worst_and_best_case_insolations = function(Ls_seq, phi, longitude, beta, taus, location_id=NULL){
  
  H_df = NULL
  if(!is.null(location_id)){
    cache_id = paste(location_id, tau, "_beta", beta, sep="")
    
    if(is.null(PB_CACHE[[cache_id]])){
      H_df = get_worst_and_best_case_insolations(Ls_seq=Ls_seq, phi=phi, longitude=longitude, beta=beta, taus=taus)
      
      # Cache the built dataframe.
      PB_CACHE[[cache_id]] <<- H_df
      
    }else{
      H_df = PB_CACHE[[cache_id]]
    }
  }else{
    H_df = get_worst_and_best_case_insolations(Ls_seq=Ls_seq, phi=phi, longitude=longitude, beta=beta, taus=taus)
  }
  
  return(H_df)
}


get_daily_insolation_lookup_table = function(location_id, tau, beta_max, Ls_filter=NULL){
  csv_filepath = here(data_dir, paste(location_id, "/", "Hbest_tau", tau, "_betamax", beta_max, ".csv", sep=""))
  
  # If lookup table file does not exist, return NULL.
  if(!file.exists(csv_filepath)){
    return(NULL)
    
  }else{
    H_df = read.csv(csv_filepath, header=TRUE)
    
    if(is.null(Ls_filter)){
      return(H_df)
      
    }else{
      return(H_df[H_df$Ls %in% Ls_filter,])
    }
  }
}

# Should rename to calculate_worst_and_best_case_daily_insolations.
get_worst_and_best_case_insolations = function(Ls_seq, phi, longitude, beta, day_type, taus){  
  
  # Dataframe that will contain results.
  results_df = data.frame(
    "tau" = rep(NA, length(taus)),
    "Ls_worst" = rep(NA, length(taus)),
    "Hh_worst" = rep(NA, length(taus)),
    "Hi_worst" = rep(NA, length(taus)),
    "gammaBest_forLsWorst" = rep(NA, length(taus)),
    "H_w_pc_inc" = rep(NA, length(taus)),
    "Ls_best" = rep(NA, length(taus)),
    "Hh_best" = rep(NA, length(taus)),
    "Hi_best" = rep(NA, length(taus)),
    "gammaBest_forLsBest" = rep(NA, length(taus)),
    "H_b_pc_inc" = rep(NA, length(taus))
  )
  
  tau_index = 1
  for(tau in taus){
    print(paste("Processing tau = ", tau, "." , sep=""))
    
    results_df$tau[tau_index] = tau
    
    # Get worst and best Ls with respect to daily insolation on a horizontal surface.
    Hh_LsWorst = Inf
    Hh_LsBest = 0
    
    for(Ls in Ls_seq){
      
      # Daily insolation on a horizontal surface.
      Hh = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=0, gamma_c=0)
      
      if(Hh < Hh_LsWorst){
        Hh_LsWorst = Hh
        
        results_df$Ls_worst[tau_index] = Ls
        results_df$Hh_worst[tau_index] = round(Hh_LsWorst)
      }
      
      if(Hh > Hh_LsBest){
        Hh_LsBest = Hh
        
        results_df$Ls_best[tau_index] = Ls
        results_df$Hh_best[tau_index] = round(Hh_LsBest)
      }
    }
    
    # Get best case angles for Ls_worst and Ls_best.
    # Best case surface inclination will be the maximum at beta = 10 deg.
    # For this inclination, figure out the best case orientation.
    Hi_LsWorst_AnglesBest = 0
    Hi_LsBest_AnglesBest = 0
    
    for(gamma_c in -179:180){
      
      # Hi with best orientation angle gamma_c for worst Ls.
      Hi_LsWorst = H_i(Ls=results_df$Ls_worst[tau_index], phi=phi, longitude=longitude, tau=tau,
                       beta=beta,
                       gamma_c=gamma_c)
      
      
      if(Hi_LsWorst > Hi_LsWorst_AnglesBest){
        Hi_LsWorst_AnglesBest = Hi_LsWorst
        
        results_df$Hi_worst[tau_index] = round(Hi_LsWorst_AnglesBest)
        results_df$gammaBest_forLsWorst[tau_index] = gamma_c
      }
      
      # Hi with best orientation angle gamma_c for best Ls.
      Hi_LsBest = H_i(Ls=results_df$Ls_best[tau_index], phi=phi, longitude=longitude, tau=tau,
                      beta=beta,
                      gamma_c=gamma_c)
      
      if(Hi_LsBest > Hi_LsBest_AnglesBest){
        Hi_LsBest_AnglesBest = Hi_LsBest
        
        results_df$Hi_best[tau_index] = round(Hi_LsBest_AnglesBest)
        results_df$gammaBest_forLsBest[tau_index] = gamma_c
      }
    }
    
    # Percent increase in daily insolation from Hh to Hi_worst.
    H_w_pc_inc = (results_df$Hi_worst[tau_index] - results_df$Hh_worst[tau_index]) / results_df$Hh_worst[tau_index] * 100
    results_df$H_w_pc_inc[tau_index] = round(H_w_pc_inc, 2)
    
    # Percent increase in daily insolation from Hh to Hi_best.
    H_b_pc_inc = (results_df$Hi_best[tau_index] - results_df$Hh_best[tau_index]) / results_df$Hh_best[tau_index] * 100
    results_df$H_b_pc_inc[tau_index] = round(H_b_pc_inc, 2)
    
    # Increment tau index in preparation for next iteration.
    tau_index = tau_index + 1
  }
  
  # return results
  return(results_df)
}


# Worst case insolations.
# For inclined surface we use best inclination angle beta and orientation angle gamma_c.
get_worst_case_daily_insolations = function(location_id, Ls_seq, phi, longitude, beta_max, tau){
  # The values we need.
  # On inclined surface.
  Ls_worst_inclined_surface = NULL
  H_worst_inclined_surface = NULL
  gammaBest_forLsWorst = NULL
  
  # On horizontal surace.
  Ls_worst_horizontal_surface = NULL
  H_worst_horizontal_surface = NULL
  
  # Check if daily insolations are already precalculated CSV file that serves as a lookup table.
  H_df = get_daily_insolation_lookup_table(location_id=location_id, tau=tau, beta_max=beta_max)
  
  # If cached results exist, fetch the values we are interested in.
  if(!is.null(H_df)){
    
    # For inclined surface.
    data_worst_inclined_surface = H_df[H_df$H_best == min(H_df$H_best),]
    
    Ls_worst_inclined_surface = data_worst_inclined_surface$Ls
    H_worst_inclined_surface = data_worst_inclined_surface$H_best
    gammaBest_forLsWorst = data_worst_inclined_surface$gamma_c_best
    
    # For horizontal surface.
    data_worst_horizontal_surface = H_df[H_df$H_horiz == min(H_df$H_horiz),]
    
    Ls_worst_horizontal_surface = data_worst_horizontal_surface$Ls
    H_worst_horizontal_surface = data_worst_horizontal_surface$H_horiz
    
  }else{
    # If there is no precalculated lookup table, then calculate the values based on worst properties for daily insolation on a horizontal surface.
    
    # For inclined surface.
    H_df = cache_fetch_worst_and_best_case_insolations(Ls_seq=Ls_seq, phi=phi, longitude=longitude, beta=beta_max, taus=tau, location_id=location_id)
    
    Ls_worst_inclined_surface = H_df$Ls_worst
    H_worst_inclined_surface = H_df$Hi_worst
    gammaBest_forLsWorst = H_df$gammaBest_forLsWorst
    
    # TODO: For horizontal surface.
  }
  
  if(is.null(Ls_worst_horizontal_surface)){
    stop("Not yet implemented: retrieving Hh_worst data.")
  } 
  
  result = list(
    "horizontal" = list(
      "H" = H_worst_horizontal_surface,
      "Ls" = Ls_worst_horizontal_surface,
      "gamma_c" = 0
    ),
    "inclined" = list(
      "H" = H_worst_inclined_surface,
      "Ls" = Ls_worst_inclined_surface,
      "gamma_c" = gammaBest_forLsWorst
    )
  )
}
