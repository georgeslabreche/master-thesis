# Calculate best and worst combinations for beta and gamma_c angles.

library(mars)

Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

ROIs = list(
  "IaniChaos" = list(
    "phi" = -2,
    "longitude" = -17,
    "data_dump_dir" = "data/mission-sites/IaniChaos"
  ), # https://trek.nasa.gov/mars/#v=0.1&x=-16.962890308581592&y=-2.10937496065263&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
  "IsmeniusCavus" = list(
    "phi" = 34,
    "longitude" = 17,
    "data_dump_dir" = "data/mission-sites/IsmeniusCavus"
  ) # https://trek.nasa.gov/mars/#v=0.1&x=17.138671555302636&y=33.92578061716317&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
)

data_dump_at_every_Ls = FALSE

# Variables that requires updating in case of location change.
roi = ROIs$IsmeniusCavus
beta_min = 15 # Must be negative in southern hemisphere.
beta_max = 15 # Must be negative in southern hemisphere.
beta_seq = beta_min:beta_max

tau = 1.5

phi = roi$phi
longitude = roi$longitude

Ls_seq = 1:360

gamma_min = -179
gamma_max = 180

# gamma_min = 0
# gamma_max = 150

gamma_seq = gamma_min:gamma_max

# When constraining the rnage of gamma_seq, do so by this value.
gamma_range = 10

find_best = TRUE
find_worst = !find_best
dynamic_gamma_boundaries = TRUE

if(!exists("H_df")){
  H_df = data.frame(
    "Ls" = 1:length(Ls_seq),
    "H_horiz" = rep(NA, length(Ls_seq)),
    "H_best" = rep(NA, length(Ls_seq)),
    "beta_best" = rep(NA, length(Ls_seq)),
    "gamma_c_best" = rep(NA, length(Ls_seq))
    # "H_worst" = rep(NA, length(Ls_seq)),
    # "beta_worst" = rep(NA, length(Ls_seq)),
    # "gamma_c_worst" = rep(NA, length(Ls_seq))
  )}

Ls_index = 1
for(Ls in Ls_seq){
  if(is.na(H_df$H_horiz[Ls_index])){
    print(paste("Ls =", Ls))
    
    # Daily insolation on horizontal surface.
    H_horiz = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=0, gamma_c=0)

    H_worst = Inf
    gamma_worst = NA
    beta_worst = NA
    
    H_best = 0
    gamma_best = NA
    beta_best = NA

    for(beta in beta_seq){
      print(paste("beta =", beta))

      for(gamma_c in gamma_seq){
        
        Hi = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=gamma_c)
       
        if(Hi > H_best){
          H_best = Hi
          beta_best = beta
          gamma_best = gamma_c
        }
        
        if(Hi < H_worst){
          H_worst = Hi
          beta_worst = beta
          gamma_worst = gamma_c
        }
      }
    }
    
    if(isTRUE(find_best)){
      # Stop and prompt for update of lower or upper limits.
      #if(length(beta_seq) > 1 && beta_best %in% c(min(beta_seq), max(beta_seq))){
      if(length(beta_seq) > 1 && beta_best %in% c(min(beta_seq))){
        stop(paste("Update beta lower or upper limits: ", beta_best, sep=""))
      }
      else if(length(gamma_seq) > 1 && gamma_best %in% c(min(gamma_seq), max(gamma_seq))){
        stop(paste("Update gamma lower or upper limits: ", gamma_best, sep=""))
      }
    }
    
    if(isTRUE(find_worst)){
      # Stop and prompt for update of lower or upper limits.
      if(length(beta_seq) > 1 && beta_worst %in% c(min(beta_seq), max(beta_seq))){
        stop(paste("Update beta lower or upper limits: ", beta_worst, sep=""))
      }
      else if(length(gamma_seq) > 1 && gamma_worst %in% c(min(gamma_seq), max(gamma_seq))){
        stop(paste("Update gamma lower or upper limits: ", gamma_worst, sep=""))
      }
    }


    # Put results in data frame.
    H_df$Ls[Ls_index] = Ls
    H_df$H_horiz[Ls_index] = H_horiz
    
    if(isTRUE(find_best)){
      H_df$H_best[Ls_index] = H_best
      H_df$beta_best[Ls_index] = beta_best
      H_df$gamma_c_best[Ls_index] = gamma_best
      
      # Display result.
      print(paste("H_best = ", H_best, " with Ls = ", Ls, ", beta = ", beta_best, ", and gamma_c = ", gamma_best, sep=""))
      
      if(isTRUE(dynamic_gamma_boundaries)){
        gamma_min = gamma_best-gamma_range
        gamma_max = gamma_best+gamma_range
        gamma_seq = gamma_min:gamma_max
      }
    }

    if(isTRUE(find_worst)){
      H_df$H_worst[Ls_index] = H_worst
      H_df$beta_worst[Ls_index] = beta_worst
      H_df$gamma_c_worst[Ls_index] = gamma_worst
      
      # Display result.
      print(paste("H_worst = ", H_worst, " with Ls = ", Ls, ", beta = ", beta_worst, ", and gamma_c = ", gamma_worst, sep=""))
      
      if(isTRUE(dynamic_gamma_boundaries)){
        gamma_min = gamma_worst-gamma_range
        gamma_max = gamma_worst+gamma_range
        gamma_seq = gamma_min:gamma_max
      }
      
    }
  }
  
  # Backup in case of data loss.
  if(isTRUE(data_dump_at_every_Ls)){
    write.csv(H_df, paste(roi$data_dump_dir, "/H_tau_", tau, "_", "Ls_", Ls, ".csv", sep=""), row.names = FALSE)
  }

  # Move on to next Ls.
  Ls_index = Ls_index + 1
  
}

if(!isTRUE(data_dump_at_every_Ls)){
  write.csv(H_df, paste(roi$data_dump_dir, "/", "Hbest_tau", tau, "_betamax", beta_max, ".csv", sep=""), row.names = FALSE)
}


diff = sum(H_df$H_best) - sum(H_df$H_horiz)
gain = diff / sum(H_df$H_horiz) * 100

print(round(sum(H_df$H_best)), 2)
print(round(diff), 2)
print(round(gain, 2))
print(unique(H_df$beta_best))

rm(H_df)