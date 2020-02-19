library(mars)
library(whisker)

Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

ROIs = list(
  "IaniChaos" = list(
    "phi" = -2,
    "longitude" = -17
  ), # https://trek.nasa.gov/mars/#v=0.1&x=-16.962890308581592&y=-2.10937496065263&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
  "IsmeniusCavus" = list(
    "phi" = 34,
    "longitude" = 17
  ) # https://trek.nasa.gov/mars/#v=0.1&x=17.138671555302636&y=33.92578061716317&z=2&p=urn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A104905&d=
)

orient = list(
  "South" = 0,
  "SouthEast" = -45,
  "East" = -90,
  "NorthEast" = -135,
  "North" = 180,
  "NorthWest" = 135,
  "West" = 90,
  "SouthhWest" = 45)


# Get the worst global daily insolation on Mars horizontal surface [Wh/m2-day].
Hh_worst = function(Ls_range, phi, longitude, tau, beta=NULL, gamma_c=NULL, verbose=FALSE){
  Hh_worst = 50000
  Ls_worst = -1
  
  for(Ls in Ls_range){
    H = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=gamma_c)
    
    if(H < Hh_worst){
      Hh_worst = H
      Ls_worst = Ls
    }
  }
  
  if(isTRUE(verbose)){
    text_template = "Worst daily insolation with tau = {{tau}} occurs at Ls = {{Ls}}° with H = {{H}} Wh/m2."
    text_data = list(tau=tau, Ls=Ls_worst, H=round(Hh_worst))
    text = whisker.render(text_template, text_data)
    print(text)
  }
  
  return(list(
    "Ls" = Ls_worst,
    "Hh" = Hh_worst,
    "tau" = tau))
}


Hh_best = function(Ls_range, phi, longitude, tau, beta=NULL, gamma_c=NULL, verbose=FALSE){
  Hh_best = 1
  Ls_best = -1
  
  for(Ls in Ls_range){
    H = H_i(Ls=Ls, phi=phi, longitude=longitude, tau=tau, beta=beta, gamma_c=gamma_c)
    
    if(H > Hh_best){
      Hh_best = H
      Ls_best = Ls
    }
  }
  
  if(isTRUE(verbose)){
    text_template = "Best daily insolation with tau = {{tau}} occurs at Ls = {{Ls}}° with H = {{H}} Wh/m2."
    text_data = list(tau=tau, Ls=Ls_best, H=round(Hh_best))
    text = whisker.render(text_template, text_data)
    print(text)
  }
  
  return(list(
    "Ls" = Ls_best,
    "Hh" = Hh_best,
    "tau" = tau))
}

# tau 0.1 to 0.5: Clear day
# tau 0.5 to 1.0: Moderately clear day (0.855). Source: https://trs.jpl.nasa.gov/bitstream/handle/2014/45184/09-1941_A1b.pdf?sequence=1
#                 Moderately dusty atmosphere (1.0). Source: https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/97JE00383
# tau 1.0 to 1.5: Dusty day (1.0). Source: None.
# tau > 1.5: High dust case. Source: https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20100017231.pdf
# tau > 3:  opacity was extraordinarily high. Source: https://link.springer.com/article/10.1007/s11214-017-0360-x
# tau = 0.4
# for(gamma_c in c(-(0:179), rev(0:180))){
#   Hh_w_IaniChaos = Hh_worst(Ls_range=1:360, phi=ROIs$IaniChaos$phi, longitude=ROIs$IaniChaos$longitude, tau=tau, beta=30, gamma_c=gamma_c, verbose=TRUE)
# }

tau = .1
#Hh_b_IaniChaos = Hh_best(Ls_range=180:360, phi=ROIs$IaniChaos$phi, longitude=ROIs$IaniChaos$longitude, tau=tau, beta=0, gamma_c=0, verbose=TRUE)
#Hh_w_IaniChaos = Hh_worst(Ls_range=1:360, phi=ROIs$IaniChaos$phi, longitude=ROIs$IaniChaos$longitude, tau=tau, beta=0, gamma_c=0, verbose=TRUE)

#Hh_b_IsmeniusCavus = Hh_best(Ls_range=1:360, phi=ROIs$IsmeniusCavus$phi, longitude=ROIs$IsmeniusCavus$longitude, tau=tau, beta=0, gamma_c=0, verbose=TRUE)
#Hh_w_IsmeniusCavus = Hh_worst(Ls_range=1:360, phi=ROIs$IsmeniusCavus$phi, longitude=ROIs$IsmeniusCavus$longitude, tau=tau, beta=0, gamma_c=0, verbose=TRUE)


