# Plot optimal beta and gamma_c combinations with different upper limits for beta.
library(here)
library(RColorBrewer)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "mission_sites"

cols = brewer.pal(n=11, name="RdYlBu")
colors = cols[c(1, 3, 9, 11)]

# Data directory.
data_dir = "mission-sites/data"

# Load data into dataframes.
data = list(
  "IsmeniusCavus" = list(
    # "Hbeta_optimal" = list(
    #   "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau0.4_beta_optimal.csv"), header=TRUE),
    #   "beta" = "Optimal"),
    # "Hbeta_max22" = list(
    #   "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau0.4_betamax22.csv"), header=TRUE),
    #   "beta" = 22),
    # "Hbeta_max21" = list(
    #   "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau0.4_betamax21.csv"), header=TRUE),
    #   "beta" = 21),
    # "Hbeta_max20" = list(
    #   "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau0.4_betamax20.csv"), header=TRUE),
    #   "beta" = 20),
    "Hbeta_max15" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau0.4_betamax15.csv"), header=TRUE),
      "beta" = 15),
    "Hbeta_max10" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau0.4_betamax10.csv"), header=TRUE),
      "beta" = 10),
    "Hbeta_max5" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau0.4_betamax5.csv"), header=TRUE),
      "beta" = 5)
  ),
  "IaniChaos" = list(
    # "Hbeta_optimal" = list(
    #   "df" = read.csv(here(data_dir, "IaniChaos/Hbest_tau0.4_beta_optimal.csv"), header=TRUE),
    #   "beta" = "Optimal"),
    # "Hbeta_max22" = list(
    #   "df" = read.csv(here(data_dir, "IaniChaos/Hbest_tau0.4_betamax-22.csv"), header=TRUE),
    #   "beta" = "22"),
    # "Hbeta_max21" = list(
    #   "df" = read.csv(here(data_dir, "IaniChaos/Hbest_tau0.4_betamax-21.csv"), header=TRUE),
    #   "beta" = 21),
    # "Hbeta_max20" = list(
    #   "df" = read.csv(here(data_dir, "IaniChaos/Hbest_tau0.4_betamax-20.csv"), header=TRUE),
    #   "beta" = 20),
    "Hbeta_max15" = list(
      "df" = read.csv(here(data_dir, "IaniChaos/Hbest_tau0.4_betamax-15.csv"), header=TRUE),
      "beta" = 15),
    "Hbeta_max10" = list(
      "df" = read.csv(here(data_dir, "IaniChaos/Hbest_tau0.4_betamax-10.csv"), header=TRUE),
      "beta" = 10),
    "Hbeta_max5" = list(
      "df" = read.csv(here(data_dir, "IaniChaos/Hbest_tau0.4_betamax-5.csv"), header=TRUE),
      "beta" = 5)
  )
)

plot_insolation = function(data, title, ylim, legend_pos=NULL){
  # Plot start convenience function.
  plot_start(PLOT_OUTPUT_KEY, plot_title=title)

  df_index = 1
  for(entry in data){
    df = entry$df

    if(df_index == 1){
      # Optimal.
      plot(df$Ls, df$H_best, type="l", ylim=ylim,
           xlab="Areocentric Longitude [deg]", ylab="Insolation [Wh/m²]",
           col=colors[df_index], lwd=2, lty=length(data)+2-df_index)
    }else{
      # Insolation with different maximum beta angles.
      lines(df$Ls, df$H_best, col=colors[df_index], lwd=2, lty=length(data)+2-df_index)
    }

    increase = sum(df$H_best) - sum(df$H_horiz)
    increase_pc = increase / sum(df$H_horiz) * 100
    print(paste("beta ", entry$beta_best, ": insolation increase from horizontal surface: ", round(increase_pc), "% (", round(increase), ")", sep=""))

    df_index = df_index + 1
  }

  # Horizontal.
  lines(df$Ls, df$H_horiz, col=colors[df_index], lwd=2, lty=1)

  # Legend.
  if(!is.null(legend_pos)){
    legend(legend_pos,
           title="β",
           legend=c("15°", "10°", "5°", "0°"),
           col=colors,
           lty=c(4, 3, 2, 1), cex=0.7, lwd=1)
  }

  plot_end()
}


title = "Ismenius Cavus solar insolations for different beta inclinations."
plot_insolation(data=data$IsmeniusCavus, title=title, ylim=c(1700, 5000), legend_pos="bottomleft")


title = "Iani Chaos solar insolations for different beta inclinations."
plot_insolation(data=data$IaniChaos, title=title, ylim=c(1700, 5000), legend_pos="bottomleft")


# # FIXME: Temp calculations.
# 
# source(here("utils", "insolation_utils.R"))
# 
# # Iani Chaos
# df_IaniChaos = get_daily_insolation_lookup_table(location_id="IaniChaos", tau=0.4, beta_max=-10)
# 
# H_diff_IaniChaos = sum(df_IaniChaos$H_best) - sum(df_IaniChaos$H_horiz)
# H_avg_IaniChaos = H_diff_IaniChaos / 360
# H_gain_IaniChaos = (H_diff_IaniChaos / sum(df_IaniChaos$H_horiz)) * 100
# 
# # Ismenius Cavus
# df_IsmeniusCavus = get_daily_insolation_lookup_table(location_id="IsmeniusCavus", tau=0.4, beta_max=10)
# 
# H_IsmeniusCavus = sum(df_IsmeniusCavus$H_best) - sum(df_IsmeniusCavus$H_horiz)
# H_avg_IsmeniusCavus = H_IsmeniusCavus / 360
# H_gain_IsmeniusCavus = (H_IsmeniusCavus / sum(df_IsmeniusCavus$H_horiz)) * 100