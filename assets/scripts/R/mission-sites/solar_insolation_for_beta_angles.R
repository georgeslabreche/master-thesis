# Plot optimal beta and gamma_c combinations with different upper limits for beta.
library(here)
library(RColorBrewer)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY_MISSION_SITES = "mission_sites"
PLOT_OUTPUT_KEY_MARS_ENV = "marsenv"

cols = brewer.pal(n=11, name="RdYlBu")
colors = cols[c(1, 3, 9, 11)]

# Data directory.
data_dir = "data/mission-sites"

# Load data into dataframes.
data = list(
  "IsmeniusCavus" = list(
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


data_tau = list(
  "IsmeniusCavus_tau1" = list(
    "Hbeta_max15" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau1_betamax15.csv"), header=TRUE),
      "beta" = 15),
    "Hbeta_max10" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau1_betamax10.csv"), header=TRUE),
      "beta" = 10),
    "Hbeta_max5" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau1_betamax5.csv"), header=TRUE),
      "beta" = 5)
  ),
  "IsmeniusCavus_tau1p5" = list(
    "Hbeta_max15" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau1.5_betamax15.csv"), header=TRUE),
      "beta" = 15),
    "Hbeta_max10" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau1.5_betamax10.csv"), header=TRUE),
      "beta" = 10),
    "Hbeta_max5" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau1.5_betamax5.csv"), header=TRUE),
      "beta" = 5)
  ),
  "IsmeniusCavus_tau2" = list(
    "Hbeta_max15" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau2_betamax15.csv"), header=TRUE),
      "beta" = 15),
    "Hbeta_max10" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau2_betamax10.csv"), header=TRUE),
      "beta" = 10),
    "Hbeta_max5" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau2_betamax5.csv"), header=TRUE),
      "beta" = 5)
  ),
  "IsmeniusCavus_tau3" = list(
    "Hbeta_max15" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau3_betamax15.csv"), header=TRUE),
      "beta" = 15),
    "Hbeta_max10" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau3_betamax10.csv"), header=TRUE),
      "beta" = 10),
    "Hbeta_max5" = list(
      "df" = read.csv(here(data_dir, "IsmeniusCavus/Hbest_tau3_betamax5.csv"), header=TRUE),
      "beta" = 5)
  )
)

plot_insolation = function(data, title, ylim, legend_pos=NULL, lwd_lines=2, lwd_legend=1, plot_output_key){
  # Plot start convenience function.
  plot_start(plot_output_key, plot_title=title)

  df_index = 1
  for(entry in data){
    df = entry$df

    if(df_index == 1){
      # Optimal.
      plot(df$Ls, df$H_best, type="l", ylim=ylim,
           xlab="Areocentric Longitude [deg]", ylab="Insolation [Wh/m²]",
           col=colors[df_index], lwd=lwd_lines, lty=length(data)+2-df_index)
    }else{
      # Insolation with different maximum beta angles.
      lines(df$Ls, df$H_best, col=colors[df_index], lwd=lwd_lines, lty=length(data)+2-df_index)
    }

    increase = sum(df$H_best) - sum(df$H_horiz)
    increase_pc = increase / sum(df$H_horiz) * 100
    print(paste("beta ", entry$beta_best, ": insolation increase from horizontal surface: ", round(increase_pc), "% (", round(increase), ")", sep=""))

    df_index = df_index + 1
  }

  # Horizontal.
  lines(df$Ls, df$H_horiz, col=colors[df_index], lwd=lwd_lines, lty=6)

  # Legend.
  if(!is.null(legend_pos)){
    legend(legend_pos,
           title="β",
           legend=c("15°", "10°", "5°", "0°"),
           col=colors,
           lty=c(4, 3, 2, 6), cex=0.7, lwd=lwd_legend)
  }

  plot_end()
}


title = "Ismenius Cavus solar insolations for different beta inclinations."
plot_insolation(data=data$IsmeniusCavus, title=title, ylim=c(1700, 5000), legend_pos="bottomleft",
                plot_output_key=PLOT_OUTPUT_KEY_MISSION_SITES)

title = "Iani Chaos solar insolations for different beta inclinations."
plot_insolation(data=data$IaniChaos, title=title, ylim=c(1700, 5000), legend_pos="bottomleft",
                plot_output_key=PLOT_OUTPUT_KEY_MISSION_SITES)

title = "Ismenius Cavus solar insolations for different beta inclinations at tau factor 1."
plot_insolation(data=data_tau$IsmeniusCavus_tau1, title=title, ylim=c(1000, 3500), legend_pos="bottomleft",
                lwd_lines=1, plot_output_key=PLOT_OUTPUT_KEY_MARS_ENV)

title = "Ismenius Cavus solar insolations for different beta inclinations at tau factor 1.5."
plot_insolation(data=data_tau$IsmeniusCavus_tau1p5, title=title, ylim=c(1000, 3500), legend_pos="bottomleft",
                lwd_lines=1, plot_output_key=PLOT_OUTPUT_KEY_MARS_ENV)

