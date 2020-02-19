# Load library.
library(here)
library(RColorBrewer)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "simulation"

# Plot colors
cols = brewer.pal(n=11, name="RdYlBu")
colors = list(
  power = cols[2] #d73027
)

# Fetch data.
df = read.csv(file = here("simulation/data", "z-axis_revolutions.csv"))

plot_title = "Z-Axis Revolutions"
plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title)

plot(x=df$step,
     y=df$power,
     xaxt='n',
     xlab='Elapsed Time [s]',
     ylab='Generated Power [W]',
     ylim=c(min(df$power), max(df$power)),
     type='l', col=colors$power, lwd=2)

# Customize x-axis.
xtick_at = seq(0, max(df$step), 210)
xtick_labels = seq(0, max(df$t_elapsed), 40)
axis(side=1, at=xtick_at, labels=xtick_labels)

plot_end()
