# Load library.
library(here)
library(RColorBrewer)

# Convenience functions for plotting.
source(here("utils", "plot_utils.R"))
PLOT_OUTPUT_KEY = "simulation"

# Plot colors
cols = brewer.pal(n=11, name="RdYlBu")
colors = list(
        power = cols[2], #d73027
        inclination = cols[10] #4575b4
)

# Fetch data.
df = read.csv(file = here("simulation/data", "slope_compensation.csv"))
df = df[df$step >= 60 & df$step <= 380,]

plot_title = "Slope Compensation"
plot_start(PLOT_OUTPUT_KEY, plot_title=plot_title, mar=c(5, 4, 4, 4) + 0.3)

plot(x=df$step,
     y=df$power,
     xaxt='n',
     xlab='Elapsed Time [s]',
     ylab='Generated Power [W]',
     ylim=c(min(df$power), max(df$power)),
     type='l', col=colors$power, lwd=2)


# Customize x-axis.
xtick_at = seq(0, max(df$step), 20)
xtick_labels = seq(0, max(df$t_elapsed), 5)
axis(side=1, at=xtick_at, labels=xtick_labels)

# Customize y-axis.
ytick = seq(0, 200, 20)
axis(side=2, at=ytick, labels=ytick)

# Allow a second plot on the same graph.
# This is for the second x-axis (slope angle).

par(new=TRUE)
plot(x=df$step,
     y=df$beta,
     xlab="", ylab="", ylim=c(15, 45),
     axes=FALSE, type='l', col=colors$inclination,
     lwd=2, lty=3)

mtext("SA Inclination Angle Beta [deg]", side=4, padj=3.5)
axis(4, ylim=c(15, 45), las=1)

legend("bottomright", legend=c("Solar Power", "SA inclination"),
       col=c(colors$power, colors$inclination),
       lty=c(1, 3), lwd=1, cex=0.8)

plot_end()