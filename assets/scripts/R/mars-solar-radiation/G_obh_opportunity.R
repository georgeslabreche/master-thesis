# Diurnal variation of beam irradiance on a horizontal surface at top of Mars atmosphere above Endaveaour Crater.
library(mars)
library(RColorBrewer)

source("utils.R")

# Planetary latitude.
phi = -2

# Areocentric longitudes of interest
# Areocentric Longitude values (deg).
Ls_VE = 0       # Vernal Equinox - Dust Storm Season ends.
#Ls_A = 71       # APHELION.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox - Dust Storm Season begins.
#Ls_P = 248      # PERIPHELION - Dust Storm Season.
Ls_WS = 270     # Winter Solstice - Dust Storm Season.

# Data plotting order.
Ls_seq = c(Ls_VE, Ls_SS, Ls_AE, Ls_WS)
Ls_lbl_seq = c('Vernal Equinox', 'Summer Solstice', 'Autumn Equinox', 'Winter Solstice')

# Legend rendering order.
Ls_legend_seq = c(Ls_WS, Ls_AE, Ls_VE, Ls_SS)
Ls_lbl_legend_seq = c('Winter Solstice', 'Autumn Equinox', 'Vernal Equinox', 'Summer Solstice')

# Color blind friendly colors.
cols = brewer.pal(n=11, name="RdYlBu")
colors = c(cols[11], cols[9], cols[3], cols[2])
colors_legend = c(cols[2], cols[3], cols[11], cols[9])

# Plot title.
plot_title = "Gobh Diurnal Over Endaveaour Crater"

# Convenience plot start function.
plot_start("marsenv", plot_title)

Ls_index = 1
for(Ls in Ls_seq){
  Gobh_func = function(x){
    G_obh(Ls=Ls, phi=phi, Ts=x)
  }
  
  plot(Gobh_func, 6, 18, add=(Ls != Ls_seq[1]),
       xlab="", ylab="",
       ylim=c(0, 700),
       lwd=1.5,
       col=colors[Ls_index])

  Ls_index = Ls_index + 1
}

legend("bottom",
       inset=0.05,
       legend=paste0(Ls_lbl_legend_seq, ", Ls=", Ls_legend_seq, "°"),
       col=colors_legend, lty=1, lwd=1, cex=0.6)

title(xlab="Solar Time [h]",
      ylab="Irradiance, Gobh [W/m²]")

plot_end()

