# Beam irradiance at top of Mars atmosphere as a function of Areocentric Longitude.
library(mars)
library(RColorBrewer)

source("utils.R")

# Areocentric Longitude values (deg).
Ls_VE = 0       # Vernal Equinox - Dust Storm Season ends.
Ls_A = 71       # APHELION.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox - Dust Storm Season begins.
Ls_P = 248      # PERIPHELION - Dust Storm Season.
Ls_WS = 270     # Winter Solstice - Dust Storm Season.
Ls_seq = c(Ls_VE, Ls_A, Ls_SS, Ls_AE,  Ls_P, Ls_WS)
Ls_lbl_seq = c('Vernal Equinox', 'Aphelion', 'Summer Solstice', 'Autumn Equinox', 'Periphelion', 'Winter Solstice')

# Color blind friendly colors.
# Plot colors
cols = brewer.pal(n=11, name="RdYlBu")
colors = list(
  irradiance = cols[2], #d73027
  Ls = cols[10] #4575b4
)

# Plot title.
plot_title = "Gob Daily Variations"

# Convenience plot start function.
plot_start("marsenv", plot_title)

# Start plotting.
curve(G_ob, 0, 360, 360,
  ylab="Irradiance, Gob [W/m²]",
  xlab="Areocentric Longitude [deg]",
  xaxt='n',
  yaxt='n',
  ylim=c(450, 750),
  type="l",
  lwd=2,
  col=colors$irradiance)

axis(1, seq(0, 360, 60))
axis(2, seq(450, 800, 50))

points(Ls_seq, ceiling(G_ob(Ls_seq)),
       pch="+",
       col=colors$Ls)

for(Ls in Ls_seq){
  lines(c(Ls, Ls), c(0, ceiling(G_ob(Ls))),
        lty=2,
        lwd=0.7,
        col=colors$Ls)
  lines(c(-15, Ls), c(ceiling(G_ob(Ls)), ceiling(G_ob(Ls))),
        lty=2,
        lwd=0.7,
        col=colors$Ls)
}

text(Ls_seq, G_ob(Ls_seq),
     #labels=paste(Ls_lbl_seq, " (", ceiling(G_ob(Ls_seq)), " W/m²)", sep=""),
     labels=Ls_lbl_seq,
     cex=0.75,
     #pos=c(4,1,4,1,3,1))
     pos=c(4,1,4,4,3,4))

plot_end()
