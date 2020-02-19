# Get solar array density basd on literature.

# To calculate average.
SA_densities = c()

# Case A: ExoMars.
#
# Source: A combined Exobiology and Geophysics Mission to Mars 2009
#         https://sci.esa.int/documents/33745/35957/1567254332993-6_Pillinger.pdf

solar_cell_area = 1.138 # m2
SA_mass = 5.9 # kg
solar_cell_packing_efficiency = 0.67

SA_area = solar_cell_area / solar_cell_packing_efficiency

SA_density = SA_mass / SA_area

print(paste(SA_density, "kg/m2"))
SA_densities = c(SA_densities, SA_density)

# Case B: MarsFAST,
#
# Source: CDF Study Report: MarsFAST. Assessment of an ESA Fast Mobility Mars Rover. Table 10-6, p.84.
#         https://sci.esa.int/documents/34923/36148/1567260221093-MarsFast_CDF_study_report.pdf

SA_density = 4.4 # kg/m2

print(paste(SA_density, "kg/m2"))
SA_densities = c(SA_densities, SA_density)

# Case C: Beagle-2.
#
#   Solar panel diameter: 0.57 m
#   Source https://royalsocietypublishing.org/doi/pdf/10.1098/rsos.170785
#
#   Solar panel including hinges: 3.21 kg
#   Source: https://pdfs.semanticscholar.org/08e8/5ff43237cfa493eee66c5f1349cfc616ebc8.pdf
#
#   Solar cell packing efficiency: 85 %
#   Source: https://royalsocietypublishing.org/doi/pdf/10.1098/rsos.170785

solar_panel_diameter = 0.57 # meters.
solar_cell_packing_efficiency = 0.85

solar_panel_area = (pi * (solar_panel_diameter / 2)^2) * solar_cell_packing_efficiency 

SA_area = solar_panel_area * 4
SA_mass = 3.21 # kg

SA_density = SA_mass / SA_area

print(paste(SA_density, "kg/m2"))
SA_densities = c(SA_densities, SA_density)

# Case D: ATK Ultraflex/Megaflex
# https://www.lpi.usra.edu/opag/meetings/aug2015/presentations/day-2/11_beauchamp.pdf
# 150 Watts/kg at 1 AU BOL.

# area: 4.2 m2
# diameter of 2.15 meters
# Source: https://www.seis-insight.eu/en/public-2/the-insight-mission/insight-lander

SA_density_avg = mean(SA_densities)
print(paste(round(SA_density, 1), "kg/m2"))
