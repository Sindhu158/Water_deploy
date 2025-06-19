# 1. INSTALL AND LOAD PACKAGE
 install.packages("hydrogeo")
library(hydrogeo)

 gw_data_mgL <- data.frame(
   SampleID = c("Well-1", "Well-2", "Well-3", "Well-4", "Well-5", "Well-6", "Well-7", "Well-8"),
   WaterType = c("Ca-HCO3", "Ca-HCO3", "Na-Cl", "Na-Cl", "Mixed-Ca-Na-Cl", "Mixed-Ca-Na-Cl", "Na-HCO3", "Ca-SO4"),
   Ca = c(95, 105, 15, 20, 60, 55, 10, 150),
   Mg = c(22, 28, 5, 8, 30, 25, 3, 40),
   Na = c(10, 12, 150, 165, 80, 75, 120, 25),
   K = c(3, 4, 12, 15, 9, 8, 11, 5),
   Cl = c(15, 18, 220, 250, 130, 120, 20, 50),
   HCO3 = c(350, 400, 40, 50, 150, 160, 300, 100),
   CO3 = c(5, 6, 1, 2, 3, 2, 8, 2),
   SO4 = c(25, 30, 18, 22, 60, 55, 15, 450)
 )
 
 # 2. CONVERSION FUNCTION (mg/L to meq/L) - MANDATORY STEP
 convert_to_meqL <- function(data) {
   molar_mass <- c(Ca = 40.08, Mg = 24.31, Na = 22.99, K = 39.10,
                   Cl = 35.45, SO4 = 96.06, HCO3 = 61.02, CO3 = 60.01)
   valence <- c(Ca = 2, Mg = 2, Na = 1, K = 1, Cl = 1, SO4 = 2, HCO3 = 1, CO3 = 2)
   data_meqL <- data.frame(SampleID = data$SampleID, WaterType = data$WaterType)
   for (ion in names(molar_mass)) {
     if (ion %in% names(data)) {
       data_meqL[[ion]] <- (data[[ion]] / molar_mass[ion]) * valence[ion]
     }
   }
   return(data_meqL)
 }
 
 # 3. APPLY THE CONVERSION
 gw_data_meqL <- convert_to_meqL(gw_data_mgL)
 
 # View the resulting data in meq/L
 print(gw_data_meqL)

# (Assumes you have already run the data prep and conversion code above)


# 2. PREPARE PLOTTING PARAMETERS (colors, symbols)
# We can create vectors for colors and symbols to customize the plot.
water_groups <- as.factor(gw_data_meqL$WaterType)
my_colors <- c("#1f77b4", "#2ca02c", "#d62728", "#9467bd")[water_groups] # Blue, Green, Red, Purple
my_symbols <- c(16, 17, 15, 18)[water_groups] # Solid circle, triangle, square, diamond


# 3. GENERATE THE PIPER PLOT
# The piper.plot function takes individual vectors for each ion.
# To save to a file, wrap the code in png() and dev.off().
png("piper_plot_hydrogeo.png", width = 8, height = 7, units = "in", res = 300)

piper.plot(
  ca = gw_data_meqL$Ca,
  mg = gw_data_meqL$Mg,
  na = gw_data_meqL$Na,
  k = gw_data_meqL$K,
  cl = gw_data_meqL$Cl,
  so4 = gw_data_meqL$SO4,
  hco3 = gw_data_meqL$HCO3,
  # Customization arguments
  main = "Piper Plot of Groundwater Chemistry (hydrogeo)",
  col = my_colors,  # Apply the custom colors
  pch = my_symbols  # Apply the custom symbols
)

# 4. ADD A LEGEND (Manually)
# Base graphics require you to add a legend as a separate step.
legend(
  "topright", 
  legend = levels(water_groups),
  col = c("#1f77b4", "#2ca02c", "#d62728", "#9467bd"),
  pch = c(16, 17, 15, 18),
  title = "Water Type",
  bty = "n" # No box around legend
)

# Close the graphics device to save the file
dev.off()

cat("Piper plot generated and saved as 'piper_plot_hydrogeo.png'\n")
Use code with caution.
R
Option 2: Using the qualV Package
The qualV (Quality Visualization) package is another tool for visualizing water quality data. Its piper.diagram function is also easy to use and has a slightly different syntax and appearance. A nice feature is its built-in legend capability.
Full Script for qualV
# -----------------------------------------------------------------------------
# R SCRIPT FOR PIPER PLOT USING the qualV PACKAGE
# -----------------------------------------------------------------------------

# 1. INSTALL AND LOAD PACKAGE
# install.packages("qualV")
library(qualV)


# (Assumes you have already run the data prep and conversion code above)


# 2. PREPARE PLOTTING PARAMETERS (colors)
# Create a color vector just as before.
water_groups <- as.factor(gw_data_meqL$WaterType)
my_colors <- c("#1f77b4", "#2ca02c", "#d62728", "#9467bd")[water_groups]


# 3. GENERATE THE PIPER PLOT
# The piper.diagram function takes a data frame and column names.
# It can also automatically generate a legend for you.
png("piper_plot_qualV.png", width = 8, height = 7, units = "in", res = 300)

piper.diagram(
  data.frame = gw_data_meqL,
  ca.col = "Ca",
  mg.col = "Mg",
  na.col = "Na",
  k.col = "K",
  so4.col = "SO4",
  hco3.col = "HCO3",
  cl.col = "Cl",
  main = "Piper Plot of Groundwater Chemistry (qualV)",
  col.points = my_colors, # Apply the custom colors
  # Built-in legend arguments
  add.legend = TRUE,
  legend.pos = "topright",
  legend.title = "Water Type",
  legend.cex = 0.8 # Adjust legend text size
)

# Close the graphics device to save the file
dev.off()

cat("Piper plot generated and saved as 'piper_plot_qualV.png'\n")