library(sm)

# -----------------------------------------------------------------------------
# CORRECTED SCRIPT FOR DATA PREPARATION AND CONVERSION
# -----------------------------------------------------------------------------

# Step 1: Define your actual data frame with a unique name
# We use 'gw_data_mgL' to be clear it contains groundwater data in mg/L.
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


# Step 2: Define the conversion function
# Inside this function, 'data' is just a placeholder for whatever data frame you pass in.
convert_to_meqL <- function(data) {
  # Molar masses (g/mol) and valences
  molar_mass <- c(Ca = 40.08, Mg = 24.31, Na = 22.99, K = 39.10,
                  Cl = 35.45, SO4 = 96.06, HCO3 = 61.02, CO3 = 60.01)
  valence <- c(Ca = 2, Mg = 2, Na = 1, K = 1, Cl = 1, SO4 = 2, HCO3 = 1, CO3 = 2)
  
  # This line will now work correctly because 'data' will be the 'gw_data_mgL' data frame
  data_meqL <- data.frame(SampleID = data$SampleID, WaterType = data$WaterType)
  
  # Loop through ions and apply the conversion formula
  for (ion in names(molar_mass)) {
    if (ion %in% names(data)) {
      data_meqL[[ion]] <- (data[[ion]] / molar_mass[ion]) * valence[ion]
    }
  }
  return(data_meqL)
}


# Step 3: CALL the function with your data frame as the input
# This is the step that executes the code inside the function correctly.
gw_data_meqL <- convert_to_meqL(data = gw_data_mgL) # Explicitly naming the argument


# Step 4: View the result to confirm it worked
print(gw_data_meqL)

# -----------------------------------------------------------------------------
# R SCRIPT FOR PIPER PLOT USING smwrGraphs (USGS)
# -----------------------------------------------------------------------------

# 1. INSTALL AND LOAD PACKAGE
# install.packages("smwrGraphs") # Run this once
library(smwrGraphs)


# (Assumes the data preparation and conversion steps above have been run)

# 2. PREPARE DATA FOR THE FUNCTION
# The piperPlot function requires a special summed 'NaK' column.
gw_data_meqL$NaK <- gw_data_meqL$Na + gw_data_meqL$K

# It also requires separate data frames for cations and anions.
cations <- gw_data_meqL[, c("Ca", "Mg", "NaK")]
anions <- gw_data_meqL[, c("Cl", "SO4", "HCO3", "CO3")]

# 3. DEFINE PLOTTING PARAMETERS (for customization)
water_groups <- as.factor(gw_data_meqL$WaterType)
my_colors <- c("blue", "red", "green", "purple")[water_groups]
my_symbols <- c(16, 17, 15, 18)[water_groups] # circle, triangle, square, diamond

# 4. GENERATE AND SAVE THE PLOT
png("piper_plot_smwr.png", width = 8, height = 7, units = "in", res = 300)

# The Plot argument is a list of settings to customize the plot
smwrGraphs::piperPlot(
  Anions = anions, 
  Cations = cations,
  Plot = list(
    name = "Piper Plot of Groundwater Chemistry (smwrGraphs)",
    color = my_colors,
    symbol = my_symbols
  )
)

# Add a legend
legend(
  "topright",
  legend = levels(water_groups),
  col = c("blue", "red", "green", "purple"),
  pch = c(16, 17, 15, 18),
  title = "Water Type",
  bty = "n"
)

dev.off()

cat("Piper plot generated and saved as 'piper_plot_smwr.png'\n")