
# Use NASAPower to get surface DLI for Kaneohe Bay
#install.packages("nasapower")
library(nasapower)
kbay <- get_power(
  community = "ag",
  lonlat = c(-157.793795, 21.439854),
  #dates = c("1984", "1985"),
  temporal_api = "climatology",
  pars = "ALLSKY_SFC_SW_DWN"
)
# Multiply annual MJ/m^2/day * 0.45 (proportion of solar radiation in PAR) * 4.48 µmol/J
# See https://github.com/micahwoods/global_dli/blob/master/dli_details.md
surfPAR <- kbay$ANN * 0.45 * 4.48
surfPAR  # Annual mean surface PAR is 41.2 mol/m^2/d

# Calculate depth range for a measured PAR, with a given surface PAR, and Kd = 0.37 and Kd = 0.62
## Kd values from Jacobson 2005, see jacobson_kd_data.csv
depthRange <- function(Kd1, Kd2, surfPAR, PAR) {
  z1 <- 1 / Kd1 * log (surfPAR/PAR)
  z2 <- 1 / Kd2 * log (surfPAR/PAR)
  return(c(z1, z2))
}

# Lowest light treatment = 0.36 DLI
depthRange(0.62, 0.37, surfPAR, 0.36)       # 7.6 to 12.8 meters

# Next light treatment = 0.76 DLI
depthRange(0.62, 0.37, surfPAR, 0.76)       # 6.4 to 10.8 meters

# Next light treatment = 1.55 DLI
depthRange(0.62, 0.37, surfPAR, 1.55)       # 5.3 to 8.9 meters

# Highest light treatment = 3.8 DLI
depthRange(0.62, 0.37, surfPAR, 3.8)        # 3.8 to 6.4 meters



# What level of PAR corresponds to 4 meters?
Kd1 = 0.62
Kd2 = 0.37
z = 4
surfPAR / exp(z * Kd1)
surfPAR / exp(z * Kd2)
# 10 meters?
z = 10
surfPAR / exp(z * Kd1)
surfPAR / exp(z * Kd2)

