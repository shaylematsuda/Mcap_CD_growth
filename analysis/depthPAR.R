
# PAR measured in at 2 meters depth shows annual mean of ~18 mol m-2 d-1 (Cunning et al. 2016 Fig. S2; Barott et al. 2021 Fig. S2)

# Calculate surface PAR when PAR @ 2m is 18, with Kd 0.62 and Kd 0.37 (from Jacobson 2005, see jacobson_kd_data.csv)
surfPAR <- function(Kd1, Kd2, zPAR, z) {
  surfPAR1 <- exp(z * Kd1) * zPAR
  surfPAR2 <- exp(z * Kd2) * zPAR
  return(c(surfPAR1, surfPAR2))
}
surfPAR(0.62, 0.37, 18, 2)   # surface PAR is 37 - 62. Use middle of range = 50


# Calculate depth range for a measured PAR, with a given surface PAR, and Kd = 0.37 and Kd = 0.62
depthRange <- function(Kd1, Kd2, surfPAR, PAR) {
  z1 <- 1 / Kd1 * log (surfPAR/PAR)
  z2 <- 1 / Kd2 * log (surfPAR/PAR)
  return(c(z1, z2))
}

#
# Lowest light treatment = 0.36 DLI
depthRange(0.62, 0.37, 50, 0.36)       # 8.0 to 13.3 meters // 6.5 to 10.9 meters // 11.5 to 20.1 meters

# Next light treatment = 0.76 DLI
depthRange(0.62, 0.37, 50, 0.76)       # 6.8 to 11.3 meters // 5.3 to 8.8 meters // 9.3 to 16.4 meters

# Next light treatment = 1.55 DLI
depthRange(0.62, 0.37, 50, 1.55)       # 5.6 to 9.4 meters // 4.2 to 6.9 meters // 7.3 to 12.8 meters

# Highest light treatment = 3.8 DLI
depthRange(0.62, 0.37, 50, 3.8)        # 4.2 to 6.9 meters // 2.7 to 4.5 meters // 4.7 to 8.3 meters



# What level of PAR corresponds to 4 meters?
surfPAR = 50
Kd1 = 0.62
Kd2 = 0.37
z = 4
surfPAR / exp(z * Kd1)
surfPAR / exp(z * Kd2)

z = 10
surfPAR / exp(z * Kd1)
surfPAR / exp(z * Kd2)

