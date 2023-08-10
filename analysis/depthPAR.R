# Kd(PAR) in Kaneohe Bay ranged from 0.2 to 0.35 (CISNet website, cited in Jacobson 2005).
# PAR measured in Nov/Dec at 2 meters depth = ~12 DLI (Cunning et al. 2016 Fig. S2; Barott et al. 2021 Fig. S2)

# Calculate surface PAR when PAR @ 2m is 12, with Kd 0.2 and Kd 0.35
surfPAR <- function(Kd1, Kd2, zPAR, z) {
  surfPAR1 <- exp(z * Kd1) * zPAR
  surfPAR2 <- exp(z * Kd2) * zPAR
  return(c(surfPAR1, surfPAR2))
}
surfPAR(0.2, 0.35, 12, 2)   # surface PAR is 17 - 24. Letʻs go with a value of 20 for now.


# Calculate depth range for a measured PAR, with a given surface PAR, and Kd = 0.35 and Kd = 0.2
depthRange <- function(Kd1, Kd2, surfPAR, PAR) {
  z1 <- 1 / Kd1 * log (surfPAR/PAR)
  z2 <- 1 / Kd2 * log (surfPAR/PAR)
  return(c(z1, z2))
}

#
# Lowest light treatment = 0.36 DLI
depthRange(0.62, 0.37, 20, 0.36)       # 6.5 to 10.9 meters 
depthRange(0.35, 0.2, 20, 0.36)       # 11.5 to 20.1 meters

# Next light treatment = 0.76 DLI
depthRange(0.62, 0.37, 20, 0.76)       # 5.3 to 8.8 meters 
depthRange(0.35, 0.2, 20, 0.76)       # 9.3 to 16.4 meters

# Next light treatment = 1.55 DLI
depthRange(0.62, 0.37, 20, 1.55)       # 4.2 to 6.9 meters
depthRange(0.35, 0.2, 20, 1.55)       # 7.3 to 12.8 meters

# Highest light treatment = 3.8 DLI
depthRange(0.62, 0.37, 20, 3.8)        # 2.7 to 4.5 meters
depthRange(0.35, 0.2, 20, 3.8)        # 4.7 to 8.3 meters

