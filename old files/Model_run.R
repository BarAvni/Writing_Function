# This code calls the Jarvis_mod function, generate the gs variable and then call the penman_montieth function
# to generate ET. In the next stage, sensetivity analysis are perform.

#rm(list = ls())
# 1. Load data:
Tdata = read.table("metdata.txt",header = TRUE, sep = "")

# Calculate the maximum surface conductance:
gsmax = max(Tdata[,c("gs")])

# Call the T variable from the metata file:
Tair = Tdata[,c("tavg")]

# Call the jarvis_modfunction:
gs <- Jarvis_mod(Tair,gsmax = 1.710054, Kt = 0.0016, Tx = 298)
# I used only the tempature variable from modified Jarvis equation. The relation of surface conductance
# to tempature is taken from (Lhomme,1998). The temapture changes throughout the year according to the 
#seasons and climate and therefore the surface conductance changes. I simplified the conductance behivior 
#into two states: on or off depends on the temapture.

# load the variables for Penman&Montieth function: 
vpd = Tdata[,c("vpd")] 
Rnet = Tdata[,c("rnet")]
ga = Tdata[,c("ga")]
dayl = Tdata[,c("ga")]

# Call the  Penman&Montieth function: 
Et = penman_montieth(Tair, vpd, Rnet, gs,ga, dayl = 24, CP=1010, Pair=101325)

# Perform sensitivity analysis to eximine how increasing air tempature impacts ET estimates:
Tfactor = -20
for (i in 1:length(Tfactor)) {
  Tair_fac2 = Tair*Tfactor[i]
  gs_fac2 <- Jarvis_mod(Tair_fac2, gsmax = 1.710054, Kt = 0.0016, Tx = 298)
}

#plot graphs:
plot(Tair,gs,type = "l", col = "red", xlab = "air tempature (K)", ylab = "surface conductance (s/mm)")
# Create a title with a red, bold/italic font
title(main="gs as function of air tempature", col.main="black", font.main=4)
plot(Tair_fac2,gs_fac2,type = "l", col = "blue", xlab = "air tempature (K)", ylab = "surface conductance (s/mm)")
title(main="gs as function of air tempature times factor -20", col.main="black", font.main=4)
# The model is not sensitive to temapture due to the assumption was done in the Jarvis_mod function. 

