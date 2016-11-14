# This code calls the Jarvis_mod function, generate the gs variable and then call the penman_montieth function
# to generate ET. In the next stage, sensetivity analysis are perform.


# 1. Load data:
Tdata = read.table("metdata.txt",header = TRUE, sep = "")

# Calculate the maximum surface conductance:
gsmax = max(Tdata[,c("gs")])

# Call the T variable from the metata file:
Tair = Tdata[,c("tavg")]

# Call the jarvis_modfunction:
Jarvis_mod(Tair,gsmax = 1.710054, Kt = 0.0016, Tx = 298)
