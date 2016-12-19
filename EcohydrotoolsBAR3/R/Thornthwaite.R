#' Thornthwaite Model
#'
#' Developed by Warren Thornthwaite in 1948 to estimate PET:
#'
#' @param     year  year
#' @param     month month    
#' @param     Tair  (deg C) mean monthly air temperature
#' @param     TMDH  Total monthly daylight hours
#' @author Bar Avni 
#' @return monthly potential ET 

Thornthwaite = 
  function(year, month, Tair, TMDH){
    
    #       Internal Variables

    #       Tk = Mean temperature of the month (deg k)
    #       MH_index = Monthly heat index (deg C)
    #       year_diff = find the first day of the year
    #       year_index = find the first day of the year
    #       AH_index = annual index
    #       a = Thornthwaite parameter
    #       b = Thornthwaite parameter
 
    
# Convert the temperature in F deg to K deg
Tk = (5*((Tair/10) - 32))/9 + 273.15

# monthly heat index:
MH_index = (Tk/5)^(1.514)

# Sum the 12 monthly heat index into an annual heat index (I):
year_diff = diff(year)
year_index = which(year_diff %in% 1)

for (n in 1:length(year_index)) {
  if (n == 1){
    AH_index = sum(MH_index[1:year_index[n]]) 
  } else {
    AH_index[n] = sum(MH_index[(year_index[n-1]+1):year_index[n]])  
  }
}

# Monthly potential ET - Ep:
Ep = 0
for (m in 1:length(AH_index)) {
  a = 0.49+1.79*10^(-2)*AH_index[m] + 7.711*10^(-5)*AH_index[m]^2 + 6.751*10^(-7)*AH_index[m]^3
  for(l in 1:length(MH_index)){
    b =  TMDH[l]/360
    Ep[l] = 1.6*b*((10*Tk[l])/(AH_index[m]))^a
  }
}
# return
Ep
}



    
    