#' Snow canopy interception model (Hedstrom and Pomeroy, 1998)
#' 
#' THis function compute evapotranspiration based on radiation, conductance etc
#' 

#' @param     preType () precipitation type TRUE = snow, FALSE = rain
#' @param     Tair    (deg C) air temperature
#' @param     LAI     ()      Leaf area index
#' @param     P       (mm)    precipitation 
#' @param     Smean   (kg/m^2)  snow loading coefficient
#' @param     L0      (mm SWE) initial snow load
#' @param     c       () dimensionless unloading coefficient = 0.678 
#' @param     MaxCanopyStorage (mm/day) changes according to LAI values
#' @author Bar Avni
#' @return effective rain/snow (mm) 


canopyInterception =
  function(preType, Tair, LAI, P, Smean, L0, c, MaxCanopyStorage) {
    
    #       Internal Variables
    #
    #       Il     (mm SWE)  interception before loading occure
    #       Lstar  (mm SW)   maximum intercepted snow load that can be retaind by the forest cannopy given current canopy structure and temperature conditions
    #       k      ()        proportionality factor
    #       S      (kg/m^2)  maximum snow load per unit branch area 
    #       ps     (kg/m^3)  fresh snow density

 #per= 0
 #preType = metdata_REP$preType
 #Tair = metdata_REP$TmMax
 #LAI = 1.56
 #P = metdata_REP$P
 #Smean = 6.6
 #L0 = 0
 #c = 0.678
 #MS = 0.7
 #LAI_living= 1.35
 #MaxCanopyStorage = MS*(LAI /LAI_living)


storage = 0
overStorage = rep(0, times = length(P))
 I = rep(0, times = length(P))
 for (i in 1:length(P)) {
   if (preType[i] == 1) {
    # Fresh snow density (ps) as a fn. of air temp
    ps = 67.92 + 51.25*exp(Tair[i]/2.59)
  
    # Maximum snow load per unit branch area (S) as a fn. of snow loading coefficient (Smean) and ps
    S = Smean*(0.27+(46/ps))
    
    # Maximum intercepted snow load (Lstar) as a fn. of S and LAI
    Lstar = S*LAI
      
    # Proportionality factor (k) as a fn. of Lstar
    k = 1/Lstar
      
    # Interception before loading occure (Il) as a fn. of Lstar, initial snow load (L0), k and snow precipitation (P)
    Il = (Lstar - L0)*(1- exp(-k*P[i]))
      
    # Canopy interception (I) as a fn. of Il and dimensionless unloading coefficient (c) 
    I[i] = Il*c
    overStorage[i] = 0
    storage = 0
   } else {
     I[i] = 0
     storage = storage + P[i]
     if (storage > MaxCanopyStorage) {
       overStorage[i] = storage - MaxCanopyStorage 
       storage = MaxCanopyStorage
     } else {overStorage[i] = 0}
     }
    # Each next day depends on the snow that accunulated in the previous day (so LO is set to I)
    L0 = I[i]
    #per[i] = I[i]/P[i] 
 }
 # Calculate the snowpack on the ground (mm) (snow percipitation (P) - canopy snow interseption (I)) 
 WY = P - I
 # Add effective rain to times with rain
 WY[preType == 0] = overStorage[preType == 0]
    # return from your function
   return(WY)
  }
