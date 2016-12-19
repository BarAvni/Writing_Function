#' Runoff model
#' 
#' THis function compute runoff from evapotranspiration, infiltration and snow precipitation  

#' @param     LAI_living () LAI for healthy forest
#' @param     LAI        ()      Leaf area index
#' @param     MS         (mm/day) maximum rain interception in a living forest
#' @param     preType    () precipitation type TRUE = snow, FALSE = rain
#' @param     Tair       (deg C) air temperature
#' @param     P          (mm)    daily precipitation 
#' @param     Smean      (kg/m^2)  snow loading coefficient
#' @param     L0         (mm SWE) initial snow load
#' @param     c          () dimensionless unloading coefficient = 0.678 
#' @param     date       () 
#' @param     lat        (deg) Latitude in degrees
#' @param     Tave       (deg C) average temperature per day
#' @param     Tmax       (deg C) maximum temperature per day
#' @param     Tmin       (deg C) minimum temperature per day 
#' @param     InfilCap   (mm/day) constant infiltration capacity
#' @param     MaxStorage (mm) maximum capacity of ground storage
#' @param     month
#' @param     year 
#' @author Bar Avni
#' @return monthly runoff depth (mm) 

Water_yield_cold_env =
  function(LAI_living, LAI, MS , preType, Tair, P, Smean, L0, c, date, lat, Tave, Tmax, Tmin, InfilCap, MaxStorage, month, year ) {
    
    #       Internal Variables
    #
    #       MaxCanopyStorage (mm/day) changes according to LAI values
    #       Q (mm) effective rain/snow 
    #       ET (mm/day) evapotranspiration

    # Compute snow interception when snowing and runoff when its rain
    MaxCanopyStorage = MS*(LAI /LAI_living)
    Q = canopyInterception(preType, Tair, LAI , P , Smean, L0, c, MaxCanopyStorage)

    # compute evapotranspiration
    ET = Evapo_HS(date, lat, LAI_living, LAI, Tave, Tmax, Tmin)
    
    # compute water yield (runoff)
    Runoff_month = runoffCalc(Q, InfilCap, MaxStorage, ET, month, year) 

  #return
   return(Runoff_month)
}
