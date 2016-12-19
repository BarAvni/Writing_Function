#' Hargreaves and Samani (1985) simple evapotranspiration model 
#'
#' This function calculates the daily evapotranspiration estimation using the Hargreaves and Samani 1985 method
#'

#' @param     date       () 
#' @param     lat        (deg) Latitude in degrees
#' @param     LAI_living () LAI for healthy forest
#' @param     LAI        () current stage LAI 
#' @param     Tave     (deg C) average temperature per day
#' @param     Tmax     (deg C) maximum temperature per day
#' @param     Tmin     (deg C) minimum temperature per day  
#' @author    Bar Avni 
#' @return    evapotranspiration (mm/day) 

Evapo_HS = 
  function(date, lat, LAI_living, LAI, Tave, Tmax, Tmin){

     #       Internal Variables
    
    #     Ra = (MJ/m^2/day)  extraterrestrial solar radiation
    #		  T_Diff= T_max-T_min (deg C) where T_max and T_min are the maximum and minimum temperatures per day respectively
    
  # Extraterrestrial radiation
    Ra = ExRadiation(date, lat, LAI_living, LAI)   
      
  # Temperature difference
    T_Diff = abs(Tmax - Tmin)
    
  #(assuming K_T = 0.017, 0.408 is to convert the units of Ra):
  EP = abs(0.0135*0.17*0.408*(Tave+17.78)*sqrt(T_Diff)*Ra)
  #return 
  return(EP)
  }