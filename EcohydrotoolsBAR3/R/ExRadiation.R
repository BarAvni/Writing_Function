#' extraterrestrial solar radiation model 
#'
#' This function calculates the daily external radiation following the formulation presented in Allen et al, 1988
#' Date is converted to J is the number of the day in the year between 1 (1 January) and 365 or 366 (31 December).
#'
#' @param     date       () 
#' @param     lat        (deg) Latitude in degrees
#' @param     LAI_living () LAI for healthy forest
#' @param     LAI        () current stage LAI 
#' @author    Bar Avni 
#' @return    Extraterrestrial radiation (MJ m^-2)day^-1

ExRadiation = 
  function(date, lat, LAI_living, LAI){
  
    #       Internal Variables
    
    #       Gsc = solar constant (MJ m^-2min^-1)
    #       J = Julain Date
    #       delta = Solar decimation (rad)
    #       d_r = inverse relative distance Earth-Sun
    #       w_s = Sunset hour angle     
    
    
    # Set Parameters and initialization:
    # converts the latitude to radians   
    lat = NISTdegTOradian(lat)
    Gsc=0.082 
      
    # Convert date section:
      require(lubridate)
      J = yday(date) 
      
    # Calculation section:
      delta = 0.409*sin(2*pi/365*J-1.39) 
      d_r = 1+0.033*cos(2*pi/365*J)	    
      w_s = acos(-tan(lat)*tan(delta))    
      Ra = 24*60/pi*Gsc*d_r*(w_s*sin(lat)*sin(delta)+cos(lat)*cos(delta)*sin(w_s)) 
      
    # Observe radiation as fn. of LAI: 
      Ra = Ra*(LAI /LAI_living)

    # return
    Ra
    
  }




