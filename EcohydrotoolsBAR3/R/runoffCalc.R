#' Runoff model
#' 
#' THis function compute runoff from evapotranspiration, infiltration and snow precipitation  

#' @param     P (mm SWE) monthly snow precipitation
#' @param     I (mm) monthly canopy interception 
#' @param     InfilCap (mm/month) constant infiltration capacity
#' @param     MaxStorage (mm) maximum capacity of ground storage
#' @param     ET (mm/month) evapotranspiration
#' @param     year 
#' @author Bar Avni
#' @return list of yearly runoff depth (mm) for three lodgpol pine mortality stages - living, red, grey (source: Pugh and Gordon, 2013) 


runoffCalc =
  function(P, I, InfilCap = 36*24  , MaxStorage = 25, ET, year) {
    
    #       Internal Variables
    #
    #       Pfc         (mm)  snowpack on the ground
    #       storage     (mm)  ground storage before runoff could start
    #       OverStorage (mm)
    #       year_diff   (  )  define the years on the list
    #       year_index  (  )  define the years on the list
    #       year_index_end ( ) define the years on the list
    #       year_index_start ( ) define the years on the list
    #       Runoff_year (  )  yearly runoff

#P = metdata$Pm*2.54
#I = I_LAI[,1]
#year = metdata$Year
#InfilCap = 36*24
#MaxStorage = 25
#ET = ET
    # Calculate the snowpack on the ground (Pfc) (snow percipitation (P) - canopy snow interseption (I))
      Pfc  = P - I  

    # Set initial value for storage (assuming the soil is dry)
    year_diff = diff(year)
    year_index = which(year_diff %in% 1)
    year_index_end = c(year_index, 72)
    year_index_start = c(1,13,25,37,49,61)
    
    
Runoff = rep(0, times=length(year))
Runoff_year = rep(0, times=length(year_index_end))
      for (m in 1:length(year_index_start)) {
        storage = 0
        for (i in year_index_start[m]:year_index_end[m]) {
          storage = storage + Pfc[i]
          if (storage > MaxStorage) {
            OverStorage = storage - MaxStorage
            storage = storage - OverStorage
            if (OverStorage > InfilCap) {
              Runoff[i] = OverStorage - InfilCap
            }else {Runoff[i] = 0 }
          }else {Runoff[i] = 0 }
          storage = storage - ET[i]
          if (storage < 0) {storage = 0}
        }
        Runoff_year[m] = sum(Runoff)
      }
    # return from your function
    return(Runoff_year)
}
