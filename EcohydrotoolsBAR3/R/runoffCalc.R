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
  function(P, I, InfilCap = 36*24*30  , MaxStorage = 25, ET, year) {
    
    #       Internal Variables
    #
    #       Pfc         (mm)  snowpack on the ground
    #       storage     (mm)  ground storage before runoff could start
    #       OverStorage (mm)
    #       year_diff   (  )  define the years on the list
    #       year_index  (  )  define the years on the list
    #       year_index_end ( ) define the years on the list
    #       year_index_start ( ) define the years on the list
    #       Pfc_case    (  )  snow precipitation according to LAI values. 
    #       Runoff_year (  )  yearly runoff
    #       Runoff_phase ( )  gather the model phases
    

    # Calculate the snowpack on the ground (Pfc) (snow percipitation (P) - canopy snow interseption (I))
    for (j in 1:length(I)){
      Pfc[1:end,j]  = P - I[[j]]  
    }
    
    # Set initial value for storage (assuming the soil is dry)
    year_diff = diff(year)
    year_index = which(year_diff %in% 1)
    year_index_end = c(year_index, 72)
    year_index_start = c(1,13,25,37,49,61)

    for (n in 1:3) {
      Pfc_case = Pfc[1:end, n]
      for (m in 1:length(year_index_start)) {
        storage = 0
        for (i in year_index_start[m]:year_index_end[m]) {
          storage = storage + Pfc_case[i]
          if (storage > MaxStorage) {
            OverStorage = storage - MaxStorage
            storage = storage - OverStorage
            if (OverStorage > InfilCap[n]) {
              Runoff[i] = OverStorage - InfilCap[n]
            }else {Runoff[i] = 0 }
          }
          storage = storage - ET
        }
        Runoff_year[m] = sum(Runoff)
      }
      Runoff_phase[1:end,n] = Runoff_year
    }

    Runoff_living = Runoff_phase[1:end,1]
    Runoff_red = Runoff_phase[1:end,2]
    Runoff_grey = Runoff_phase[1:end,3]
    years = c(2005,2006,2007,2008,2009,2010)
    # return from your function
    Runoff_phases = list(years,Runoff_living = Runoff_living, Runoff_red = Runoff_red, Runoff_grey = Runoff_grey)
    return(Runoff_phases)
}
