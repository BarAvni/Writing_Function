#' Runoff model
#' 
#' THis function compute runoff from evapotranspiration, infiltration and snow precipitation  


#' @param     Q (mm) effective rain/snow (mm)
#' @param     InfilCap (mm/day) constant infiltration capacity
#' @param     MaxStorage (mm) maximum capacity of ground storage
#' @param     ET (mm/day) evapotranspiration
#' @param     month
#' @param     year 
#' @author Bar Avni
#' @return monthly runoff depth (mm) 

runoffCalc =
  function(Q, InfilCap , MaxStorage, ET, month, year) {
    
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

#P = metdata_REP$P
#I = I
#month = metdata_REP$month
#InfilCap = 5
#MaxStorage = 10
#ET = ET

    # Set initial value for storage (assuming the soil is dry)
    month_diff = diff(month)
    month_diff[month_diff < 0] = 1
    month_index = which(month_diff %in% 1)
    month_index_end = c(month_index, length(month))
    month_index_start = c(1,(month_index_end[1:length(month_index_end) - 1]+1))
    
Runoff_month = rep(0, times=length(month_index_end))
      for (m in 1:length(month_index_start)) {
        Runoff = rep(0, times=length(month_index_start[m]:month_index_end[m]))
        storage = 0
        counter = 1
        for (i in month_index_start[m]:month_index_end[m]) {
          storage = storage + Q[i]
          if (storage > MaxStorage) {
            OverStorage = storage - MaxStorage
            storage = MaxStorage
            if (OverStorage > InfilCap) {
              Runoff[counter] = OverStorage - InfilCap
            }else {Runoff[counter] = 0 }
          }else {Runoff[counter] = 0 }
          storage = storage - ET[i]
          if (storage < 0) {storage = 0}
          counter = counter + 1
        }
        Runoff_month[m] = sum(Runoff)
      }
return(Runoff_month)
}
