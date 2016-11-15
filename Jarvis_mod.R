#'  Modified Jarvis Equation
#'  
#'  This funciton compute surface conductance based on the Jarvis equation, 
#'  as function of tempature.


#'  @param Tair (deg C) leaf temperature
#'  @param Kt 
#'  @param Tx 
#'  @param gsmax (s/mm)  maximum surface conductance - the maximum stomatal conductance of the plant

#'  @author Bar Avni
#'  @return gs surface conductance (s/mm)

Jarvis_mod =
  function(Tair,gsmax, Kt, Tx) {
    
    #       Internal Variables
    
    #       tk       (deg K)   
    #       Ta       (deg K) 

    # A simple representation of  tempature function in Jarvis equation (Dickinson, 1984)
    tk = Tair + 273.15 
   

    Ta = (1-Kt*(Tx-tk)^2)^(-1)
    
    for (i in 1:length(Ta)) {
    if (Ta[i] > 0) {
      Ta[i] = 1 
     } else {
      Ta[i] = 0 
     }
    }
  
    #Ta = Ta + 273.15
    gs = gsmax*Ta
    # return from your function
    return(gs)
  }