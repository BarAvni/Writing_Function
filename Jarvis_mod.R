#'  Modified Jarvis Equation
#'  
#'  This funciton compute surface conductance based on the Jarvis equation, 
#'  as function of tempature.
czxc

#'  @param Ti (deg C) leaf temperature
#'  @param Tmin (deg C) leaf temperature
#'  @param Tmax (deg C) leaf temperature
#'  @param Topt (deg C) leaf temperature
#'  @param gsmax (s/mm)  maximum surface conductance

#'  @author Bar Avni
#'  @return surface conductance (s/mm)

Jarvis_mod =
  function(Ti,Tmin,Tmax,Topt,gsmax) {
    
    #       Internal Variables
    
    #       b       () 
    
    #
    b = (Tmax-Topt)/(Topt-Tmin)
    Ta = (Ti-Tmin)/(Topt-Tmin)*((Tmax-Ti)/(Tmax-Topt))^b
    
    if (Ta > 0) {
      Ta = 1 
    } else {
      Ta = 0 
    }
    
    gs = gsmax*Ta
    # return from your function
    gs
  }