#' SCS - CN model to compute affective rain
#' 
#' @param P annual precip
#' @param CN   
#' @return effective rain 


SCS_CN = function(P, CN ) {
  
  P = metdata_REP$P
  CN = 80  
 #  S is related to the soil and cover conditions of the watershed through the CN.
 # CN has a range of 0 to 100, and S is related to CN by
  
  S = (25400/CN) - 250 # mm
  
# Initial abstraction (Ia) is all losses before runoff begins. 
# It includes water intercepted by vegetation. 
  
  Ia = 0.2*S
  
# Compute effective rain
  
  Q = ((P - Ia)^2)/(P - Ia - S)
  return(Q)
}

