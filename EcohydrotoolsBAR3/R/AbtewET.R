#' Abtew’s (1996) simple evapotranspiration model 
#'
#' Calculates evapotranspiration as fn. of solar radiation and latent heat of vaporization of water 
#'
#' @param     Rs    (MJ/m^2/day C) solar radiation 
#' @param     k         () coefficien (0.53)
#' @param     lambada     (MJ/kg) latent heat of vaporization of water 
#' @author    Bar Avni 
#' @return    evapotranspiration (mm/day) 

AbtewET = 
  function(Rs, k = 0.53, lambada = 334*0.001){
    Rs = Ra
    k = 0.53
    lambada = 334*0.001  

    # Evapotranspiration calculation 
    ET = (k*Rs)/lambada
    # return
    ET
  }




