canopyInterception <-
function(Tair, LAI, P, Smean = 6.6, L0 = 0) {
    
    #       Internal Variables
    #
    #       Il     (mm SWE)  interception before loading occure
    #       Lstar  (mm SW)   maximum intercepted snow load that can be retaind by the forest cannopy given current canopy structure and temperature conditions
    #       k      ()        proportionality factor
    #       S      (kg/m^2)  maximum snow load per unit branch area 
    #       ps     (kg/m^3)  fresh snow density

    
    
    # Fresh snow density (ps) as a fn. of air temp
    ps = 67.92 + 51.25*exp(Tair/2.59)
    
    # Maximum snow load per unit branch area (S) as a fn. of snow loading coefficient (Smean) and ps
    S = Smean*(0.27+(46/ps))
    
    # Maximum intercepted snow load (Lstar) as a fn. of S and LAI
    Lstar = S*LAI
    
    # Proportionality factor (k) as a fn. of Lstar
    k = 1/Lstar
    
    # Interception before loading occure (Il) as a fn. of Lstar, initial snow load (L0), k and snow precipitation (P)
    (Lstar - L0)*(1- exp(-k*P))
    
    # Canopy interception (I) as a fn. of Il and dimensionless unloading coefficient (c) 
    I = Il*c
 
    # return from your function
    I
  }
