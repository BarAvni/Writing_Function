runoffCalc <-
function(P, I, InfilCap, MaxStorage, ET) {
    
    #       Internal Variables
    #
    #       Pfc         (mm)  snowpack on the ground
    #       storage     (mm)  ground storage before runoff could start
    #       OverStorage (mm) 

    # Calculate the snowpack on the ground (Pfc) (snow percipitation (P) - canopy snow interseption (I))
    Pfc  = P - I
    # Set initial value for storage (assuming the soil is dry)
    storage = 0
    
    
for (i in 1:length(P)) {
  storage = storage + P[i]
    if (storage > MaxStorage) {
      OverStorage = storage - MaxStorage
      storage = storage - OverStorage
      if (OverStorage > InfilCap) {
        Runoff[i] = OverStorage - InfilCap
      }
    }
  storage = storage - ET
}
      
    # return from your function
    Runoff
}
