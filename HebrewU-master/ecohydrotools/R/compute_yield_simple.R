
#' compute annual yield'
#' 
#' Function to compute yeild of different fruits as a function of annual temperature and precipitation
#' @param T annual temperature (C)
#' @param Topt optimal temperature (C)
#' @param P annual precipitation (mm)
#' @param max.water  maximum water requirement (mm)
#' @param base.yield baseline yield  (kg)
#' @param irr  irrigation per year (mm/year) 
#' @param Tslope yield increase per C (default 0.4)
#' @param Pslope yield increase per mm water (default 0.5)
#' @examples compute_yield(T=20, P=250, irr=200, base.yield=500, Topt=25, max.water=800)
#' @return yield in kg


compute_yield_simple = function(T, P, irr, max.water, Topt, base.yield, Tslope=0.4, Pslope=0.5) {



nyears=length(T)

# compute with irrigation
irr.peryear = rep(irr, times=nyears)
water.input = P+irr.peryear;
yield = ifelse(water.input < max.water,
	Pslope*water.input - Tslope*abs(T-Topt) + base.yield,
	Pslope*max.water - Tslope*abs(T-Topt) + base.yield )
yield=pmax(yield,0)

#now computed without irrigation
water.input = P;
yield.noirr = ifelse(water.input < max.water,
               Pslope*water.input - Tslope*abs(T-Topt) + base.yield,
               Pslope*max.water - Tslope*abs(T-Topt) + base.yield )
yieldnoirr=pmax(yield.noirr,0)


return(list(yield=yield, yield.no.irrigation=yieldnoirr))


}

