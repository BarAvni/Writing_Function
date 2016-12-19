
cc = function(gmax, LAI, stoden, Tair) {
  cond = gmax*LAI*stoden
  cond=ifelse(Tair < 0, 0, cond)
 
  return(cond)
}
