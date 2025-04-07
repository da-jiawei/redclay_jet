## For gamma distribution
# plot function - precision instead of sd
pg = function(shp, rt){
  pre = rgamma(1e6, shp, rt)
  sd = sqrt(1/pre)
  plot(density(sd))
}
pg(10, 1e2)

## For beta distribution
pb = function(shp, rt){
  pre = rbeta(1e6, shp, rt)
  plot(density(pre))
}
pb(0.65 * 50 / 0.35, 50) 
pb(5, 2)
## For normal distribution
pn = function(mean, sd){
  pre = rnorm(1e6, mean, sd)
  plot(density(pre))
}
pn(300, 50)
