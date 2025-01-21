ctrl = function(){
  vars = list(
    RH = 0.5,
    d18p = -10,
    MAST = 10,
    MSST = 25,
    depth = 50,
    t = 3,
    evap = 5e-10
  )
  return(vars)
}

BASS = function(vars){
  
  list2env(vars, environment())
  
  # coefficient  
  R18smow = 0.0020052
  R17smow = 0.0003799
  R18vpdb = 0.0020672
  R17vpdb = 0.0003860
  Rgas = 8.314462 # gas constant
  theta.o = 0.05 # disconnected water content
  rho = 1000 # liquid water density (kg/m3)
  Dair = 2.44E-05 # water vapor diffusivity in air (m2/s) (Merlivat, 1978)
  a.theta = 0.05 # rate of increase of water content with depth (m-1)
  
  R18p = (d18p / 1000 + 1) * R18smow
  dp18p = log(R18p / R18smow) * 1000
  # dp17p = 40/1000 + 0.528 * dp18p
  dp17p = 0.5268 * dp18p + 0.015 # Global meteoric water - Aron (2021)
  R17p = exp(dp17p / 1000) * R17smow
  pore = 0.45 # for loess
  tort = 0.7
  theta.mean = 0.065
  dT = MSST - MAST
  airT = MAST + dT * sin(2 * pi * t/12)
  airT.K = airT + 273.15
  
  # equilibrium fractionation factor for vapor to liquid - (Horita and Wesolowski 1994)
  alpha18.eq = 1 / exp(((1.137e6 / (airT.K ^ 2) - 0.4156e3/airT.K - 2.0667) /1000))
  theta.eq = 0.529
  alpha17.eq = alpha18.eq ^ theta.eq
  
  # water vapor in air assumed to be in equilibrium with reservoir water
  R18a = R18p * alpha18.eq
  R17a = R17p * alpha17.eq
  
  # diffusion fractionation factor (Merlivat 1978)
  alpha18.diff = 1.028489
  theta.diff = 0.5185
  alpha17.diff = alpha18.diff ^ theta.diff
  
  Dsoil.eff = Dair * tort * (pore - theta.o) # The effective diffusivity of water vapor (m2/s)
  es = (0.611 * exp(17.502 * airT / (airT + 240.97))) * 1000 # saturated water vapor pressure from Tetens formula
  N.sat = 0.01802 * es / (Rgas * airT.K) # saturated water vapor concentration at a given temperature
  z.bar = N.sat * Dsoil.eff / (evap * rho) # penetration depth (m)
  z.ef = (1 - RH) * z.bar # the thickness of the water vapor phase region (m)
  
  # liquid water diffusivity (m2/s) (Easteal 1984)
  Dlo = exp(1.6766 + 1.6817 * (1000 / airT.K) - 0.5773 * (1000 / airT.K)^2) * 10^-9 
  Dlo.eff = Dlo * pore * tort # effective diffusivity of liquid water (m2/s)
  z.hat = Dlo.eff / evap # the decay length (mean penetration depth)
  
  # Evaporation front
  h.ef = RH + z.ef / z.bar # humidity at the evaporation front
  R18.ef = (alpha18.diff * R18p * (z.ef / z.bar) + RH * R18a) / (h.ef * alpha18.eq)
  R17.ef = (alpha17.diff * R17p * (z.ef / z.bar) + RH * R17a) / (h.ef * alpha17.eq)
  
  z = depth / 100 # in meter unit
  hs = min(RH + z / z.bar, 1)
  z.ef = max(z.ef, 1e-10)
  z.f = (theta.mean / a.theta) * log(z / z.ef) # the modified depth function
  
  R18 = (R18.ef - R18p) * exp(-z.f / z.hat) + R18p # Zimmermann et al. (1967)
  R17 = (R17.ef - R17p) * exp(-z.f / z.hat) + R17p  
  
  if (z <= z.ef) {
    R18sw = (alpha18.diff * R18p * z / z.bar + RH * R18a) / (hs * alpha18.eq)
    R17sw = (alpha17.diff * R17p * z / z.bar + RH * R17a) / (hs * alpha17.eq)
  } else {
    R18sw = (R18.ef - R18p) * exp(-z.f / z.hat) + R18p
    R17sw = (R17.ef - R17p) * exp(-z.f / z.hat) + R17p
  }

  d18sw = ((R18sw / R18smow) - 1) * 1000
  dp18sw = log(R18sw / R18smow) * 1000
  dp17sw = log(R17sw / R17smow) * 1000
  Dp17sw = 1000 * (dp17sw - 0.528 * dp18sw)
  
  results = data.frame("d18sw" = rep(d18sw), "dp18sw" = rep(dp18sw), "Dp17sw" = rep(Dp17sw))
}

vars = ctrl()
BASS(vars)
