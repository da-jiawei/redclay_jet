ctrl = function(){
  vars = list(
    RH = 0.5,
    d18p = -20,
    Dp17p = 10,
    Tsoil = 20,
    f = seq(0.01, 1, 0.01)
  )
  return(vars)
}


IWB = function(vars){
  ## Unpack variables
  list2env(vars, environment())
  
  # constants  
  R18smow = 0.0020052
  R17smow = 0.0003799
  R18vpdb = 0.0020672
  R17vpdb = 0.0003860
  theta_eq = 0.529
  alpha18_diff = 1.028489
  theta_diff = 0.5185
  alpha17_diff = alpha18_diff^theta_diff
  
  ### The isotopic compositions of rainfall
  R18p = (d18p / 1000 + 1) * R18smow
  dp18p = log(R18p / R18smow) * 1000
  
  # GMWL.inter = rnorm(nsyth, 0.015, 0.002)
  # dp17p = 0.5268 * dp18p + 0.015 # Global meteoric water - Aron (2021)
  dp17p = dp18p * 0.528 + Dp17p / 1000
  R17p = exp(dp17p / 1000) * R17smow
  
  ### equilibrium fractionation coefficients
  ### assume Tsoil = Tair, as sensitivity analyses show little variations in response to temperature 
  Tsoil.K = Tsoil + 273.15
  alpha18_eq = exp((-2.0667 * 1e-3) - (0.4156 / Tsoil.K) + (1.137 * 1e3 / (Tsoil.K ^ 2)))
  alpha17_eq = alpha18_eq ^ theta_eq
  
  ### triple oxygen isotopes of water vapor (assumed to be in equilibrium of initial water)  
  R17a = R17p / alpha17_eq
  R18a = R18p / alpha18_eq 
  
  # the steady-state isotopic composition that the water may reach at low f
  R18wss = (alpha18_eq * RH * R18a) / (1 - alpha18_eq * alpha18_diff * (1 - RH))
  R17wss = (alpha17_eq * RH * R17a) / (1 - alpha17_eq * alpha17_diff * (1 - RH))
  
  # coefficient for isolated water body, assuming no turbulence
  u18 = (1 - alpha18_eq * alpha18_diff * (1 - RH)) / (alpha18_eq * alpha18_diff * (1 - RH))
  u17 = (1 - alpha17_eq * alpha17_diff * (1 - RH)) / (alpha17_eq * alpha17_diff * (1 - RH))
  
  # isotopic compositions of remaining soil water
  R18sw = f ^ u18 * (R18p - R18wss) + R18wss  #isotopic compositions of remain water
  R17sw = f ^ u17 * (R17p - R17wss) + R17wss
  
  dp18sw = 1000 * log(R18sw / R18smow)
  dp17sw = 1000 * log(R17sw / R17smow)
  
  Dp17sw = (dp17sw - 0.528 * dp18sw) * 1000
  
  # isotopic compositions of calcite in equilibrium with soil water
  alpha18_c_w_eq = exp((1.61e4 / Tsoil.K - 24.6) / 1e3) # Wostbrock (2020)
  theta_c_w = 0.5305 - 1.39 / Tsoil.K
  alpha17_c_w_eq = alpha18_c_w_eq ^ theta_c_w
  R18c = R18sw * alpha18_c_w_eq
  R17c = R17sw * alpha17_c_w_eq
  d18c = (R18c / R18vpdb - 1) * 1e3
  dp18c = log(R18c / R18vpdb) * 1e3
  dp17c = log(R17c / R17vpdb) * 1e3
  Dp17c = (dp17c - 0.528 * dp18c) * 1e3 # per meg
  D47c = 0.0391e6 / Tsoil.K ^ 2 + 0.154 # Andersen (2021)
  
  results = data.frame("f" = rep(f), "Tsoil" = rep(Tsoil), "d18c" = rep(d18c), "Dp17c" = rep(Dp17c))
  return(results)
}