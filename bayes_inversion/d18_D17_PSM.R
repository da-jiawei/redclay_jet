ctrl = function(){
  vars = list(
    d18p = -15,
    MAP = 500,
    MAT = 15,
    PCQ.pf = 0.3,
    PCQ_to = 10,
    tsc = 0.3,
    lat = 35,
    pore = 0.35,
    tort = 0.7
  )
  return(vars)
}

PSM = function(vars){
  
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
  alpha18.diff = 1.028489   # diffusion fractionation factor (Merlivat 1978) 
  theta.mean = 0.065
  theta.diff = 0.5185
  alpha17.diff = alpha18.diff ^ theta.diff
  
  
  # Derived values ----
  PPCQ = MAP * PCQ.pf 
  TmPCQ = MAT + PCQ_to # air temperature of pedogenic carbonate quarter
  Ra = 42.608 - 0.3538 * abs(lat) # total radiation at the top of the atmosphere
  Rs = Ra * 0.16 * sqrt(12) # daily temperature range assumed to be 12
  
  # soil parameters ----
  ## Depth to carbonate formation based on  Retallack (2005) data
  z = (0.0925 * MAP + 13.4) / 100

  ## Soil temperatures at depth z
  d = sqrt((2 * 0.0007) / ((2 * 3.1415 / 3.154e7) * 0.3)) / 100
  Tsoil = MAT + PCQ_to * sin(2 * 3.1415 * tsc - z / d) / exp(z / d) 
  Tsoil.K = Tsoil + 273.15
  ## Potential Evapotranspiration - Hargreaves and Samani (1982) and Turc (1961)
  ha = pmin(0.95, 0.25 + 0.7 * (PPCQ / 900))
  PET_PCQ_D = ifelse(ha < 0.5, 
                     0.013 * (TmPCQ / (TmPCQ + 15)) * (23.885 * Rs + 50) * (1 + ((0.5 - ha) / 0.7)),
                     0.013 * (TmPCQ / (TmPCQ + 15)) * (23.885 * Rs + 50))
  PET_PCQ_D = pmax(PET_PCQ_D, 0.01)
  PET_PCQ = PET_PCQ_D * 90
  PET_D = ifelse(ha < 0.5, 
                 0.013 * (MAT / (MAT + 15)) * (23.885 * Rs + 50) * (1 + ((0.5 - ha) / 0.7)),
                 0.013 * (MAT / (MAT + 15)) * (23.885 * Rs + 50))
  PET_D = pmax(PET_D, 0.01)
  PET = PET_D * 365
  
  ## AET in mm/quarter from Budyko curve - Pike (1964)
  AET_PCQ = PPCQ * (1 / (sqrt(1 + (1 / ((PET_PCQ / (PPCQ)))) ^ 2)))
  ### Soil evaporation from AET
  ETR = 0.06
  E = ETR * AET_PCQ
  E = pmax(E, 1) # minimum of 1 mm
  E_s = E / (1000 * 90 * 24 * 3600) # soil evaporation rate in m/sec
  
  # oxygen isotope
  R18p = (d18p / 1000 + 1) * R18smow
  dp18p = log(R18p / R18smow) * 1000
  # dp17p = 40/1000 + 0.528 * dp18p
  # dp17p = 0.5268 * dp18p + 0.015 # Global meteoric water - Aron (2021)
  Dp17p = 10 # per meg
  dp17p = Dp17p / 1000 + 0.528 * dp18p
  R17p = exp(dp17p / 1000) * R17smow

  # equilibrium fractionation factor for vapor to liquid - (Horita and Wesolowski 1994)
  alpha18.eq = 1 / exp(((1.137e6 / ((TmPCQ + 273.15) ^ 2) - 0.4156e3/(TmPCQ + 273.15) - 2.0667) /1000))
  theta.eq = 0.529
  alpha17.eq = alpha18.eq ^ theta.eq
  
  # water vapor in air assumed to be in equilibrium with reservoir water
  R18a = R18p * alpha18.eq
  R17a = R17p * alpha17.eq
  
  Dsoil.eff = Dair * tort * (pore - theta.o) # The effective diffusivity of water vapor (m2/s)
  es = (0.611 * exp(17.502 * MAT / (MAT + 240.97))) * 1000 # saturated water vapor pressure from Tetens formula
  N.sat = 0.01802 * es / (Rgas * (TmPCQ + 273.15)) # saturated water vapor concentration at a given temperature
  z.bar = N.sat * Dsoil.eff / (E_s * rho) # penetration depth (m)
  z.ef = (1 - ha) * z.bar # the thickness of the water vapor phase region (m)
  
  # liquid water diffusivity (m2/s) (Easteal 1984)
  Dlo = exp(1.6766 + 1.6817 * (1000 / (TmPCQ + 273.15)) - 0.5773 * (1000 / (TmPCQ + 273.15))^2) * 10^-9 
  Dlo.eff = Dlo * pore * tort # effective diffusivity of liquid water (m2/s)
  z.hat = Dlo.eff / E_s # the decay length (mean penetration depth)
  
  # Evaporation front
  h.ef = ha + z.ef / z.bar # humidity at the evaporation front
  R18.ef = (alpha18.diff * R18p * (z.ef / z.bar) + ha * R18a) / (h.ef * alpha18.eq)
  R17.ef = (alpha17.diff * R17p * (z.ef / z.bar) + ha * R17a) / (h.ef * alpha17.eq)
  
  hs = min(ha + z / z.bar, 1)
  z.ef = max(z.ef, 1e-10)
  z.f = (theta.mean / a.theta) * log(z / z.ef) # the modified depth function
  
  if (z <= z.ef) {
    R18sw = (alpha18.diff * R18p * z / z.bar + ha * R18a) / (hs * alpha18.eq)
    R17sw = (alpha17.diff * R17p * z / z.bar + ha * R17a) / (hs * alpha17.eq)
  } else {
    R18sw = (R18.ef - R18p) * exp(-z.f / z.hat) + R18p
    R17sw = (R17.ef - R17p) * exp(-z.f / z.hat) + R17p
  }

  d18sw = ((R18sw / R18smow) - 1) * 1000
  dp18sw = log(R18sw / R18smow) * 1000
  dp17sw = log(R17sw / R17smow) * 1000
  Dp17sw = 1000 * (dp17sw - 0.528 * dp18sw)
  
  results = data.frame("d18p" = rep(d18p), "z" = rep(z), "RH" = rep(ha), "R18sw" = rep(R18sw),
                       "d18sw" = rep(d18sw), "dp18sw" = rep(dp18sw), "Dp17sw" = rep(Dp17sw))
}



