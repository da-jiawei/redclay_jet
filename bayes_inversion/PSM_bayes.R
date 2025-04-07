model{

  # Data model ----
  for(i in 1:length(d18Oc.ai)){
    d18Oc.obs[i, 1] ~ dnorm(d18c[d18Oc.ai[i]], d18Oc.pre[i])
    d18Oc.pre[i] = 1 / d18Oc.obs[i, 2] ^ 2
  }
  
  for(i in 1:length(D47c.ai)){
    D47c.obs[i, 1] ~ dnorm(D47c[D47c.ai[i]], D47c.pre[i])
    D47c.pre[i] = 1 / D47c.obs[i, 2] ^ 2
  }
  
  for (i in 1:length(Dp17c.ai)) {
    Dp17c.obs[i, 1] ~ dnorm(Dp17c[Dp17c.ai[i]], Dp17c.pre[i])
    Dp17c.pre[i] = 1 / Dp17c.obs[i, 2] ^ 2
  }
  
  for(i in 1:length(ai)){  
    # Soil carbonate ----
    ## Depth to carbonate formation based on Retallack (2005) data, meters
    z.min[i] = MAP[i] * 0.0925 + 13.4
    z.thick[i] = abs(PPCQ[i] - MAP[i] / 4) * 0.74 + 17.3
    z.mean[i] = z.min[i] + z.thick[i] / 2
    z.beta[i] = z.mean[i] / (22 ^ 2)
    z.alpha[i] = z.mean[i] * z.beta[i]
    z[i] ~ dgamma(z.alpha[i], z.beta[i])
    z_m[i] = z[i] / 100

    ## Soil temperatures at depth z
    Tsoil[i] = MAT[i] + (PCQ_to[i] * sin(2 * 3.141593 * tsc - z[i] / d)) / exp(z[i] / d) 
    Tsoil.K[i] = Tsoil[i] + 273.15
    
    ## Potential Evapotranspiration - Hargreaves and Samani (1982) and Turc (1961)
    Tair_PCQ[i] = MAT[i] + PCQ_to[i] * sin(2 * 3.141593 * tsc)
    PET_PCQ_D.1[i] = ifelse(ha[i] < 0.5, 
                       0.013 * (Tair_PCQ[i] / (Tair_PCQ[i] + 15)) * (23.885 * Rs + 50) * (1 + ((0.5 - ha[i]) / 0.7)),
                       0.013 * (Tair_PCQ[i] / (Tair_PCQ[i] + 15)) * (23.885 * Rs + 50))
    PET_PCQ_D[i] = max(PET_PCQ_D.1[i], 0.01)
    PET_PCQ[i] = PET_PCQ_D[i] * 90
    
    PET_D.1[i] = ifelse(ha[i] < 0.5, 
                            0.013 * (MAT[i] / (MAT[i] + 15)) * (23.885 * Rs + 50) * (1 + ((0.5 - ha[i]) / 0.7)),
                            0.013 * (MAT[i] / (MAT[i] + 15)) * (23.885 * Rs + 50))
    PET_D[i] = max(PET_D.1[i], 0.01)
    PET[i] = PET_D[i] * 365
    
    ## AET in mm/quarter from Budyko curve - Pike (1964)
    PPCQ[i] = MAP[i] * PCQ_pf[i] 
    AET_var[i] ~ dgamma(1 / 0.2 ^ 2, 1 / 0.2 ^ 2) # noise parameter - Gentine (2012)
    AET_PCQ[i] = PPCQ[i] * (1 / (sqrt(1 + (1 / ((PET_PCQ[i] / (PPCQ[i])) * AET_var[i])) ^ 2)))
    
    ## Average rooting depth
    AI[i] = PET[i] / MAP[i]
    L[i] = ifelse(AI[i] < 1.4, (-2 * AI[i]^2 + 2.5 * AI[i] + 1) * 100, 60)
    
    ## Oxygen isotopes ----
    ### Rainfall isotopes
    R18p[i] = (d18p[i] / 1e3 + 1) * R18smow
    dp18p[i] = log(R18p[i] / R18smow) * 1e3
    dp17p[i] = dp18p[i] * 0.528 + Dp17p[i]
    R17p[i] = exp(dp17p[i] / 1e3) * R17smow
    
    ### Equilibrium fractionation (Horita and Wesolowski 1994)
    alpha18.eq[i] = 1 / exp(((1.137e6 / (Tsoil.K[i] ^ 2) - 0.4156e3/Tsoil.K[i] - 2.0667) /1e3))
    alpha17.eq[i] = alpha18.eq[i] ^ theta.eq
    
    ### Atmospheric water vapor isotopes
    R18a[i] = R18p[i] * alpha18.eq[i]
    R17a[i] = R17p[i] * alpha17.eq[i]
    
    ### Soil evaporation from AET
    E1[i] = ETR * AET_PCQ[i]
    E[i] = max(E1[i], 1) # minimum of 1 mm
    E_s[i] = E[i] / (1e3 * 90 * 24 * 3600) # soil evaporation rate in m/sec
    
    ### Water vapor diffusivity
    es[i] = (0.611 * exp(17.502 * Tsoil[i] / (Tsoil[i] + 240.97))) * 1e3 # saturated water vapor pressure from Tetens formula
    N.sat[i] = 0.01802 * es[i] / (Rgas * Tsoil.K[i]) # saturated water vapor concentration at a given temperature
    Dv.soil[i] = Dair * tort * (pore - theta.o) # effective diffusivity of water vapor in soil (m2/s)
    z.bar[i] = N.sat[i] * Dv.soil[i] / (E_s[i] * rho) # penetration depth (m)
    z.ef1[i] = (1 - ha[i]) * z.bar[i] # the thickness of the water vapor phase region (m)
    z.ef[i] = max(z.ef1[i], 1e-10)
    
    ### Liquid water diffusivity (m2/s) (Easteal 1984)
    Dl[i] = exp(1.6766 + 1.6817 * (1e3 / Tsoil.K[i]) - 0.5773 * (1e3 / Tsoil.K[i]) ^ 2) * 1e-9 
    Dl.soil[i] = Dl[i] * pore * tort # effective diffusivity of liquid water (m2/s)
    z.hat[i] = Dl.soil[i] / E_s[i] # the decay length (mean penetration depth)
    
    ### The evaporation front
    h.ef[i] = ha[i] + z.ef[i] / z.bar[i] # humidity at the evaporation front
    R18.ef[i] = (alpha18.diff * R18p[i] * (z.ef[i] / z.bar[i]) + 
                   ha[i] * R18a[i]) / (h.ef[i] * alpha18.eq[i]) # isotopic composition at the evaporation front
    R17.ef[i] = (alpha17.diff * R17p[i] * (z.ef[i] / z.bar[i]) + 
                   ha[i] * R17a[i]) / (h.ef[i] * alpha17.eq[i])
    
    
    ### Isotope composition of soil water at depth z
    hs[i] = min(ha[i] + z_m[i] / z.bar[i], 1)
    z.f[i] = (theta.mean / a.theta) * log(z_m[i] / z.ef[i]) # the modified depth function
    R18s[i] = ifelse(z_m[i] <= z.ef[i], 
                      (alpha18.diff * R18p[i] * z_m[i] / z.bar[i] + ha[i] * R18a[i]) / 
                        (hs[i] * alpha18.eq[i]),
                      (R18.ef[i] - R18p[i]) * exp(-z.f[i] / z.hat[i]) + R18p[i])
    R17s[i] = ifelse(z_m[i] <= z.ef[i], 
                     (alpha17.diff * R17p[i] * z_m[i] / z.bar[i] + ha[i] * R17a[i]) / 
                       (hs[i] * alpha17.eq[i]),
                     (R17.ef[i] - R17p[i]) * exp(-z.f[i] / z.hat[i]) + R17p[i])
    
    ### Isotope composition of soil carbonate
    alpha18_c_w_eq[i] = exp((1.61e4 / Tsoil.K[i] - 24.6) / 1e3) # Wostbrock (2020)
    theta_c_w[i] = 0.5305 - 1.39 / Tsoil.K[i]
    alpha17_c_w_eq[i] = alpha18_c_w_eq[i] ^ theta_c_w[i]
    R18c[i] = R18s[i] * alpha18_c_w_eq[i]
    R17c[i] = R17s[i] * alpha17_c_w_eq[i]
    d18c[i] = (R18c[i] / R18vpdb - 1) * 1e3
    dp18c[i] = log(R18c[i] / R18vpdb) * 1e3
    dp17c[i] = log(R17c[i] / R17vpdb) * 1e3
    Dp17c[i] = (dp17c[i] - 0.528 * dp18c[i]) * 1e3 # per meg
    D47c[i] = 0.0391e6 / Tsoil.K[i] ^ 2 + 0.154 # Andersen (2021)
  }
  
  for(i in 1:length(ai)){
    # Time dependent variables ----
    ## Primary environmental ----
    MAT[i] ~ dunif(5, 20) # mean annual temperature
    PCQ_to[i] ~ dunif(7, 15)
    MAP[i] ~ dunif(200, 700) # mean annual precipitation, mm
    PCQ_pf[i] ~ dunif(0.1, 0.5) # PCQ precipitation fraction
    d18p[i] ~ dunif(-25, -15)
    Dp17p[i] ~ dunif(-5, 20)
    
    ## Secondary soil ----
    # tsc[i] ~ dunif(0.1, 0.6)
    h_m[i] = min(0.95, 0.25 + 0.7 * (PPCQ[i] / 900))
    ha[i] ~ dbeta(h_m[i] * 100 / (1 - h_m[i]), 100) # PCQ atmospheric humidity
    # ETR[i] ~ dbeta(0.06 * 1000 / 0.94, 1000) # Soil evaporation / AET
    # theta.mean[i] ~ dunif(0.05, 0.5) # mean water content
    # pore[i] ~ dunif(0.45, 0.54) # soil porosity
  }
  
  # Not time dependent ----
  lat = 36 # terrestrial site latitude
  Ra = 42.608 - 0.3538 * abs(lat) # total radiation at the top of the atmosphere
  Rs = Ra * 0.16 * sqrt(12) # daily temperature range assumed to be 12

  ## Constants ----
  d = sqrt((2 * 0.0007) / ((2 * 3.1415 / 3.154e7) * 0.3)) # decay depth
  ### Isotope ratio constants
  R18smow = 0.0020052
  R17smow = 0.0003799
  R18vpdb = 0.0020672
  R17vpdb = 0.0003860
  alpha18.diff = 1.028489   # diffusion fractionation factor (Merlivat 1978) 
  theta.diff = 0.5185
  alpha17.diff = alpha18.diff ^ theta.diff
  theta.eq = 0.529
  
  tsc = 0.3
  ETR = 0.06
  pore = 0.4 # soil porosity
  tort = 0.7 # soil tortuosity
  theta.mean = 0.065 # mean water content
  theta.o = 0.05 # disconnected water content
  a.theta = 0.05 # rate of increase of water content with depth (m-1) (Barnes and Allison, 1983)
  Rgas = 8.314462 # gas constant
  rho = 1000 # liquid water density (kg/m3)
  Dair = 2.44E-05 # water vapor diffusivity in air (m2/s) (Merlivat, 1978)
}
  