model{
  # Data model ----
  for(i in 1:length(ages)){
    d18c.obs[i, 1] ~ dnorm(d18c[i], d18c.pre[i])
    d18c.pre[i] = 1 / d18c.obs[i, 2] ^ 2
  }
  
  for(i in 1:length(ages)){
    D47c.obs[i, 1] ~ dnorm(D47c[i], D47c.pre[i])
    D47c.pre[i] = 1 / D47c.obs[i, 2] ^ 2
  }
  
  for (i in 1:length(ages)) {
    Dp17c.obs[i, 1] ~ dnorm(Dp17c[i], Dp17c.pre[i])
    Dp17c.pre[i] = 1 / Dp17c.obs[i, 2] ^ 2
  }
  
  # Isolated water body model ----
  for (i in 1:length(ages)) {
    ### The isotopic compositions of rainfall
    R18p[i] = (d18p[i] / 1000 + 1) * R18smow
    dp18p[i] = log(R18p[i] / R18smow) * 1000
    
    # GMWL.inter = rnorm(nsyth, 0.015, 0.002)
    # dp17p[i] = 0.5268 * dp18p[i] + 0.015 # Global meteoric water - Aron (2021)
    dp17p[i] = dp18p[i] * 0.528 + Dp17p[i] / 1000
    R17p[i] = exp(dp17p[i] / 1000) * R17smow

    ### equilibrium fractionation coefficients
    ### assume Tsoil = Tair, as sensitivity analyses show little variations in response to temperature 
    Tsoil.K[i] = Tsoil[i] + 273.15
    alpha18_eq[i] = exp((-2.0667 * 1e-3) - (0.4156 / Tsoil.K[i]) + (1.137 * 1e3 / (Tsoil.K[i] ^ 2)))
    alpha17_eq[i] = alpha18_eq[i] ^ theta_eq

    ### triple oxygen isotopes of water vapor (assumed to be in equilibrium of initial water)  
    R17a[i] = R17p[i] / alpha17_eq[i]
    R18a[i] = R18p[i] / alpha18_eq[i] 

    # the steady-state isotopic composition that the water may reach at low f
    R18wss[i] = (alpha18_eq[i] * RH[i] * R18a[i]) / (1 - alpha18_eq[i] * alpha18_diff * (1 - RH[i]))
    R17wss[i] = (alpha17_eq[i] * RH[i] * R17a[i]) / (1 - alpha17_eq[i] * alpha17_diff * (1 - RH[i]))

    # coefficient for isolated water body, assuming no turbulence
    u18[i] = (1 - alpha18_eq[i] * alpha18_diff * (1 - RH[i])) / (alpha18_eq[i] * alpha18_diff * (1 - RH[i]))
    u17[i] = (1 - alpha17_eq[i] * alpha17_diff * (1 - RH[i])) / (alpha17_eq[i] * alpha17_diff * (1 - RH[i]))

    # isotopic compositions of remaining soil water
    R18sw[i] = f[i] ^ u18[i] * (R18p[i] - R18wss[i]) + R18wss[i]  #isotopic compositions of remain water
    R17sw[i] = f[i] ^ u17[i] * (R17p[i] - R17wss[i]) + R17wss[i]
    
    # isotopic compositions of calcite in equilibrium with soil water
    alpha18_c_w_eq[i] = exp((1.61e4 / Tsoil.K[i] - 24.6) / 1e3) # Wostbrock (2020)
    theta_c_w[i] = 0.5305 - 1.39 / Tsoil.K[i]
    alpha17_c_w_eq[i] = alpha18_c_w_eq[i] ^ theta_c_w[i]
    R18c[i] = R18sw[i] * alpha18_c_w_eq[i]
    R17c[i] = R17sw[i] * alpha17_c_w_eq[i]
    d18c[i] = (R18c[i] / R18vpdb - 1) * 1e3
    dp18c[i] = log(R18c[i] / R18vpdb) * 1e3
    dp17c[i] = log(R17c[i] / R17vpdb) * 1e3
    Dp17c[i] = (dp17c[i] - 0.528 * dp18c[i]) * 1e3 # per meg
    D47c[i] = 0.0391e6 / Tsoil.K[i] ^ 2 + 0.154 # Andersen (2021)
  }
  
  # priors of environmental parameters ----
  for (i in 1:length(ages)) {
    d18p[i] ~ dunif(-35, -20)
    Dp17p[i] ~ dunif(20, 50)
    RH[i] ~ dunif(0.5, 0.8)
    f[i] ~ dunif(0.1, 0.5)
    Tsoil[i] ~ dunif(10, 30)
  }
  
  # constants  
  R18smow = 0.0020052
  R17smow = 0.0003799
  R18vpdb = 0.0020672
  R17vpdb = 0.0003860
  theta_eq = 0.529
  alpha18_diff = 1.028489
  theta_diff = 0.5185
  alpha17_diff = alpha18_diff^theta_diff
}