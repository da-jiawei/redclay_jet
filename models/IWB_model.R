ctrl = function(){
  vars = list(
    RH = 0.8,
    d18p = -15,
    Dp17p = 10,
    Tsoil = 30,
    f = seq(0.01, 1, 0.01)
  )
  return(vars)
}

IWB = function(vars) {
  
  ## Unpack variables
  list2env(vars, environment())

  ### isotopic compositions of standard samples  
  R18_SMOW <- 0.0020052
  R17_SMOW <- 0.000401
  R2_SMOW <- 0.00015576
  
  ### The isotopic compositions of rainfall
  d18_i <- d18p
  R18_i <- (d18_i / 1000 + 1) * R18_SMOW
  dp18.p <- log(R18_i / R18_SMOW) * 1000
  # GMWL.slope <- rnorm(nsyth, 0.5268, 0.0002)
  # GMWL.inter <- rnorm(nsyth, 0.015, 0.002)
  dp17.p <- Dp17p / 1e3 + 0.528 * dp18.p
  # dp17.p <- 0.5268 * dp18.p + 0.015 # Global meteoric water - Aron (2021)
  # dp17.p <- 0.5292 * dp18.p + 0.0506 # Tap water Northern China - Tian (2017)
  # dp17.p <- dp18.p * 0.528 + Dp17.p / 1000
  R17_i <- exp(dp17.p/1000) * R17_SMOW
  d2_i <- 6.9689*d18_i + 0.0511 # modern meteoric water across the CLP
  # LMWL.slope <- rnorm(nsyth, 6.9689, 0.1830)
  # LMWL.inter <- rnorm(nsyth, 0.0512, 1.590)
  R2_i <- (d2_i / 1000 + 1) * R2_SMOW
  
  ### equilibrium fractionation coefficients
  theta_eq <- 0.529
  Tair <- Tsoil
  ### assume Tsoil = Tair, as sensitivity analyses show little variations in response to temperature 
  alpha18_eq <- exp((-2.0667*10^-3)-(0.4156/(Tsoil+273.15))+(1.137*10^3/((Tsoil+273.15)^2)))
  alpha17_eq <- alpha18_eq ^ theta_eq
  alpha2_eq <- exp((52.612*10^-3)-(76.248/(Tsoil+273.15))+(24.844*10^3/((Tsoil+273.15)^2)))
  
  alpha18_eq_air <- exp((-2.0667*10^-3)-(0.4156/(Tair+273.15))+(1.137*10^3/((Tair+273.15)^2))) # equilibrium fractionation in air
  alpha17_eq_air <- alpha18_eq ^ theta_eq
  alpha2_eq_air <- exp((52.612*10^-3)-(76.248/(Tair+273.15))+(24.844*10^3/((Tair+273.15)^2)))
  
  ### kinetic fractionation coefficients - diffusion
  alpha18_diff <- 1.028489
  theta_diff <- 0.5185
  alpha17_diff <- alpha18_diff^theta_diff
  alpha17_diff <- 1.014672
  alpha2_diff <- 1.025115
  
  ### triple oxygen isotopes of water vapor (assumed to be in equilibrium of initial water)  
  R17_a <- R17_i / alpha17_eq
  R18_a <- R18_i / alpha18_eq 
  R2_a <- R2_i / alpha2_eq
  
  # the steady-state isotopic composition that the water may reach at low f
  R18wss <- (alpha18_eq * RH * R18_a) / (1 - alpha18_eq * alpha18_diff * (1 - RH))
  R17wss <- (alpha17_eq * RH * R17_a) / (1 - alpha17_eq * alpha17_diff * (1 - RH))
  R2wss <- (alpha2_eq * RH * R2_a) / (1 - alpha2_eq * alpha2_diff * (1 - RH))
  
  # coefficient for isolated water body
  u18 <- (1 - alpha18_eq * alpha18_diff * (1 - RH)) / (alpha18_eq * alpha18_diff * (1 - RH))
  u17 <- (1 - alpha17_eq * alpha17_diff * (1 - RH)) / (alpha17_eq * alpha17_diff * (1 - RH))
  u2 <- (1 - alpha2_eq * alpha2_diff * (1 - RH)) / (alpha2_eq * alpha2_diff * (1 - RH))
  
  # isotopic compositions of remaining soil water
  R18sw <- f ^ u18 * (R18_i - R18wss) + R18wss  #isotopic compositions of remain water
  R17sw <- f ^ u17 * (R17_i - R17wss) + R17wss
  R2sw <- f ^ u2 * (R2_i - R2wss) + R2wss
  
  d18sw <- ((R18sw / R18_SMOW) - 1) * 1000
  d17sw <- ((R17sw / R17_SMOW) - 1) * 1000
  d2sw <- ((R2sw / R2_SMOW) - 1) * 1000
  
  dp18sw <- 1000 * log(R18sw / R18_SMOW)
  dp17sw <- 1000 * log(R17sw / R17_SMOW)
  
  Dp17sw <- (dp17sw - 0.528 * dp18sw) * 1000
  D2sw <- d2sw - 8 * d18sw
  
  results = data.frame("d18sw" = rep(d18sw), "dp18sw" = rep(dp18sw), 
                       "Dp17sw" = rep(Dp17sw),
                       "D2sw" = rep(D2sw),
                       "f" = f)
  # dat <- c(d18sw, dp18sw, Dp17sw, D2sw)
  # return(dat)
}