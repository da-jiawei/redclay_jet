#### FUNCTIONS ----
# equilibrium isotope fractionation function
alpha_18_l_v = function(temp){
  exp((-7.685 + 6.7123*10^3/(temp+273.15) - 1.6664*10^6/(temp+273.15)^2 + 0.35041*10^9/(temp+273.15)^3)/1000)
}
delta_prime = function(d18, d17){
  dp18 = 1e3 * log(d18 / 1e3 + 1)
  dp17 = 1e3 * log(d17 / 1e3 + 1)
  Dp17 = 1e3 * (dp17 - .528 * dp18)
  return(Dp17)
}
# tetens equation
tetens = function(temp){
  6.112 * exp(17.67 * temp / (temp + 243.5)) * 100 # Tetens equation
}

#### MODEL INPUT PARAMETERS ----
ctrl = function(){
  vars = list(
    # atmosphere
    RH_air = 80,
    T_air = 20,
    T_air_half_range = 10,
    time_of_year = .25,
    d18_p = -15,
    Dp17_p = 10,
    
    # soil
    soil_depth = 30,
    pore = 0.99,
    tort = 0.99,
    D_H2O_air = 0.242, # diffusion rate under 20C - unit: cm2/s
    initial_water_content = 0.1,
    # model setup
    dz = .5,
    rundays = 1
  )
  return(vars)
}
soil_evap_model = function(vars) {
  list2env(vars, environment())
  
  # model setup
  z = seq(0, soil_depth, dz)
  soil_bttm = soil_depth/dz + 1
  D_H2O_max = ceiling(D_H2O_air * tort * pore * 1e2) / 1e2
  dt = dz^2 / (2 * D_H2O_max)
  
  #### CONSTANTS ----
  # triple oxygen
  R18_SMOW = 0.0020052
  R17_SMOW = 0.0003799
  theta_eq = 0.529
  alpha18_diff = 1.028489
  theta_diff = 0.5185
  alpha17_diff = alpha18_diff ^ theta_diff
  
  Rgas = 8.314462 # gas constant
  rho = 1 # water density - g/cm3
  
  #### INITIAL CONDITION ----
  # soil temperature profile
  omega = 2*pi    # unit - yr^-1
  Cv = 2.5e6      # J/m3/C
  kappa = 0.25*(365*24*60*60)   #J/yr/m/C
  d = (2*kappa/(Cv*omega))^0.5
  soilT = T_air + T_air_half_range * (sin(omega * time_of_year - (z / 100) / d)) / exp((z / 100) / d)
  
  # upper boundary
  atm_T = soilT[1]
  es_air = tetens(atm_T)
  e_air = es_air * RH_air / 100
  vapor_conc_air = e_air / (Rgas * (atm_T + 273.15)) # mol/m3
  vapor_conc_air = vapor_conc_air / 1e6 # mol/cm3
  
  cloud_T = 0
  alpha18_eq = alpha_18_l_v(cloud_T)
  d18_v = (d18_p + 1e3) / alpha18_eq - 1e3
  
  dp18_p = 1e3 * log(d18_p / 1e3 + 1)
  dp17_p = Dp17_p / 1e3 + dp18_p * .528
  d17_p = (exp(dp17_p / 1e3) - 1) * 1e3
  alpha17_eq = alpha18_eq ^ theta_eq
  d17_v = (d17_p + 1e3) / alpha17_eq - 1e3
  Dp17_v = delta_prime(d18_v, d17_v)
  
  R18_v_air = (d18_v / 1e3 + 1) * R18_SMOW
  R17_v_air = (d17_v / 1e3 + 1) * R17_SMOW
  
  H216O_v_air = vapor_conc_air / (1 + R18_v_air + R17_v_air)
  H217O_v_air = H216O_v_air * R17_v_air
  H218O_v_air = H216O_v_air * R18_v_air
  
  # initial values 
  water_content = rep(initial_water_content, soil_bttm) # unit - cm3/cm3
  water_content[1] = 0
  
  # intial soil liquid has isotopic composition equal to rain. Vapor is in
  # equilibrium with liquid at soil T. Vapor at surface is atm vapor
  d18_sw = rep(d18_p, length(z))
  alpha18_eq_sw = alpha_18_l_v(soilT)
  d18_sv = (d18_sw + 1e3) / alpha18_eq_sw - 1e3
  d18_sv[1] = d18_v
  R18_sv = (d18_sv / 1e3 + 1) * R18_SMOW
  d17_sw = rep(d17_p, length(z))
  alpha17_eq_sw = alpha18_eq_sw ^ theta_eq
  d17_sv = (d17_sw + 1e3) / alpha17_eq_sw - 1e3
  d17_sv[1] = d17_v
  R17_sv = (d17_sv / 1e3 + 1) * R17_SMOW
  Dp17_sv = delta_prime(d18_sv, d17_sv)
  
  d18_sv_initial = d18_sv
  d17_sv_initial = d17_sv
  Dp17_sv_initial = Dp17_sv
  
  # air filled pore space at 100% RH initially
  es_soil = tetens(soilT)
  svc = es_soil / (Rgas * (soilT + 273.15))
  svc = svc / 1e6 # mol/cm3
  H216O_sv = svc / (1 + R18_sv + R17_sv)
  H217O_sv = H216O_sv * R17_sv
  H218O_sv = H216O_sv * R18_sv
  H216O_sv[1] = H216O_v_air
  H217O_sv[1] = H217O_v_air
  H218O_sv[1] = H218O_v_air
  
  # bottom boundary
  H216O_sv_bttm = H216O_sv[soil_bttm]
  H217O_sv_bttm = H217O_sv[soil_bttm]
  H218O_sv_bttm = H218O_sv[soil_bttm]
  
  R18_sw = (d18_sw / 1e3 + 1) * R18_SMOW
  R17_sw = (d17_sw / 1e3 + 1) * R17_SMOW
  swc = water_content * rho * (1 / 18.01528) # cm3/cm3 * g/cm3 * mol/g = mol/cm3
  swc_bttm = swc[soil_bttm]
  
  H216O_sw = swc / (1 + R18_sw + R17_sw)
  H217O_sw = H216O_sw * R17_sw
  H218O_sw = H216O_sw * R18_sw
  H216O_sw_bttm = H216O_sw[soil_bttm]
  H217O_sw_bttm = H217O_sw[soil_bttm]
  H218O_sw_bttm = H218O_sw[soil_bttm]
  
  # H2O diffusion rate in soil pore space
  free_pore = pore - water_content # free air porosity
  D_H216O = D_H2O_air * free_pore * tort * ((soilT + 273.15) / (20 + 273.15)) ^ 1.823
  D_H218O = D_H216O / alpha18_diff
  D_H217O = D_H216O / alpha17_diff
  
  # preallocate these variables
  # H216O_sv_new = rep(0, soil_bttm)
  # H217O_sv_new = rep(0, soil_bttm)
  # H218O_sv_new = rep(0, soil_bttm)
  # H216O_sw_new = rep(0, soil_bttm)
  # H217O_sw_new = rep(0, soil_bttm)
  # H218O_sw_new = rep(0, soil_bttm)
  # swc_new = rep(0, soil_bttm)
  
  #### LOOP ----
  time_steps = floor((rundays * 24 * 3600) / dt)
  for (i in 1:(time_steps)) {
    for (j in 2:(soil_bttm - 1)) {
      # changes in vapor concentration under Fick's second law
      dH216Odz2 = (H216O_sv[j+1] - 2 * H216O_sv[j] + H216O_sv[j-1]) / dz^2
      dH217Odz2 = (H217O_sv[j+1] - 2 * H217O_sv[j] + H217O_sv[j-1]) / dz^2
      dH218Odz2 = (H218O_sv[j+1] - 2 * H218O_sv[j] + H218O_sv[j-1]) / dz^2
      
      H216O_sv_after_diff = H216O_sv[j] + D_H216O[j] * dH216Odz2 * dt
      H217O_sv_after_diff = H217O_sv[j] + D_H217O[j] * dH217Odz2 * dt
      H218O_sv_after_diff = H218O_sv[j] + D_H218O[j] * dH218Odz2 * dt
      
      d18_sv_after_diff = ((H218O_sv_after_diff / H216O_sv_after_diff) / R18_SMOW - 1) * 1e3
      d17_sv_after_diff = ((H217O_sv_after_diff / H216O_sv_after_diff) / R17_SMOW - 1) * 1e3
      svc_after_diff = H216O_sv_after_diff + H217O_sv_after_diff + H218O_sv_after_diff
      
      total_water = svc_after_diff * free_pore[j] + swc[j] # liquid water is still from the previous step
      svc_eq = es_soil[j] / (Rgas * (soilT[j] + 273.15))
      svc_eq = svc_eq / 1e6 # mol/cm3
      
      # equilibrate liquid and vapor
      if(total_water > svc_eq * free_pore[j]) {
        swc_after_eq = total_water - svc_eq * free_pore[j]
        d18_total_water = (d18_sv_after_diff * svc_after_diff * free_pore[j] + d18_sw[j] * swc[j]) / total_water
        d17_total_water = (d17_sv_after_diff * svc_after_diff * free_pore[j] + d17_sw[j] * swc[j]) / total_water
        Fv = (svc_eq * free_pore[j]) / total_water # fraction of vapor after equilibrium
        
        d18_sw_after_eq = (d18_total_water * alpha18_eq_sw[j] + Fv * 1e3 * (alpha18_eq_sw[j] - 1)) / (alpha18_eq_sw[j] * (1 - Fv) + Fv)
        d18_sv_after_eq = (d18_sw_after_eq + 1e3) / alpha18_eq_sw[j] - 1e3
        d17_sw_after_eq = (d17_total_water * alpha17_eq_sw[j] + Fv * 1e3 * (alpha17_eq_sw[j] - 1)) / (alpha17_eq_sw[j] * (1 - Fv) + Fv)
        d17_sv_after_eq = (d17_sw_after_eq + 1e3) / alpha17_eq_sw[j] - 1e3
        
        # update vapor and liquid isotopes
        R18_sv_after_eq = (d18_sv_after_eq / 1e3 + 1) * R18_SMOW
        R17_sv_after_eq = (d17_sv_after_eq / 1e3 + 1) * R17_SMOW
        H216O_sv[j] = svc_eq / (1 + R18_sv_after_eq + R17_sv_after_eq)
        H217O_sv[j] = H216O_sv[j] * R17_sv_after_eq
        H218O_sv[j] = H216O_sv[j] * R18_sv_after_eq
        R18_sw_after_eq = (d18_sw_after_eq / 1e3 + 1) * R18_SMOW
        R17_sw_after_eq = (d17_sw_after_eq / 1e3 + 1) * R17_SMOW
        H216O_sw[j] = swc_after_eq / (1 + R18_sw_after_eq + R17_sw_after_eq)
        H217O_sw[j] = H216O_sw[j] * R17_sw_after_eq
        H218O_sw[j] = H216O_sw[j] * R18_sw_after_eq
        
        water_content[j] = swc_after_eq * 18.01528 / rho  # unit - mol/cm3 * g/mol * cm3/g
        
      } else {
        swc[j] = 0
        H216O_sw[j] = 0
        H217O_sw[j] = 0
        H218O_sw[j] = 0
        water_content[j] = 0
        
        H216O_sv[j] = H216O_sv_after_diff
        H217O_sv[j] = H217O_sv_after_diff
        H218O_sv[j] = H218O_sv_after_diff
      }
      
      # update free air porosity and diffusion rate
      free_pore[j] = pore - water_content[j]
      D_H216O[j] = D_H2O_air * free_pore[j] * tort * ((soilT[j] + 273.15) / (20 + 273.15)) ^ 1.823
      D_H217O[j] = D_H216O[j] / alpha17_diff
      D_H218O[j] = D_H216O[j] / alpha18_diff
    }
    # upper and lower boundary held constant
    H216O_sv[1] = H216O_v_air
    H217O_sv[1] = H217O_v_air
    H218O_sv[1] = H218O_v_air
    H216O_sw[1] = 0
    H217O_sw[1] = 0
    H218O_sw[1] = 0
    H216O_sv[soil_bttm] = H216O_sv_bttm
    H217O_sv[soil_bttm] = H217O_sv_bttm
    H218O_sv[soil_bttm] = H218O_sv_bttm
    H216O_sw[soil_bttm] = H216O_sw_bttm
    H217O_sw[soil_bttm] = H217O_sw_bttm
    H218O_sw[soil_bttm] = H218O_sw_bttm
    
    d18_sw = ((H218O_sw / H216O_sw) / R18_SMOW - 1) * 1e3
    d17_sw = ((H217O_sw / H216O_sw) / R17_SMOW - 1) * 1e3
    Dp17_sw = delta_prime(d18_sw, d17_sw)
  }
  return(results = data.frame(
    z = z, d18_sw = d18_sw, d17_sw = d17_sw, Dp17_sw = Dp17_sw
  ))
}
