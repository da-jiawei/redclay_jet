rm(list = ls())
pacman::p_load(tidyverse, readxl)
d13Catm = read_csv("data/global_records/d13Ca_tipple.csv") |> filter(age <= 8)
CO2atm = read_csv("data/global_records/age_co2_plot_data.csv")[, c(4, 7:9)] |>
  mutate(age = age / 1e3)
# tooth enamel
d13Catm$co2 = approx(CO2atm$age, CO2atm$co2, xout = d13Catm$age)$y
d13te = d13Catm |>
  drop_na() |>
  mutate(D13_wet = 4/4 + (29.9 - 4.4) * .81 - 19.2 / co2,
         D13_dry = 4/4 + (29.9 - 4.4) * .55 - 19.2 / co2) |>
  mutate(d13_wet = (d13C - D13_wet) / (D13_wet / 1e3 + 1),
         d13_dry = (d13C - D13_dry) / (D13_dry / 1e3 + 1)) |>
  mutate(d13te_wet = ((1e3 + d13_wet) / ((-14.1 / 1e3) + 1)) - 1e3,
         d13te_dry = ((1e3 + d13_dry) / ((-14.1 / 1e3) + 1)) - 1e3)
write.csv(d13te, "data/regional_records/d13_org/processed/modeled_d13C.csv")

# Jiaxian (occluded organic matter)
jiaxian_age = read_xlsx("data/regional_records/d13_org/jiaxian/Jiaxian_summary_MS.xlsx")
jiaxian_d13o = read_xlsx("data/regional_records/d13_org/jiaxian/Jiaxian_summary_om.xlsx")
jiaxian_d13o$age = approx(x = jiaxian_age$D_cm, y = jiaxian_age$T_Ma, xout = jiaxian_d13o$depth_cm_om)$y
names(jiaxian_d13o) = c("depth_cm", "d13o", "n", "d13o.se", "age")
jiaxian_d13o = jiaxian_d13o |>
  filter(age < 8) |>
  mutate(d13o.low = d13o - d13o.se,
         d13o.high = d13o + d13o.se)
jiaxian_d13o$d13a = approx(d13Catm$age, d13Catm$d13C, xout = jiaxian_d13o$age)$y
jiaxian_d13o$co2 = approx(CO2atm$age, CO2atm$co2, xout = jiaxian_d13o$age)$y
jiaxian_d13o = jiaxian_d13o |>
  mutate(D13_wet = 4/4 + (29.9 - 4.4) * .81 - 19.2 / co2,
         D13_dry = 4/4 + (29.9 - 4.4) * .55 - 19.2 / co2) |>
  mutate(d13_wet = (d13a - D13_wet) / (D13_wet / 1e3 + 1),
         d13_dry = (d13a - D13_dry) / (D13_dry / 1e3 + 1))
write.csv(jiaxian_d13o, "data/regional_records/d13_org/processed/jiaxian.csv")

# Lingtai black carbon (Zhou et al., 2015)
lingtai = read_xlsx("data/regional_records/d13_org/Lingtai_zhou2014.xlsx")
lingtai_BC = lingtai[,c(8,11)]
names(lingtai_BC) = c("age", "d13")
lingtai_BC$d13a = approx(d13Catm$age, d13Catm$d13C, xout = lingtai_BC$age)$y
lingtai_BC$co2 = approx(CO2atm$age, CO2atm$co2, xout = lingtai_BC$age)$y
lingtai_BC = lingtai_BC |>
  mutate(D13_wet = 4/4 + (29.9 - 4.4) * .81 - 19.2 / co2,
         D13_dry = 4/4 + (29.9 - 4.4) * .55 - 19.2 / co2) |>
  mutate(d13_wet = (d13a - D13_wet) / (D13_wet / 1e3 + 1),
         d13_dry = (d13a - D13_dry) / (D13_dry / 1e3 + 1))
write_csv(lingtai_BC, "data/regional_records/d13_org/processed/lingtai.csv")
# ggplot(lingtai_SOM, aes(x = age, y = d13)) +
#   geom_line() +
#   geom_line(data = lingtai_BC, aes(x = age, y = d13))

# Northern China (Lu et al., 2020)
northern_china = read_xlsx("data/regional_records/d13_org/northern_china_lu2020.xlsx")
names(northern_china) = c("depth_m", "age", "d13o", "CaCO3", "TOC")
northern_china$d13a = approx(d13Catm$age, d13Catm$d13C, xout = northern_china$age)$y
northern_china$co2 = approx(CO2atm$age, CO2atm$co2, xout = northern_china$age)$y
northern_china = northern_china |>
  mutate(D13_wet = 4/4 + (29.9 - 4.4) * .81 - 19.2 / co2,
         D13_dry = 4/4 + (29.9 - 4.4) * .55 - 19.2 / co2) |>
  mutate(d13_wet = (d13a - D13_wet) / (D13_wet / 1e3 + 1),
         d13_dry = (d13a - D13_dry) / (D13_dry / 1e3 + 1))

# ggplot(northern_china, aes(x = age, y = d13o)) +
#   geom_point()
write_csv(northern_china, "data/regional_records/d13_org/processed/northern_china.csv")

# Japan sea black carbon (Shen et al., 2018)
japan_bc = read_xlsx("data/regional_records/d13_org/japan_sea_black_carbon_shen2018.xlsx")
japan_bc = japan_bc[3:356, 2:3]
names(japan_bc) = c("age", "d13o")
japan_bc = japan_bc |>
  mutate_all(as.numeric)
japan_bc$d13a = approx(d13Catm$age, d13Catm$d13C, xout = japan_bc$age)$y
japan_bc$co2 = approx(CO2atm$age, CO2atm$co2, xout = japan_bc$age)$y
japan_bc = japan_bc |>
  mutate(D13_wet = 4/4 + (29.9 - 4.4) * .81 - 19.2 / co2,
         D13_dry = 4/4 + (29.9 - 4.4) * .55 - 19.2 / co2) |>
  mutate(d13_wet = (d13a - D13_wet) / (D13_wet / 1e3 + 1),
         d13_dry = (d13a - D13_dry) / (D13_dry / 1e3 + 1))
# ggplot(japan_bc, aes(x = age, y = d13o)) +
#   geom_point()
write_csv(japan_bc, "data/regional_records/d13_org/processed/japan_sea.csv")

# fossil teeth (Passey et al., 2009)
clp_teeth = read_xls("data/regional_records/d13_org/CLP_fossil_teeth_passey2009.xls")
baode = data_frame(clp_teeth[6:52, 7:8], section = "Baode")
names(baode)[1:2] = c("age", "d13")
yushe = data_frame(clp_teeth[56:88, 7:8], section = "Yushe")
names(yushe)[1:2] = c("age", "d13")
lantian = data_frame(clp_teeth[92:146, 7:8], section = "Lantian")
names(lantian)[1:2] = c("age", "d13")
clp_teeth = rbind(baode, yushe, lantian) |>
  mutate(age = as.numeric(age),
         d13 = as.numeric(d13))
write_csv(clp_teeth, "data/regional_records/d13_org/processed/clp_teeth.csv")

# ggplot(clp_teeth, aes(x = age, y = d13, color = section)) +
#   geom_point()

# Siwalik 
vogeli_2017 = read_xlsx("data/regional_records/d13_org/Siwalik.xlsx", sheet = 2)[2:156, c(5,7)]
names(vogeli_2017) = c("age", "d13")
vogeli_2017 = vogeli_2017 |>
  filter(d13 != "N.D.") |>
  mutate(ref = "vogeli_2017")
ghosh_2004 = read_xlsx("data/regional_records/d13_org/Siwalik.xlsx", sheet = 3)[, c(5,8)] |> drop_na()
names(ghosh_2004) = c("age", "d13")
ghosh_2004 = ghosh_2004 |>
  mutate(ref = "ghosh_2004")
freeman_2001 = read_xlsx("data/regional_records/d13_org/Siwalik.xlsx", sheet = 6)[, c(5,8)]
names(freeman_2001) = c("age", "d13")
freeman_2001 = freeman_2001 |>
  mutate(ref = "freeman_2001")
roy_2020 = read_xlsx("data/regional_records/d13_org/Siwalik.xlsx", sheet = 7)[, c(5,8)]
names(roy_2020) = c("age", "d13")
roy_2020 = roy_2020 |>
  mutate(ref = "roy_2020")
siwalik = rbind(vogeli_2017, ghosh_2004, freeman_2001, roy_2020) |>
  mutate(age = as.numeric(age),
         d13 = as.numeric(d13))
siwalik$d13a = approx(d13Catm$age, d13Catm$d13C, xout = siwalik$age)$y
siwalik$co2 = approx(CO2atm$age, CO2atm$co2, xout = siwalik$age)$y
siwalik = siwalik |>
  mutate(D13_wet = 4/4 + (29.9 - 4.4) * .81 - 19.2 / co2,
         D13_dry = 4/4 + (29.9 - 4.4) * .55 - 19.2 / co2) |>
  mutate(d13_wet = (d13a - D13_wet) / (D13_wet / 1e3 + 1),
         d13_dry = (d13a - D13_dry) / (D13_dry / 1e3 + 1))

ggplot(siwalik, aes(x = age, y = d13_wet, color = ref)) +
  geom_point()
write_csv(siwalik, "data/regional_records/d13_org/processed/siwalik.csv")

dust_885 = read_xlsx("data/global_records/Dust_abell2021.xlsx", sheet = 1)[4:179, 3:5]
dust_885 = data.frame(lapply(dust_885, as.numeric))
names(dust_885)[1:3] = c("age", "flux", "flux_sd")
dust_885 = dust_885 |>
  mutate(low = flux - flux_sd,
         high = flux + flux_sd,
         age = age / 1e3)
write_csv(dust_885, "data/regional_records/d13_org/processed/dust_885.csv")
ggplot(dust_885, aes(x = age, y = flux)) +
  geom_point()
dust_1208 = read_xlsx("data/global_records/Dust_abell2021.xlsx", sheet = 2)[4:150, 3:5]
dust_1208 = data.frame(lapply(dust_1208, as.numeric))
names(dust_1208)[1:3] = c("age", "flux", "flux_sd")
dust_1208 = dust_1208 |>
  mutate(low = flux - flux_sd,
         high = flux + flux_sd,
         age = age / 1e3)
write_csv(dust_1208, "data/regional_records/d13_org/processed/dust_1208.csv")
ggplot(dust_1208, aes(x = age, y = flux)) +
  geom_point()
