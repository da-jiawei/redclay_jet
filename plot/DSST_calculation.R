rm(list = ls())
pacman::p_load(tidyverse, readxl)

# Prepare data ----
# Northern Hemisphere high latitude
site883 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "883 884") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site907 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "907") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site982 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "982") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
HL = list(site883 = site883, site907 = site907, site982 = site982)

# Northern Hemisphere middle latitude
site1010 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "1010") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site1021 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "1021") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site1208 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "1208") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
ML = list(site1010 = site1010, site1021 = site1021, site1208 = site1208)
# Tropics
site722 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "722") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site846 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "846") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site850 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "850") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site1338 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "U1338") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
site1241 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "1241") |>
  mutate(age = ageBP / 1000) |> filter(age < 8)
TP = list(site722 = site722, site846 = site846, site850 = site850, site1338 = site1338, site1241 = site1241)

# interpolation and bin ----
mean_SST = function(bin, data_list){
  times = seq(0, 8, by = bin)
  data = data.frame(age = times[-1])
  for (i in 1:length(data_list)) {
    site_name = names(data_list[i])
    for (j in 1:(length(times)-1)) {
      age_min = times[j]
      age_max = times[j+1]
      sample = data_list[[i]] |>
        filter(age >= age_min & age <= age_max)
      data[[site_name]][j] = mean(sample$SST_50)
    }
    data[[site_name]] = approx(data$age, data[[site_name]], data$age)$y
  }
  data$mean = rowMeans(data[, 2:ncol(data)], na.rm = TRUE)
  return(data)
}
HL_mean = mean_SST(0.1, HL)
ML_mean = mean_SST(0.1, ML)
TP_mean = mean_SST(0.1, TP)

DSST = data.frame(age = HL_mean$age,
                  DTP_HL = TP_mean$mean - HL_mean$mean,
                  DTP_ML = TP_mean$mean - ML_mean$mean)
DSST$DTP_HL[1:28] = NA

plot(DSST$age, DSST$DTP_HL, type = "l", col = "tomato",
     ylim = c(22,5))
lines(DSST$age, DSST$DTP_ML, col = "royalblue")
write_csv(DSST, "output/DSST.csv")
