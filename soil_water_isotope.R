library(tidyverse)
library(readxl)
nsyth = 10000

## read data ----
d18.lantian = read_xlsx("data/redclay_isotope.xlsx", sheet = "Lantian") %>%
  mutate(site = "Lantian")
d18.shilou = read_xlsx("data/redclay_isotope.xlsx", sheet = "Shilou") %>%
  mutate(site = "Shilou")
d18.jiaxian = read_xlsx("data/redclay_isotope.xlsx", sheet = "Jiaxian") %>%
  mutate(site = "Jiaxian")
d18c = rbind(d18.lantian, d18.shilou, d18.jiaxian)
D47 = read_xlsx("data/redclay_D47.xlsx") %>%
  mutate(st.low = temp - temp.sd,
         st.high = temp + temp.sd)
dp17 = read_xlsx("data/Dp17.xlsx") %>%
  mutate(Dp17sw.se = replace_na(Dp17sw.se, 0),
         dp18sw.se = replace_na(dp18sw.se, 0),
         Dp17sw.low = Dp17sw - Dp17sw.se,
         Dp17sw.high = Dp17sw + Dp17sw.se,
         dp18sw.low = dp18sw - dp18sw.se,
         dp18sw.high = dp18sw + dp18sw.se)
write.csv(dp17, "out/dp17.csv")

## paired D47-d18O data ----
for (i in 1:nrow(D47)) {
  d18c = rnorm(nsyth, D47$d18[i], D47$d18.se[i])
  st = rnorm(nsyth, D47$temp[i], D47$temp.sd[i])
  d18c_vsmow = (1.03091 * d18c) + 30.91
  alpha = exp((16.1*1000/(273.15 + st)-24.6)/1000) # Tremaine 2011
  d18sw = (1000 + d18c_vsmow) / alpha - 1000
  D47$d18sw[i] = mean(d18sw)
  D47$d18sw.se[i] = sd(d18sw)
}
D47 = D47 %>%
  mutate(d18sw.low = d18sw - d18sw.se,
         d18sw.high = d18sw + d18sw.se)

write.csv(D47, file = "out/D47.csv")

## interpolated D47 data ----
lm = loess(data = D47, temp ~ age, span = 0.3)
prediction = predict(lm, newdata = d18c$age, se = TRUE, level = 0.95)
d18c = d18c %>%
  mutate(st = prediction$fit,
         st.sd = prediction$se) %>%
  drop_na(st, st.sd)

for (i in 1:nrow(d18c)) {
  d18c1 = rnorm(nsyth, d18c$d18O[i], 0.25)
  st = rnorm(nsyth, d18c$st[i], d18c$st.sd[i])
  d18c_vsmow = (1.03091 * d18c1) + 30.91
  alpha = exp((16.1*1000/(273.15 + st)-24.6)/1000) # Tremaine 2011
  d18sw = (1000 + d18c_vsmow) / alpha - 1000
  d18c$d18sw[i] = mean(d18sw)
  d18c$d18sw.se[i] = sd(d18sw)
}
d18c = d18c %>%
  mutate(d18sw.low = d18sw - d18sw.se,
         d18sw.high = d18sw + d18sw.se)

write.csv(d18c, file = "out/d18c.csv")