library(tidyverse)
library(readxl)
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
pal2 = c("#A6CEE3", "#FB9A99", "#E31A1C")

## read data ----
sw = read.csv("output/d18c.csv")
t47 = read.csv("output/D47.csv")
dp17 = read.csv("output/dp17.csv")
smi = read_xlsx("data/regional_records/SMI.xlsx") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2 & age < 7.5) %>%
  drop_na(SMI)
iron = read_xlsx("data/regional_records/freeiron.xlsx", sheet = "Pianguan")
lr04 = read_xlsx("data/global_records/LR04.xlsx") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2.5 & age < 7.5)
co2 = read.csv("data/global_records/Pliocene_CO2.csv") %>%
  filter(xls != "phytoplankton_tanner_2021_p1.0.xlsx") %>%
  filter(xls != "phytoplankton_badger_2013a_p1.0.xlsx") %>%
  filter(proxy != "Paleosols") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2.5 & age < 7.5)
# Northern Hemisphere high latitude
site883 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "883 884") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site883$SST_change = site883$SST_50 - 5.420348
site907 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "907") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site907$SST_change = site907$SST_50 - 1.77141
site982 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "982") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site982$SST_change = site982$SST_50 - 10.754817
# Northern Hemisphere middle latitude
site1010 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "1010")
site1010$SST_change = site1010$SST_50 - 17.303341
site1021 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "1021")
site1021$SST_change = site1021$SST_50 - 14.320929
site1208 = read_xlsx("data/global_records/SST_calibrated.xlsx", sheet = "1208")
site1208$SST_change = site1208$SST_50 - 18.702248
# SST gradient
DSST = read.csv("data/global_records/DSST.csv") %>%
  mutate(age = age / 1000)
# obliquity modulation
ob = read_xlsx("data/global_records/Pliocene_orbital_and_insolation_data.xlsx")
ob.am = read.csv("output/ob.am.csv")
sig.am = read.csv("output/sig.am.csv")

## plot ----
png("figures/Fig4.global_records.png", 4, 8, units = "in", res = 600)
# pdf("figures/Fig4.global_records.pdf", width = 4.5, height = 7)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 6.5), axes = FALSE,
     xlab = "", ylab = "")

yext = range(lr04$d18O)
tix = seq(ceiling(max(yext * 10 + 3)), 
          floor(min(yext * 10)), by = -5) / 10
lr04.rs = cbind(lr04$age,
               6.5 - (lr04$d18O - min(tix)) / diff(range(tix)))
lines(lr04.rs[, 1], lr04.rs[, 2], col = pal[1])
axis(2, 6.5 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(delta^"18"*"O (\u2030)")), 2, line = 2.5, at = 6)

yext = range(smi$SMI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 0.2)
smi.rs = cbind(smi$age,
               4.8 + (smi$SMI - min(tix)) / diff(range(tix)))
lines(smi.rs[, 1], smi.rs[, 2], col = pal[2], pch = 21, cex = 1.2)
axis(4, 4.8 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("SMI"), 4, line = 2.5, at = 5.2)
yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
                 4.8 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

yext = range(iron$Fe)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10)), by = 1) /10
iron.rs = cbind(iron$age,
                4 + (iron$Fe - min(tix)) / diff(range(tix)))
lines(iron.rs[, 1], iron.rs[, 2], col = pal[3], pch = 21, cex = 1.2)
axis(2, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Fe"[2]*"O"[3]*"(f)/Fe"[2]*"O"[3]*"(t)"), 2, line = 2.5, at = 4.5)
yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
                 4 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

site = pal[factor(sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
yext = range(sw$d18sw.high, sw$d18sw.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
sw.rs = cbind(sw$age,
              3.3 + (sw$d18sw - min(tix)) / diff(range(tix)),
              3.3 + (sw$d18sw.low - min(tix)) / diff(range(tix)),
              3.3 + (sw$d18sw.high - min(tix)) / diff(range(tix)))
arrows(sw.rs[, 1], sw.rs[, 3], sw.rs[, 1], sw.rs[, 4], col = "grey",
       angle=90, length=0, code = 0)
points(sw.rs[, 1], sw.rs[, 2], col = "black", bg = site, pch = 21, cex = 1.2)
axis(4, 3.3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(delta^"18"*"O"[sw]*" (\u2030)")), 4, line = 2.5, at = 3.8)
yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
                 3.3 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

site = pal[factor(t47$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
yext = range(t47$st.low, t47$st.high)
tix = seq(floor(min(yext)-4), 
          ceiling(max(yext)), by = 5)
t47.rs = cbind(t47$age,
              2.3 + (t47$temp - min(tix)) / diff(range(tix)),
              2.3 + (t47$st.low - min(tix)) / diff(range(tix)),
              2.3 + (t47$st.high - min(tix)) / diff(range(tix)))
arrows(t47.rs[, 1], t47.rs[, 3], t47.rs[, 1], t47.rs[, 4], col = "grey",
       angle=90, length=0, code = 0)
points(t47.rs[, 1], t47.rs[, 2], col = "black", bg = site, pch = 22, cex = 1.2)
axis(2, 2.3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("T"[47]*" (", degree, "C)")), 2, line = 2.5, at = 2.8)

site = pal[factor(dp17$section, levels = c("Lantian", "Shilou", "Jiaxian"))]
yext = range(dp17$Dp17sw.low, dp17$Dp17sw.high)
tix = seq(ceiling(min(yext)+27), floor(max(yext+20)), 
          by = 50)
dp17.rs = cbind(dp17$age,
                2.5 - (dp17$Dp17sw - min(tix)) / diff(range(tix)),
                2.5 - (dp17$Dp17sw.low - min(tix)) / diff(range(tix)),
                2.5 - (dp17$Dp17sw.high - min(tix)) / diff(range(tix)))
arrows(dp17.rs[, 1], dp17.rs[, 3], dp17.rs[, 1], dp17.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(dp17.rs[, 1], dp17.rs[, 2], col = "black", bg = site, pch = 21, cex = 1.5)
axis(4, 2.5 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(Delta^"'17"*"O"[sw]*" (per meg)"), 4, line = 2.5, at = 2)

# yext = range(site982$SST_change, site907$SST_change, site883$SST_change)
# tix = seq(floor(min(yext) + 1),
#           ceiling(max(yext) + 2), by = 5)
# site982.rs = cbind(site982$age,
#                 2.5 + (site982$SST_change - min(tix)) / diff(range(tix)))
# site907.rs = cbind(site907$age,
#                    2.5 + (site907$SST_change - min(tix)) / diff(range(tix)))
# site883.rs = cbind(site883$age,
#                    2.5 + (site883$SST_change - min(tix)) / diff(range(tix)))
# lines(site883.rs[, 1], site883.rs[, 2], col = pal[1])
# lines(site982.rs[, 1], site982.rs[, 2], col = pal[2])
# lines(site907.rs[, 1], site907.rs[, 2], col = pal[3])
# axis(2, 2.5 + (tix - min(tix)) / diff(range(tix)), tix)
# mtext(expression(paste("SST change (", degree, "C)")), 2, line = 2.5, at = 3)

# yext = range(ob$gradient)
# tix = seq(ceiling(max(yext)),
#           floor(min(yext-2)), by = -10)
# sig.rs = cbind(ob$age,
#                3.3 - (ob$gradient - min(tix)) / diff(range(tix)))
# lines(sig.rs[, 1], sig.rs[, 2], col = "grey", lwd = 2)
# sig.am.rs = cbind(sig.am$age,
#                  3.3 - (sig.am$sig.am - min(tix)) / diff(range(tix)))
# lines(sig.am.rs[, 1], sig.am.rs[, 2], col = "tomato", lwd = 2)

yext = range(DSST$TH)
tix = seq(ceiling(max(yext)),
          floor(min(yext)), -2)
dsst.rs = cbind(DSST$age,
                1.8 - (DSST$TH - min(tix)) / diff(range(tix)))
lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[5], lwd = 3)
axis(2, 1.8 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(Delta*"SST (", degree, "C)")), 2, line = 2.5, at = 1.2)

# yext = range(DSST$MH)
# tix = seq(ceiling(max(yext)),
#           floor(min(yext)), -2)
# dsst.rs = cbind(DSST$age,
#                 1.8 - (DSST$MH - min(tix)) / diff(range(tix)))
# lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[1], lwd = 3)
# axis(4, 1.8 - (tix - min(tix)) / diff(range(tix)), tix)
# mtext(expression(paste(Delta*"SST (", degree, "C)")), 4, line = 2.5, at = 1.3)

pal2 = pal[1:2]
proxy = pal2[factor(co2$proxy)]
yext = range(co2$co2)
tix = seq(floor(min(yext) + 10), 
          ceiling(max(yext) + 75), by = 100)
co2.rs = cbind(co2$age,
              0 + (co2$co2 - min(tix)) / diff(range(tix)))
points(co2.rs[, 1], co2.rs[, 2], col = "black", bg = proxy, pch = 21, cex = 1)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(italic(p)*"CO"[2]*" (ppm)"), 4, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()





