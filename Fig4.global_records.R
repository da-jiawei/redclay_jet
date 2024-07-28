library(tidyverse)
library(readxl)
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
pal2 = c("#A6CEE3", "#FB9A99", "#E31A1C")

## read data ----
sw = read.csv("out/d18c.csv")
lr04 = read_xlsx("data/global records/LR04.xlsx") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2.5 & age < 7.5)
co2 = read.csv("data/global records/Pliocene_CO2.csv") %>%
  filter(xls != "phytoplankton_tanner_2021_p1.0.xlsx") %>%
  filter(xls != "phytoplankton_badger_2013a_p1.0.xlsx") %>%
  filter(proxy != "Paleosols") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2.5 & age < 7.5)
# Northern Hemisphere high latitude
# site883 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "883 884")
# site883$SST_change = site883$SST_50 - 5.420348
site907 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "907") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site907$SST_change = site907$SST_50 - 1.77141
site982 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "982") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site982$SST_change = site982$SST_50 - 10.754817
# Northern Hemisphere middle latitude
site1010 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "1010")
site1010$SST_change = site1010$SST_50 - 17.303341
site1021 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "1021")
site1021$SST_change = site1021$SST_50 - 14.320929
site1208 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "1208")
site1208$SST_change = site1208$SST_50 - 18.702248
# SST gradient
DSST = read.csv("data/global records/DSST.csv") %>%
  mutate(age = age / 1000)

## plot ----
png("figures/global_records.png", 4.5, 7, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2.5, 7.5), ylim = c(0, 4.5), axes = FALSE,
     xlab = "", ylab = "")

yext = range(lr04$d18O)
tix = seq(ceiling(max(yext * 10 + 3)), 
          floor(min(yext * 10)), by = -5) / 10
lr04.rs = cbind(lr04$age,
               4.5 - (lr04$d18O - min(tix)) / diff(range(tix)))
lines(lr04.rs[, 1], lr04.rs[, 2], col = pal, pch = 21, cex = 1.5)
axis(2, 4.5 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("benthic"*delta^"18"*"O (\u2030)")), 2, line = 2.5, at = 4)

site = pal[factor(sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
yext = range(sw$d18sw.high, sw$d18sw.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
sw.rs = cbind(sw$age,
              3 + (sw$d18sw - min(tix)) / diff(range(tix)),
              3 + (sw$d18sw.low - min(tix)) / diff(range(tix)),
              3 + (sw$d18sw.high - min(tix)) / diff(range(tix)))
arrows(sw.rs[, 1], sw.rs[, 3], sw.rs[, 1], sw.rs[, 4], col = "grey",
       angle=90, length=0, code = 0)
points(sw.rs[, 1], sw.rs[, 2], col = "black", bg = site, pch = 21, cex = 1.5)
axis(4, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(delta^"18"*"O"[sw]*" (\u2030)")), 4, line = 2.5, at = 3.5)

yext = range(site982$SST_change, site907$SST_change)
tix = seq(floor(min(yext) + 1), 
          ceiling(max(yext)), by = 5)
site982.rs = cbind(site982$age,
                1.7 + (site982$SST_change - min(tix)) / diff(range(tix)))
site907.rs = cbind(site907$age,
                   1.7 + (site907$SST_change - min(tix)) / diff(range(tix)))
lines(site982.rs[, 1], site982.rs[, 2], col = pal[2], cex = 1.5)
lines(site907.rs[, 1], site907.rs[, 2], col = pal[3], cex = 1.5)
axis(2, 1.7 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("SST change (", degree, "C)")), 2, line = 2.5, at = 2.2)

yext = range(DSST$TH)
tix = seq(ceiling(max(yext)),
          floor(min(yext)), -2)
dsst.rs = cbind(DSST$age,
                2 - (DSST$TH - min(tix)) / diff(range(tix)))
lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[5], lwd = 3)
axis(4, 2 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("SST gradient (", degree, "C)")), 4, line = 2.5, at = 1.5)

pal2 = pal[5:6]
proxy = pal2[factor(co2$proxy)]
yext = range(co2$co2)
tix = seq(floor(min(yext) + 10), 
          ceiling(max(yext) + 75), by = 100)
co2.rs = cbind(co2$age,
              0 + (co2$co2 - min(tix)) / diff(range(tix)))
points(co2.rs[, 1], co2.rs[, 2], col = "black", bg = proxy, pch = 21, cex = 1)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(italic(p)*"CO"[2]*" (ppm)"), 2, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()





