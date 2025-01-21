library(tidyverse)
library(readxl)
library(ggpubr)

## load data
# insolation
ob.am = read.csv("out/ob.am.csv")
# local records
sw = read.csv("out/d18c.csv")
D47 = read.csv("out/D47.csv")
D47= D47[order(D47$age),] 
smi = read_xlsx("data/regional records/SMI.xlsx") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2 & age < 7.5) %>%
  drop_na(SMI)
iron = read_xlsx("data/regional records/freeiron.xlsx", sheet = "Pianguan")
# global records
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
site883 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "883 884") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site883$SST_change = site883$SST_50 - 5.420348
site907 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "907") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site907$SST_change = site907$SST_50 - 1.77141
site982 = read_xlsx("data/global records/SST_calibrated.xlsx", sheet = "982") %>%
  mutate(age = ageBP / 1000) %>%
  filter(age > 2.5 & age < 7.5)
site982$SST_change = site982$SST_50 - 10.754817
# SST gradient
DSST = read.csv("data/global records/DSST.csv") %>%
  mutate(age = age / 1000)

## plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
site1 = pal[factor(D47$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
site2 = pal[factor(sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]

# png("figures/Fig3.regional_records.png", 4, 6, units = "in", res = 300)
pdf("figures/Fig4.records_comparison.pdf", width = 5, height = 8)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 9), axes = FALSE,
     xlab = "", ylab = "")

yext = range(lr04$d18O)
tix = seq(ceiling(max(yext * 10 + 3)), 
          floor(min(yext * 10)), by = -5) / 10
lr04.rs = cbind(lr04$age,
                9 - (lr04$d18O - min(tix)) / diff(range(tix)))
lines(lr04.rs[, 1], lr04.rs[, 2], col = pal[1])
axis(2, 9 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("benthic"*delta^"18"*"O (\u2030)")), 2, line = 2.5, at = 8.5)

yext = range(D47$st.low, D47$st.high)
tix = seq(floor(min(yext) + 1), 
          ceiling(max(yext)), by = 10)
D47.rs = cbind(D47$age,
               7 + (D47$temp - min(tix)) / diff(range(tix)),
               7 + (D47$st.low - min(tix)) / diff(range(tix)),
               7 + (D47$st.high - min(tix)) / diff(range(tix)))
arrows(D47.rs[, 1], D47.rs[, 3], D47.rs[, 1], D47.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(D47.rs[, 1], D47.rs[, 2], col = "black", bg = site1, pch = 21, cex = 1.5)
axis(4, 7 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("T"[Delta*"47"]*" (", degree, "C)")), 4, line = 2.5, at = 7.5)

yext = range(sw$d18sw.low, sw$d18sw.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
sw.rs = cbind(sw$age,
                6 + (sw$d18sw - min(tix)) / diff(range(tix)),
                6 + (sw$d18sw.low - min(tix)) / diff(range(tix)),
                6 + (sw$d18sw.high - min(tix)) / diff(range(tix)))
arrows(sw.rs[, 1], sw.rs[, 3], sw.rs[, 1], sw.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(sw.rs[, 1], sw.rs[, 2], col = "black", bg = site, pch = 21, cex = 1.2)
axis(2, 6 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 2, line = 2.5, at = 6.5)

yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
               6.4 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

yext = range(smi$SMI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 0.2)
smi.rs = cbind(smi$age,
              5 + (smi$SMI - min(tix)) / diff(range(tix)))
lines(smi.rs[, 1], smi.rs[, 2], col = pal[1], pch = 21, cex = 1.2)
axis(4, 5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("SMI"), 4, line = 2.5, at = 5.5)

yext = range(iron$Fe)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10)), by = 1) /10
iron.rs = cbind(iron$age,
               4 + (iron$Fe - min(tix)) / diff(range(tix)))
lines(iron.rs[, 1], iron.rs[, 2], col = pal[3], pch = 21, cex = 1.2)
axis(2, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Fe"[2]*"O"[3]*"(f)/Fe"[2]*"O"[3]*"(t)"), 2, line = 2.5, at = 4.5)

yext = range(site982$SST_change, site907$SST_change, site883$SST_change)
tix = seq(floor(min(yext) + 1),
          ceiling(max(yext) + 2), by = 5)
site982.rs = cbind(site982$age,
                   3 + (site982$SST_change - min(tix)) / diff(range(tix)))
site907.rs = cbind(site907$age,
                   3 + (site907$SST_change - min(tix)) / diff(range(tix)))
site883.rs = cbind(site883$age,
                   3 + (site883$SST_change - min(tix)) / diff(range(tix)))
lines(site883.rs[, 1], site883.rs[, 2], col = pal[1])
lines(site982.rs[, 1], site982.rs[, 2], col = pal[2])
lines(site907.rs[, 1], site907.rs[, 2], col = pal[3])
axis(4, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("SST change (", degree, "C)")), 4, line = 2.5, at = 3.5)

yext = range(DSST$TH)
tix = seq(ceiling(max(yext)),
          floor(min(yext)), -2)
dsst.rs = cbind(DSST$age,
                3 - (DSST$TH - min(tix)) / diff(range(tix)))
lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[5], lwd = 3)
axis(2, 3 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(Delta*"SST (", degree, "C)")), 2, line = 2.5, at = 2.5)

yext = range(DSST$MH)
tix = seq(ceiling(max(yext)),
          floor(min(yext)), -2)
dsst.rs = cbind(DSST$age,
                2 - (DSST$MH - min(tix)) / diff(range(tix)))
lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[1], lwd = 3)
axis(4, 2 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(Delta*"SST (", degree, "C)")), 4, line = 2.5, at = 1.5)

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
