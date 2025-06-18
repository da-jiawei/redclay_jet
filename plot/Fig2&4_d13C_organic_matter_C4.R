rm(list = ls())
pacman::p_load(tidyverse, readxl)
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99")

#### load and groom data ----
d18sw = read_csv("output/d18c.csv")
D47 = read_csv("output/D47.csv") |>
  select(site, age, temp, temp.sd) |>
  mutate(low = temp - temp.sd,
         high = temp + temp.sd)
benthic = read_xlsx("data/global_records/LR04.xlsx") |> mutate(age = age / 1e3) |> filter(age <= 8)
jiaxian = read_csv("data/regional_records/d13_org/processed/jiaxian.csv")
lingtai = read_csv("data/regional_records/d13_org/processed/lingtai.csv") |> drop_na()
teeth = read_csv("data/regional_records/d13_org/processed/clp_teeth.csv")
teeth_d13max = read_csv("data/regional_records/d13_org/processed/modeled_d13C.csv")
northern_china = read_csv("data/regional_records/d13_org/processed/northern_china.csv")
japan = read_csv("data/regional_records/d13_org/processed/japan_sea.csv") |> filter(age <= 8)
dust_885 = read_csv("data/regional_records/d13_org/processed/dust_885.csv")
dust_1208 = read_csv("data/regional_records/d13_org/processed/dust_1208.csv")
ob.am = read.csv("output/ob.am.csv")
co2 = read.csv("data/global_records/Pliocene_CO2.csv")[, c(3:4, 7:9)] |>
  filter(proxy != "Phytoplankton") |>
  mutate(age = age / 1000,
         low = co2 - co2_uncertainty_lower,
         high = co2 + co2_uncertainty_higher)
DSST = read.csv("data/global_records/DSST.csv") %>%
  mutate(age = age / 1000)
#### 8 Ma Plot ----
png("figures/Fig2.d13_organic_matter_time_series.png", 10, 4.6, units = "in", res = 500)
par(mar = c(4, 2, 4, 2))
plot(-1, 0, xlim = c(0, 7), ylim = c(8, 0), axes = FALSE,
     xlab = "", ylab = "")
axis(2, at = seq(0, 8, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 2, line = 1)
# CO2
site = pal[factor(co2$proxy, levels = c("Boron Proxies", "Paleosols"))]
co2 = co2 |> arrange(age)
xext = range(co2$co2)
tix = seq(floor(min(xext)/100), ceiling(max(xext)/100), by = 1) * 100
co2.rs = cbind(1 - (co2$co2 - min(tix)) / diff(range(tix)),
                   co2$age)
points(co2.rs[, 1], co2.rs[, 2], col = site, bg = "white", pch = 21, cex = .8)
axis(1, 1 - (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression("CO"[2]*" (ppmv)"), 1, line = 2, at = 0.5)
legend(x = 0, y = 7.2, legend = c("Boron Proxies", "Paleosols"),
       col = pal, pch = 16, cex = 0.8, pt.cex = 1.5,
       bty = "n")

# lingtai
xext = range(lingtai$d13, lingtai$d13_wet, lingtai$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 5)
lingtai.rs = cbind(1 + (lingtai$d13 - min(tix)) / diff(range(tix)),
                   1 + (lingtai$d13_wet - min(tix)) / diff(range(tix)),
                   1 + (lingtai$d13_dry - min(tix)) / diff(range(tix)),
                   lingtai$age)
# lines(lingtai.rs[, 2], lingtai.rs[, 4], col = "grey70", lwd = 1)
lines(lingtai.rs[, 3], lingtai.rs[, 4], col = "grey70", lwd = 1)
lines(lingtai.rs[, 1], lingtai.rs[, 4], col = pal[2])
axis(3, 1 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))

mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 3, line = 2, at = 1.5)
text(1.5, 7.5, "Lingtai")

# jiaxian
xext = range(jiaxian$d13o.low, jiaxian$d13o.high, jiaxian$d13_wet, jiaxian$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
jiaxian_d13o.rs = cbind(2 + (jiaxian$d13o - min(tix)) / diff(range(tix)),
                        2 + (jiaxian$d13o.low - min(tix)) / diff(range(tix)),
                        2 + (jiaxian$d13o.high - min(tix)) / diff(range(tix)),
                        2 + (jiaxian$d13_wet - min(tix)) / diff(range(tix)),
                        2 + (jiaxian$d13_dry - min(tix)) / diff(range(tix)),
                        jiaxian$age)
arrows(jiaxian_d13o.rs[, 2], jiaxian_d13o.rs[, 6], 
       jiaxian_d13o.rs[, 3], jiaxian_d13o.rs[, 6], 
       col = "grey80", angle=90, length=0, code = 0)
# lines(jiaxian_d13o.rs[, 4], jiaxian_d13o.rs[, 6], col = "grey70", lwd = 1)
lines(jiaxian_d13o.rs[, 5], jiaxian_d13o.rs[, 6], col = "grey70", lwd = 1)
points(jiaxian_d13o.rs[, 1], jiaxian_d13o.rs[, 6], col = pal[1], bg = "white", pch = 21, cex = .8)
axis(1, 2 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[OOM]*" (\u2030)"), 1, line = 2, at = 2.5)
text(2.5, 1.5, "Jiaxian")

# Northern China
xext = range(northern_china$d13o, northern_china$d13_wet, northern_china$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
northern_china.rs = cbind(3 + (northern_china$d13o - min(tix)) / diff(range(tix)),
                          3 + (northern_china$d13_wet - min(tix)) / diff(range(tix)),
                          3 + (northern_china$d13_dry - min(tix)) / diff(range(tix)),
                          northern_china$age)
# lines(northern_china.rs[, 2], northern_china.rs[, 4], col = "grey70", lwd = 1)
lines(northern_china.rs[, 3], northern_china.rs[, 4], col = "grey70", lwd = 1)
lines(northern_china.rs[, 1], northern_china.rs[, 4], col = pal[3])
points(northern_china.rs[, 1], northern_china.rs[, 4], col = pal[3], bg = "white", pch = 21, cex = .8)
axis(3, 3 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 3, line = 2, at = 3.5)
text(3.8, 5, "G3")

# tooth enamel
sites = pal[factor(teeth$section, levels = c("Lantian", "Baode", "Yushe"))] 
xext = range(teeth$d13, teeth_d13max$d13te_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
teeth.rs = cbind(4 + (teeth$d13 - min(tix)) / diff(range(tix)),
                 teeth$age)
teeth_max.rs = cbind(4 + (teeth_d13max$d13te_dry - min(tix)) / diff(range(tix)),
                     teeth_d13max$age)
lines(teeth_max.rs[, 1], teeth_max.rs[, 2], col = "grey70", lwd = 1)
points(teeth.rs[, 1], teeth.rs[, 2], col = sites, bg = "white", pch = 22, cex = .8)
axis(1, 4 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[TE]*" (\u2030)"), 1, line = 2, at = 4.5)
legend(x = 4, y = -.2, legend = c("Lantian", "Baode", "Yushe"),
       col = pal, pch = 22, cex = 0.8, pt.cex = 1, bg = "white",
       x.intersp = .8, y.intersp = .8, text.width = 0.4)

# Japan sea 
xext = range(japan$d13o, japan$d13_wet, japan$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
japan.rs = cbind(5 + (japan$d13o - min(tix)) / diff(range(tix)),
                 5 + (japan$d13_wet - min(tix)) / diff(range(tix)),
                 5 + (japan$d13_dry - min(tix)) / diff(range(tix)),
                 japan$age)
# lines(japan.rs[, 2], japan.rs[, 4], col = "grey70")
lines(japan.rs[, 3], japan.rs[, 4], col = "grey70")
lines(japan.rs[, 1], japan.rs[, 4], col = pal[2])
points(japan.rs[, 1], japan.rs[, 4], col = pal[2], bg = "white", pch = 22, cex = .8)
axis(3, 5 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 3, line = 2, at = 5.5)
mtext("U1430", 1, line = -.5, at = 5.5, cex = .8)

# benthic d18O
xext = range(benthic$d18O)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 1)
benthic.rs = cbind(6 + (benthic$d18O - min(tix)) / diff(range(tix)),
                   benthic$age)
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1])
axis(1, 6 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"18"*"O"[benthic]*" (\u2030)"), 1, line = 2, at = 6.5)

rect(xleft = 0, ybottom = 3, xright = 7, ytop = .75, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
# rect(xleft = 1.2, ybottom = 3.5, xright = 8, ytop = 3, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
# rect(xleft = 1.2, ybottom = 2.8, xright = 8, ytop = 2.5, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
text(.1, .2, "a", cex = 1, col = "black", font = 2)
text(1, .2, "b", cex = 1, col = "black", font = 2)
text(2.5, .2, "c", cex = 1, col = "black", font = 2)
text(3.8, .2, "d", cex = 1, col = "black", font = 2)
text(4.8, 7.8, "e", cex = 1, col = "black", font = 2)
text(5.2, .2, "f", cex = 1, col = "black", font = 2)
text(6.8, 7.8, "g", cex = 1, col = "black", font = 2)

axis(4, at = seq(0, 8, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 4, line = 1)
dev.off()

# late Miocene - Pliocene plot ----
png("figures/Fig4.d13_organic_matter_time_series2.png", 10, 4.7, units = "in", res = 500)
par(mar = c(4, 2, 4, 2))
plot(-1, 0, xlim = c(0, 8.5), ylim = c(7, 2), axes = FALSE,
     xlab = "", ylab = "")
axis(2, at = seq(2, 7, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 2, line = 1)
rect(xleft = 0, ybottom = 5.6, xright = 6.7, ytop = 5, border = NA, col = rgb(1, 0, 0, 0.1), lwd = 0, alpha = .5)
rect(xleft = 0, ybottom = 5, xright = 6.7, ytop = 3.7, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
rect(xleft = 0, ybottom = 3.4, xright = 8.5, ytop = 2.4, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)

# DSST
xext = range(DSST$TH)
tix = seq(ceiling(max(xext)),
          floor(min(xext)), -2)
dsst.rs = cbind(0 + (DSST$TH - min(tix)) / diff(range(tix)),
                DSST$age)
lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[1], lwd = 3)
axis(3, 0 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0), 
     col = pal[1], col.axis = pal[1])
mtext(expression(paste(Delta*"SST (", degree, "C)")), 3, line = 2, at = .5)

xext = range(DSST$MH)
tix = seq(ceiling(max(xext)),
          floor(min(xext)), -2)
dsst.rs = cbind(0.4 + (DSST$MH - min(tix)) / diff(range(tix)),
                DSST$age)
lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[2], lwd = 3)
axis(3, 0.4 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(-2, -2.3, -3), 
     col = pal[2], col.axis = pal[2])

# Soil water d18O
site = pal[factor(d18sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
xext = range(d18sw$d18sw)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
soil_water.rs = cbind(2 - (d18sw$d18sw - min(tix)) / diff(range(tix)),
                      d18sw$age)
points(soil_water.rs[, 1], soil_water.rs[, 2], col = "black", bg = site, pch = 21, cex = .8)
axis(1, 2 - (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 1, line = 2, at = 1.5)

# obliquity cycle
xext = range(ob.am$ob.am)
tix = seq(floor(min(xext*10)), ceiling(max(xext*10+3)), by = 5)/10
ob.am.rs = cbind(2 - (ob.am$ob.am - min(tix)) / diff(range(tix)),
                 ob.am$age)
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "grey", lwd = 2)

# Lingtai 
lingtai2 = lingtai |> filter(age <= 7 & age >= 2)
xext = range(lingtai2$d13)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 5)
lingtai2.rs = cbind(2 + (lingtai2$d13 - min(tix)) / diff(range(tix)),
                   lingtai2$age)
lines(lingtai2.rs[, 1], lingtai2.rs[, 2], col = pal[2])
axis(3, 2 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 3, line = 2, at = 2.5)
text(2.2, 2, "Lingtai", cex = .8)

# Jiaxian
jiaxian2 = jiaxian |> filter(age <= 7)
xext = range(jiaxian2$d13o.low, jiaxian2$d13o.high)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
jiaxian_d13o.rs = cbind(2.8 + (jiaxian2$d13o - min(tix)) / diff(range(tix)),
                        2.8 + (jiaxian2$d13o.low - min(tix)) / diff(range(tix)),
                        2.8 + (jiaxian2$d13o.high - min(tix)) / diff(range(tix)),
                        jiaxian2$age)
arrows(jiaxian_d13o.rs[, 2], jiaxian_d13o.rs[, 4], 
       jiaxian_d13o.rs[, 3], jiaxian_d13o.rs[, 4], 
       col = "grey80", angle=90, length=0, code = 0)
points(jiaxian_d13o.rs[, 1], jiaxian_d13o.rs[, 4], col = pal[1], bg = "white", pch = 21, cex = .8)
axis(1, 2.8 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[OOM]*" (\u2030)"), 1, line = 2, at = 3.3)
text(3.3, 2, "Jiaxian", cex = .8)

# northern China
northern_china2 = northern_china |> filter(age <= 7 & age >= 2)
xext = range(northern_china2$d13o)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
northern_china.rs = cbind(3.9 + (northern_china2$d13o - min(tix)) / diff(range(tix)),
                          northern_china2$age)
lines(northern_china.rs[, 1], northern_china.rs[, 2], col = pal[3])
points(northern_china.rs[, 1], northern_china.rs[, 2], col = pal[3], bg = "white", pch = 21, cex = .8)
axis(3, 3.9 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 3, line = 2, at = 4.4)
text(4.3, 7, "G3", cex = .8)

# Japan Sea
japan2 = japan |> filter(age <= 7 & age >= 2)
xext = range(japan2$d13o)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 1)
japan.rs = cbind(4.7 + (japan2$d13o - min(tix)) / diff(range(tix)),
                 japan2$age)
lines(japan.rs[, 1], japan.rs[, 2], col = pal[2])
points(japan.rs[, 1], japan.rs[, 2], col = pal[2], bg = "white", pch = 22, cex = .8)
axis(1, 4.7 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 1, line = 2, at = 5.2)
text(5.4, 7, "U1430", cex = .8)

# benthic
benthic2 = benthic |> filter(age <= 7 & age >= 2)
xext = range(benthic2$d18O)
tix = seq(floor(min(xext)*10), ceiling((max(xext) + .3)*10), by = 5)/10
benthic.rs = cbind(5.7 + 1.2 * (benthic2$d18O - min(tix)) / diff(range(tix)),
                   benthic2$age)
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1])
axis(3, 5.7 + 1.2 * (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"18"*"O"[benthic]*" (\u2030)"), 3, line = 2, at = 6.2)

# dust
xext = range(dust_1208$flux)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 1)
dust1208.rs = cbind(6.8 + (dust_1208$flux - min(tix)) / diff(range(tix)),
                6.8 + (dust_1208$low - min(tix)) / diff(range(tix)),
                6.8 + (dust_1208$high - min(tix)) / diff(range(tix)),
                dust_1208$age)
# arrows(dust1208.rs[, 2], dust1208.rs[, 4], 
#        dust1208.rs[, 3], dust1208.rs[, 4], 
#        col = "grey80", angle=90, length=0, code = 0)
# points(dust1208.rs[, 1], dust1208.rs[, 4], col = pal[2], bg = "white", pch = 21, cex = .8)
lines(dust1208.rs[, 1], dust1208.rs[, 4], col = pal[2])
axis(1, 6.8 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(-5, -5.3, -6))
mtext(expression("flux (g/cm"^"2"*"/kyr)"), 1, line = -4, at = 7.3)
text(7, 4.8, "ODP 1208", col = pal[2], cex = .8)

xext = range(dust_885$flux)
tix = seq(floor(min(xext)*10), ceiling(max(xext)*10), by = 1) / 10
dust885.rs = cbind(7.4 + (dust_885$flux - min(tix)) / diff(range(tix)),
                   7.4 + (dust_885$low - min(tix)) / diff(range(tix)),
                   7.4 + (dust_885$high - min(tix)) / diff(range(tix)),
                   dust_885$age)
# arrows(dust885.rs[, 2], dust885.rs[, 4], 
#        dust885.rs[, 3], dust885.rs[, 4], 
#        col = "grey80", angle=90, length=0, code = 0)
# points(dust885.rs[, 1], dust885.rs[, 4], col = pal[3], bg = "white", pch = 21, cex = .8)
lines(dust885.rs[, 1], dust885.rs[, 4], col = pal[3])
axis(3, 7.4 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression("flux (g/cm"^"2"*"/kyr)"), 3, line = 2, at = 7.9)
text(7.9, 2.2, "ODP 885/886", col = pal[4], cex = .8)

axis(4, at = seq(2, 7, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 4, line = 1)

text(.5, 7, "a", cex = 1, col = "black", font = 2)
text(1.1, 7, "b", cex = 1, col = "black", font = 2)
text(2.7, 7, "c", cex = 1, col = "black", font = 2)
text(3.6, 7, "d", cex = 1, col = "black", font = 2)
text(4, 7, "e", cex = 1, col = "black", font = 2)
text(4.8, 7, "f", cex = 1, col = "black", font = 2)
text(6.2, 7, "g", cex = 1, col = "black", font = 2)
text(7.5, 4.8, "h", cex = 1, col = "black", font = 2)
text(8.3, 4.2, "i", cex = 1, col = "black", font = 2)
legend(x = 6.8, y = 6.3, legend = c("Lantian", "Shilou", "Jiaxian"),
       pch = 21, col = c("black","black","black"), pt.bg = pal, cex = 0.8, pt.cex = 1,
       y.intersp = .9, text.width = 0.4, bty = "n")

dev.off()

