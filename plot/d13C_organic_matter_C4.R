rm(list = ls())
pacman::p_load(tidyverse, readxl)

#### load and groom data ----
this_study = read_csv("output/d18c.csv")
benthic = read_xlsx("data/global_records/LR04.xlsx") |> mutate(age = age / 1e3) |> filter(age <= 8)
jiaxian = read_csv("data/regional_records/d13_org/processed/jiaxian.csv")
lingtai = read_csv("data/regional_records/d13_org/processed/lingtai.csv") |> drop_na()
teeth = read_csv("data/regional_records/d13_org/processed/clp_teeth.csv")
teeth_d13max = read_xls("data/regional_records/d13_org/modeled_d13C_passey2009.xls", sheet = 2)
northern_china = read_csv("data/regional_records/d13_org/processed/northern_china.csv")
siwalik = read_csv("data/regional_records/d13_org/processed/siwalik.csv") |> filter(age <= 8) |> drop_na()
japan = read_csv("data/regional_records/d13_org/processed/japan_sea.csv") |> filter(age <= 8)
dust_885 = read_csv("data/regional_records/d13_org/processed/dust_885.csv")
dust_1208 = read_csv("data/regional_records/d13_org/processed/dust_1208.csv")

#### 8 Ma Plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99")
png("figures/d13_organic_matter_time_series.png", 10, 4.6, units = "in", res = 500)
par(mar = c(4, 2, 4, 2))
plot(-1, 0, xlim = c(0, 7), ylim = c(8, 0), axes = FALSE,
     xlab = "", ylab = "")
axis(2, at = seq(0, 8, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 2, line = 1)
# siwalik
site = pal[factor(siwalik$ref, levels = c("vogeli_2017", "ghosh_2004", "freeman_2001", "roy_2020"))]
siwalik = siwalik |> arrange(age)
xext = range(siwalik$d13, siwalik$d13_wet, siwalik$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 5)
siwalik.rs = cbind(0 + (siwalik$d13 - min(tix)) / diff(range(tix)),
                   0 + (siwalik$d13_wet - min(tix)) / diff(range(tix)),
                   0 + (siwalik$d13_dry - min(tix)) / diff(range(tix)),
                   siwalik$age)
# lines(siwalik.rs[, 2], siwalik.rs[, 4], col = "grey70", lwd = 2)
lines(siwalik.rs[, 3], siwalik.rs[, 4], col = "grey70", lwd = 2)
points(siwalik.rs[, 1], siwalik.rs[, 4], col = site, bg = "white", pch = 21, cex = .8)
axis(1, 0 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 1, line = 2, at = 0.5)
mtext("Siwalik", 3, line = 0, at = .2)
# legend(x = 6, y = 3.5, legend = c("Lantian", "Shilou", "Jiaxian"),
#        col = pal, pch = 16, cex = 0.8, pt.cex = 1.5)

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
xext = range(teeth$d13, teeth_d13max$d13e_max)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
teeth.rs = cbind(4 + (teeth$d13 - min(tix)) / diff(range(tix)),
                 teeth$age)
teeth_max.rs = cbind(4 + (teeth_d13max$d13e_max - min(tix)) / diff(range(tix)),
                     teeth_d13max$age)
lines(teeth_max.rs[, 1], teeth_max.rs[, 2], col = "grey70", lwd = 2)
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
text(.8, 7.8, "a", cex = 1, col = "black", font = 2)
text(1, .2, "b", cex = 1, col = "black", font = 2)
text(2, 7.8, "c", cex = 1, col = "black", font = 2)
text(3.8, .2, "d", cex = 1, col = "black", font = 2)
text(4.8, 7.8, "e", cex = 1, col = "black", font = 2)
text(5.2, .2, "f", cex = 1, col = "black", font = 2)
text(6.8, 7.8, "g", cex = 1, col = "black", font = 2)

axis(4, at = seq(0, 8, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 4, line = 1)
dev.off()

# late Miocene - Pliocene plot ----
png("figures/d13_organic_matter_time_series2.png", 9, 4.4, units = "in", res = 500)
par(mar = c(4, 2, 4, 2))
plot(-1, 0, xlim = c(0, 7.5), ylim = c(7, 2), axes = FALSE,
     xlab = "", ylab = "")
axis(2, at = seq(2, 7, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 2, line = 1)

# Soil water d18O
site = pal[factor(this_study$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
xext = range(this_study$d18sw)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
soil_water.rs = cbind(1 - (this_study$d18sw - min(tix)) / diff(range(tix)),
                      this_study$age)
points(soil_water.rs[, 1], soil_water.rs[, 2], col = site, bg = "white", pch = 21, cex = .8)
axis(1, 1 - (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 1, line = 2, at = 0.5)

# Lingtai 
lingtai2 = lingtai |> filter(age <= 7 & age >= 2)
xext = range(lingtai2$d13)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 5)
lingtai2.rs = cbind(1 + (lingtai2$d13 - min(tix)) / diff(range(tix)),
                   lingtai2$age)
lines(lingtai2.rs[, 1], lingtai2.rs[, 2], col = pal[2])
axis(3, 1 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 3, line = 2, at = 1.5)
mtext("Lingtai", 1, line = -.5, at = 1.5)

# Jiaxian
jiaxian2 = jiaxian |> filter(age <= 7)
xext = range(jiaxian2$d13o.low, jiaxian2$d13o.high)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
jiaxian_d13o.rs = cbind(2 + (jiaxian2$d13o - min(tix)) / diff(range(tix)),
                        2 + (jiaxian2$d13o.low - min(tix)) / diff(range(tix)),
                        2 + (jiaxian2$d13o.high - min(tix)) / diff(range(tix)),
                        jiaxian2$age)
arrows(jiaxian_d13o.rs[, 2], jiaxian_d13o.rs[, 4], 
       jiaxian_d13o.rs[, 3], jiaxian_d13o.rs[, 4], 
       col = "grey80", angle=90, length=0, code = 0)
points(jiaxian_d13o.rs[, 1], jiaxian_d13o.rs[, 4], col = pal[1], bg = "white", pch = 21, cex = .8)
axis(1, 2 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[OOM]*" (\u2030)"), 1, line = 2, at = 2.5)
text(2.5, 2, "Jiaxian")

# northern China
northern_china2 = northern_china |> filter(age <= 7 & age >= 2)
xext = range(northern_china2$d13o)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
northern_china.rs = cbind(3 + (northern_china2$d13o - min(tix)) / diff(range(tix)),
                          northern_china2$age)
lines(northern_china.rs[, 1], northern_china.rs[, 2], col = pal[3])
points(northern_china.rs[, 1], northern_china.rs[, 2], col = pal[3], bg = "white", pch = 21, cex = .8)
axis(3, 3 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 3, line = 2, at = 3.5)
text(3.5, 5.5, "G3")

# Japan Sea
japan2 = japan |> filter(age <= 7 & age >= 2)
xext = range(japan2$d13o)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 1)
japan.rs = cbind(4 + (japan2$d13o - min(tix)) / diff(range(tix)),
                 japan2$age)
lines(japan.rs[, 1], japan.rs[, 2], col = pal[2])
points(japan.rs[, 1], japan.rs[, 2], col = pal[2], bg = "white", pch = 22, cex = .8)
axis(1, 4 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 1, line = 2, at = 4.5)
text(4.4, 2.1, "U1430", cex = .8)

# benthic
benthic2 = benthic |> filter(age <= 7 & age >= 2)
xext = range(benthic2$d18O)
tix = seq(floor(min(xext)*10), ceiling(max(xext)*10), by = 5)/10
benthic.rs = cbind(4.7 + (benthic2$d18O - min(tix)) / diff(range(tix)),
                   benthic2$age)
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1])
axis(3, 4.7 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"18"*"O"[benthic]*" (\u2030)"), 3, line = 2, at = 5.2)

# dust
xext = range(dust_1208$low, dust_1208$high)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 1)
dust1208.rs = cbind(5.7 + (dust_1208$flux - min(tix)) / diff(range(tix)),
                5.7 + (dust_1208$low - min(tix)) / diff(range(tix)),
                5.7 + (dust_1208$high - min(tix)) / diff(range(tix)),
                dust_1208$age)
arrows(dust1208.rs[, 2], dust1208.rs[, 4], 
       dust1208.rs[, 3], dust1208.rs[, 4], 
       col = "grey80", angle=90, length=0, code = 0)
points(dust1208.rs[, 1], dust1208.rs[, 4], col = pal[2], bg = "white", pch = 21, cex = .8)
axis(1, 5.7 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(-4.5, -4.8, -5.5))
mtext(expression("flux (g/cm"^"2"*"/kyr)"), 1, line = -3.5, at = 6.2)
text(6, 4.8, "ODP 1208", col = pal[2], cex = .8)

xext = range(dust_885$low, dust_885$high)
tix = seq(floor(min(xext)*10), ceiling(max(xext)*10), by = 1) / 10
dust885.rs = cbind(6.4 + (dust_885$flux - min(tix)) / diff(range(tix)),
                   6.4 + (dust_885$low - min(tix)) / diff(range(tix)),
                   6.4 + (dust_885$high - min(tix)) / diff(range(tix)),
                   dust_885$age)
arrows(dust885.rs[, 2], dust885.rs[, 4], 
       dust885.rs[, 3], dust885.rs[, 4], 
       col = "grey80", angle=90, length=0, code = 0)
points(dust885.rs[, 1], dust885.rs[, 4], col = pal[3], bg = "white", pch = 21, cex = .8)
axis(3, 6.4 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression("flux (g/cm"^"2"*"/kyr)"), 3, line = 2, at = 6.9)
text(6.9, 2.2, "ODP 885/886", col = pal[4], cex = .8)

axis(4, at = seq(2, 7, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 4, line = 1)

rect(xleft = 0, ybottom = 5, xright = 7.5, ytop = 4, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
rect(xleft = 0, ybottom = 3.5, xright = 7.5, ytop = 3, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
rect(xleft = 0, ybottom = 2.8, xright = 7.5, ytop = 2.5, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
text(.2, 6.8, "a", cex = 1, col = "black", font = 2)
text(1.1, 2.2, "b", cex = 1, col = "black", font = 2)
text(2.1, 6.8, "c", cex = 1, col = "black", font = 2)
text(3.5, 6.8, "d", cex = 1, col = "black", font = 2)
text(4.2, 6.8, "e", cex = 1, col = "black", font = 2)
text(5.2, 6.8, "f", cex = 1, col = "black", font = 2)
text(6.5, 4.8, "g", cex = 1, col = "black", font = 2)
text(7.3, 4.2, "h", cex = 1, col = "black", font = 2)

dev.off()

