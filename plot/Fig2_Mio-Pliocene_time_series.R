rm(list = ls())
pacman::p_load(tidyverse, readxl)
pal = c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e")
pal_rc = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99")

# load data ----
lantian = read.csv("data/regional_records/d13_org/processed/lantian.csv")
jiaxian = read.csv("data/regional_records/d13_org/processed/jiaxian.csv")
lingtai = read.csv("data/regional_records/d13_org/processed/lingtai.csv") |> drop_na()
northern_china = read.csv("data/regional_records/d13_org/processed/northern_china.csv")
japan = read.csv("data/regional_records/d13_org/processed/japan_sea.csv") |> filter(age <= 8)

benthic = read_xlsx("data/global_records/LR04.xlsx") |> mutate(age = age / 1e3) |> filter(age <= 8)
co2 = read.csv("data/global_records/100kyrCO2.csv") |> filter(ages <= 8) |>
  mutate(mean = exp(X50.),
         low = exp(X2.5.),
         high = exp(X97.5.)) |>
  select(ages, mean, low, high)
DSST = read.csv("output/DSST.csv")
d18sw = read.csv("output/d18c.csv")
D47 = read.csv("output/D47.csv") |>
  select(site, age, temp, temp.sd) |>
  mutate(low = temp - temp.sd,
         high = temp + temp.sd)
ob.am = read.csv("output/ob.am.csv") |> filter(age >= 2.5)
dust_1208 = read_csv("data/regional_records/d13_org/processed/dust_1208.csv")

# Miocene-Pliocene plot ----
pdf("figures/Fig2.Miocene_Pliocene_time_series.pdf", 5.1, 7.7)
# png("figures/Fig2.Miocene_Pliocene_time_series.png", width = 5.1, height = 7.7, units = "in", res = 500)
par(mar = c(2, 4, 2, 4))
plot(-10, 0, xlim = c(2.5, 8), ylim = c(0, 9), axes = FALSE,
     xlab = "", ylab = "")
axis(3, at = seq(2.5, 8, 0.5), cex = 1, mgp = c(0.3, 0, -0.7))
mtext("Age (Ma)", 3, line = 1)

rect(xleft = 5.5, ybottom = 0, xright = 7.5, ytop = 9, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0)
rect(xleft = 5, ybottom = 0, xright = 5.5, ytop = 9, border = NA, col = rgb(1, 0, 0, 0.1), lwd = 0)
rect(xleft = 3.8, ybottom = 0, xright = 5, ytop = 9, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0)
rect(xleft = 3.5, ybottom = 0, xright = 3.8, ytop = 9, border = NA, col = rgb(1, 0, 0, 0.1), lwd = 0)
rect(xleft = 2.6, ybottom = 0, xright = 3.5, ytop = 9, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0)

benthic_pm = benthic |> filter(age > 2.5)
yext = range(benthic_pm$d18O)
tix = seq(floor(min(yext*10)), ceiling(max(yext*10)+2), 5) / 10 
benthic.rs = cbind(benthic_pm$age, 9 - (benthic_pm$d18O - min(tix)) / diff(range(tix)))
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1])
axis(2, 9 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Benthic "*delta^"18"*"O (\u2030)"), 2, line = 2, at = 8.5)

japan_pm = japan |> filter(age > 2.5)
yext = range(japan_pm$d13o)
tix = seq(floor(min(yext)), ceiling(max(yext)), 1)
japan.rs = cbind(japan_pm$age, 7.2 + (japan_pm$d13o - min(tix)) / diff(range(tix)))
lines(japan.rs[, 1], japan.rs[, 2], col = pal[2])
points(japan.rs[, 1], japan.rs[, 2], pch = 21, col = pal[2], bg = "white", cex = .8)
axis(4, 7.2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[BC]*" (\u2030)"), 4, line = 2.5, at = 7.7)
text(7, 8, label = "U1430", cex = .8)

g3_pm = northern_china |> filter(age > 3.5)
yext = range(g3_pm$d13o)
tix = seq(-26, -22, 1)
g3.rs = cbind(g3_pm$age, 6.2 + (g3_pm$d13o - min(tix)) / diff(range(tix)))
lines(g3.rs[, 1], g3.rs[, 2], col = "black")
points(g3.rs[, 1], g3.rs[, 2], pch = 21, bg = pal[3], col = "black", cex = .8)
axis(2, 6.2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 2, line = 2.5, at = 6.7)
text(7.2, 6.9, label = "G3", cex = .8)

jiaxian_pm = jiaxian |> filter(age > 2.5)
yext = range(jiaxian_pm$d13o)
tix = seq(floor(min(yext)), ceiling(max(yext)-1), 1)
jiaxian.rs = cbind(jiaxian_pm$age, 5.2 + (jiaxian_pm$d13o - min(tix)) / diff(range(tix)))
# lines(jiaxian.rs[, 1], jiaxian.rs[, 2], col = pal[4])
points(jiaxian.rs[, 1], jiaxian.rs[, 2], pch = 21, col = pal[4], cex = .8)
lines(lowess(jiaxian.rs[, 1], jiaxian.rs[, 2], f = 0.05), col = pal[6], lwd = 2)
axis(4, 5.2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[OOM]*" (\u2030)"), 4, line = 2.5, at = 5.7)
text(4, 5.5, label = "Jiaxian", cex = .9)

yext = range(dust_1208$flux)
tix = seq(0, 2.5, .5)
dust.rs = cbind(dust_1208$age, 4.5 + (dust_1208$flux - min(tix)) / diff(range(tix)))
lines(dust.rs[, 1], dust.rs[, 2], col = pal[1])
axis(2, 4.5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("flux (g/cm"^"2"*"/kyr)"), 2, line = 2, at = 5)
text(5.2, 4.7, "ODP 1208", cex = .8)

lingtai_pm = lingtai |> filter(age > 2.5)
yext = range(lingtai_pm$d13)
tix = seq(floor(min(yext)), ceiling(max(yext)), 2)
lingtai.rs = cbind(lingtai_pm$age, 3.5 + (lingtai_pm$d13 - min(tix)) / diff(range(tix)))
lines(lingtai.rs[, 1], lingtai.rs[, 2], col = pal[5])
axis(4, 3.5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[BC]*" (\u2030)"), 4, line = 2.5, at = 4)
text(7.5, 4, label = "Lingtai", cex = .8)

site = pal_rc[factor(d18sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
yext = range(d18sw$d18sw)
tix = seq(floor(min(yext)), ceiling(max(yext)), by = 2)
soil_water.rs = cbind(d18sw$age, 2.5 + (d18sw$d18sw - min(tix)) / diff(range(tix)))
points(soil_water.rs[, 1], soil_water.rs[, 2], col = "black", bg = site, pch = 21, cex = .8)
axis(2, 2.5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 2, line = 2.5, at = 3)

yext = range(ob.am$ob.am)
tix = seq(23.5, 24.4, .2)
ob.rs = cbind(ob.am$age, 2.5 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.rs[, 1], ob.rs[, 2], col = pal[2], lwd = 2)

site = pal_rc[factor(D47$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
yext = range(D47$low, D47$high)
tix = seq(floor(min(yext)+1), ceiling(max(yext)), by = 5)
D47.rs = cbind(D47$age, 
                      1.5 + (D47$temp - min(tix)) / diff(range(tix)),
                      1.5 + (D47$low - min(tix)) / diff(range(tix)),
                      1.5 + (D47$high - min(tix)) / diff(range(tix)))
arrows(D47.rs[, 1], D47.rs[, 3], D47.rs[, 1], D47.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(D47.rs[, 1], D47.rs[, 2], col = "black", bg = site, pch = 22, cex = 1)
axis(4, 1.5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("T"[Delta*"47"]*" (", degree, "C)")), 4, line = 2.5, at = 2)

yext = range(ob.am$ob.am)
tix = seq(23.5, 24.4, .2)
ob.rs = cbind(ob.am$age, 1.6 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.rs[, 1], ob.rs[, 2], col = pal[2], lwd = 2)

DSST_pm = DSST |> drop_na()
yext = range(DSST_pm$DTP_HL)
tix = seq(floor(min(yext)), ceiling(max(yext)), 1)
DSST.rs = cbind(DSST_pm$age, 1.8 - (DSST_pm$DTP_HL - min(tix)) / diff(range(tix)))
lines(DSST.rs[, 1], DSST.rs[, 2], col = pal[6], lwd = 3)
axis(2, 1.8 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(Delta*"SST (", degree, "C)")), 2, line = 2.5, at = 1.3)

co2_pm = co2 |> filter(ages > 2.5)
yext = range(co2_pm$low, co2_pm$high)
tix = seq(230, 410, 20)
co2.rs = cbind(co2_pm$ages,
               0 + (co2_pm$mean - min(tix)) / diff(range(tix)),
               0 + (co2_pm$low - min(tix)) / diff(range(tix)),
               0 + (co2_pm$high - min(tix)) / diff(range(tix)))
polygon(c(co2.rs[, 1], rev(co2.rs[, 1])), c(co2.rs[, 3], rev(co2.rs[, 4])), col = pal[3], border = NA)
lines(co2.rs[, 1], co2.rs[, 2], col = pal[1], lwd = 3)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CO"[2]*" (ppmv)"), 4, line = 2.5, at = 0.5)

axis(1, at = seq(2.5, 8, 0.5), cex = 1, mgp = c(0.3, 0, -0.7))
mtext("Age (Ma)", 1, line = 1)

text(7.8, 8.6, "a", font = 2)
text(7.8, 8, "b", font = 2)
text(7.8, 7, "c", font = 2)
text(7.8, 5.5, "d", font = 2)
text(7.8, 5, "e", font = 2)
text(7.8, 4.3, "f", font = 2)
text(7.8, 3.2, "g", font = 2)
text(7.8, 2.2, "h", font = 2)
text(7.8, 1.2, "i", font = 2)
text(7.8, .2, "j", font = 2)

dev.off()


