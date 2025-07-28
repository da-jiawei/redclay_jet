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

# Pliocene - Pleistocene plot ----
png("figures/Fig4.Pleistocene_time_series.png", width = 5.21, height = 7.17, units = "in", res = 500)
par(mar = c(2, 4, 2, 4))
plot(-10, 0, xlim = c(0, 3.6), ylim = c(0, 7), axes = FALSE,
     xlab = "", ylab = "")
axis(3, at = seq(0, 3.6, 0.2), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 3, line = 1)

benthic_pm = benthic |> filter(age < 3.6)
yext = range(benthic_pm$d18O)
tix = seq(floor(min(yext*10)), ceiling(max(yext*10)+2), 5) / 10 
benthic.rs = cbind(benthic_pm$age, 7 - (benthic_pm$d18O - min(tix)) / diff(range(tix)))
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1])
axis(2, 7 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Benthic "*delta^"18"*"O (\u2030)"), 2, line = 2, at = 6.5)

japan_pm = japan |> filter(age < 3.6)
yext = range(japan_pm$d13o)
tix = seq(floor(min(yext)), ceiling(max(yext)), 2)
japan.rs = cbind(japan_pm$age, 4.7 + (japan_pm$d13o - min(tix)) / diff(range(tix)))
lines(japan.rs[, 1], japan.rs[, 2], col = pal[2])
points(japan.rs[, 1], japan.rs[, 2], pch = 21, col = pal[2], bg = "white", cex = .8)
axis(4, 4.7 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[BC]*" (\u2030)"), 4, line = 2.5, at = 5.2)
text(3.2, 5, label = "U1430", cex = 1)
rect(1.4, 5.9, 3.6, 6, col = pal[4], lwd = 0)

g3_pm = northern_china |> filter(age < 3.6)
yext = range(g3_pm$d13o)
tix = seq(floor(min(yext)), ceiling(max(yext)), 2)
g3.rs = cbind(g3_pm$age, 3.5 + (g3_pm$d13o - min(tix)) / diff(range(tix)))
lines(g3.rs[, 1], g3.rs[, 2], col = "black")
points(g3.rs[, 1], g3.rs[, 2], pch = 21, bg = pal[3], col = "black", cex = .8)
axis(2, 3.5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 2, line = 2.5, at = 4)
text(0.4, 4, label = "G3", cex = 1)
rect(2.1, 4.6, 3.2, 4.7, col = pal[4], lwd = 0)

lingtai_pm = lingtai |> filter(age < 3.6)
yext = range(lingtai_pm$d13)
tix = seq(floor(min(yext)), ceiling(max(yext)), 2)
lingtai.rs = cbind(lingtai_pm$age, 2.2 + (lingtai_pm$d13 - min(tix)) / diff(range(tix)))
lines(lingtai.rs[, 1], lingtai.rs[, 2], col = pal[5])
axis(4, 2.2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[BC]*" (\u2030)"), 4, line = 2.5, at = 2.7)
text(3.2, 2.9, label = "Lingtai", cex = 1)
rect(0.85, 3.2, 2.5, 3.3, col = pal[4], lwd = 0)

yext = range(lantian$d13o)
tix = seq(floor(min(yext)), ceiling(max(yext)), 2)
lingtai.rs = cbind(lantian$age, 1 + (lantian$d13o - min(tix)) / diff(range(tix)))
lines(lingtai.rs[, 1], lingtai.rs[, 2], col = pal[6])
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 2, line = 2.5, at = 1.5)
text(3.2, 1.5, label = "Lantian", cex = 1)
rect(0.09, 2, 1.35, 2.1, col = pal[4], lwd = 0)

co2_pl = co2 |> filter(ages < 3.6)
yext = range(co2_pl$low, co2_pl$high)
tix = seq(200, 350, 50)
co2.rs = cbind(co2_pl$ages, 0 + (co2_pl$mean - min(tix)) / diff(range(tix)),
               0 + (co2_pl$low - min(tix)) / diff(range(tix)),
               0 + (co2_pl$high - min(tix)) / diff(range(tix)))
polygon(c(co2.rs[, 1], rev(co2.rs[, 1])), c(co2.rs[, 3], rev(co2.rs[, 4])), col = pal[3], border = NA)
lines(co2.rs[, 1], co2.rs[, 2], col = pal[1], lwd = 3)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CO"[2]*" (ppmv)"), 4, line = 2.5, at = 0.5)

axis(1, at = seq(0, 3.6, 0.2), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 1, line = 1)

text(3.5, 6.5, "a", font = 2)
text(3.5, 5.7, "b", font = 2)
text(3.5, 4, "c", font = 2)
text(3.5, 2.2, "d", font = 2)
text(3.5, 1.8, "e", font = 2)
text(3.5, 0.2, "f", font = 2)

dev.off()