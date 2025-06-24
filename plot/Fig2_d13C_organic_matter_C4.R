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
ob.am = read_csv("output/ob.am.csv")
co2 = read_csv("data/global_records/100kyrCO2.csv") |> filter(ages <= 8) |>
  mutate(mean = exp(`50%`),
         low = exp(`2.5%`),
         high = exp(`97.5%`)) |>
  select(ages, mean, low, high)
DSST = read_csv("output/DSST.csv")
#### 8 Ma Plot ----
png("figures/Fig2.d13_organic_matter_time_series.png", 10, 4.6, units = "in", res = 500)
par(mar = c(4, 2, 4, 2))
plot(-1, 0, xlim = c(0, 7), ylim = c(8, 0), axes = FALSE,
     xlab = "", ylab = "")
axis(2, at = seq(0, 8, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 2, line = 1)
# CO2
xext = range(co2$low, co2$high)
tix = seq(200, 450, 50)
co2.rs = cbind(1 - (co2$mean - min(tix)) / diff(range(tix)),
               1 - (co2$low - min(tix)) / diff(range(tix)),
               1 - (co2$high - min(tix)) / diff(range(tix)),
               co2$ages)
polygon(c(co2.rs[, 2], rev(co2.rs[, 3])), c(co2.rs[, 4], rev(co2.rs[, 4])), col = pal[1], border = NA)
lines(co2.rs[, 1], co2.rs[, 4], col = pal[2], lwd = 3)
axis(1, 1 - (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression("CO"[2]*" (ppmv)"), 1, line = 2, at = 0.5)

# DSST
xext = range(DSST$DTP_ML, na.rm = TRUE)
tix = seq(7, 13, 2)
dsst.rs = cbind(.9 + (DSST$DTP_ML - min(tix)) / diff(range(tix)),
                DSST$age)
lines(dsst.rs[, 1], dsst.rs[, 2], col = pal[3], lwd = 3)
axis(3, .9 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(paste(Delta*"SST (", degree, "C)")), 3, line = 2, at = 1.4)

# lingtai
xext = range(lingtai$d13, lingtai$d13_wet, lingtai$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 5)
lingtai.rs = cbind(1.7 + (lingtai$d13 - min(tix)) / diff(range(tix)),
                   1.7 + (lingtai$d13_wet - min(tix)) / diff(range(tix)),
                   1.7 + (lingtai$d13_dry - min(tix)) / diff(range(tix)),
                   lingtai$age)
# lines(lingtai.rs[, 2], lingtai.rs[, 4], col = "grey70", lwd = 1)
# lines(lingtai.rs[, 3], lingtai.rs[, 4], col = "grey70", lwd = 1)
lines(lingtai.rs[, 1], lingtai.rs[, 4], col = pal[2])
axis(1, 1.7 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 1, line = 2, at = 2.2)
text(2.2, 7.5, "Lingtai", col = pal[2])
arrows(2.3, 3, 2.9, .75, length = .1, lwd = 2)

# jiaxian
xext = range(jiaxian$d13o.low, jiaxian$d13o.high, jiaxian$d13_wet, jiaxian$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
jiaxian_d13o.rs = cbind(2.4 + (jiaxian$d13o - min(tix)) / diff(range(tix)),
                        2.4 + (jiaxian$d13o.low - min(tix)) / diff(range(tix)),
                        2.4 + (jiaxian$d13o.high - min(tix)) / diff(range(tix)),
                        2.4 + (jiaxian$d13_wet - min(tix)) / diff(range(tix)),
                        2.4 + (jiaxian$d13_dry - min(tix)) / diff(range(tix)),
                        jiaxian$age)
arrows(jiaxian_d13o.rs[, 2], jiaxian_d13o.rs[, 6], 
       jiaxian_d13o.rs[, 3], jiaxian_d13o.rs[, 6], 
       col = "grey80", angle=90, length=0, code = 0)
# lines(jiaxian_d13o.rs[, 4], jiaxian_d13o.rs[, 6], col = "grey70", lwd = 1)
# lines(jiaxian_d13o.rs[, 5], jiaxian_d13o.rs[, 6], col = "grey70", lwd = 1)
points(jiaxian_d13o.rs[, 1], jiaxian_d13o.rs[, 6], col = pal[1], bg = "white", pch = 21, cex = .8)
axis(3, 2.4 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[OOM]*" (\u2030)"), 3, line = 2, at = 2.9)
text(3, 2, "Jiaxian", col = pal[1])

# Northern China
xext = range(northern_china$d13o, northern_china$d13_wet, northern_china$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
northern_china.rs = cbind(3.2 + (northern_china$d13o - min(tix)) / diff(range(tix)),
                          3.2 + (northern_china$d13_wet - min(tix)) / diff(range(tix)),
                          3.2 + (northern_china$d13_dry - min(tix)) / diff(range(tix)),
                          northern_china$age)
# lines(northern_china.rs[, 2], northern_china.rs[, 4], col = "grey70", lwd = 1)
# lines(northern_china.rs[, 3], northern_china.rs[, 4], col = "grey70", lwd = 1)
lines(northern_china.rs[, 1], northern_china.rs[, 4], col = pal[3])
points(northern_china.rs[, 1], northern_china.rs[, 4], col = pal[3], bg = "white", pch = 21, cex = .8)
axis(1, 3.2 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 1, line = 2, at = 3.7)
text(3.8, 5, "G3", col = pal[3])
arrows(3.9, 4.1, 4.3, 2.5, length = .1, lwd = 2)

# Japan sea 
xext = range(japan$d13o, japan$d13_wet, japan$d13_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
japan.rs = cbind(4 +(japan$d13o - min(tix)) / diff(range(tix)),
                 4 +(japan$d13_wet - min(tix)) / diff(range(tix)),
                 4 +(japan$d13_dry - min(tix)) / diff(range(tix)),
                 japan$age)
# lines(japan.rs[, 3], japan.rs[, 4], col = "grey70")
lines(japan.rs[, 1], japan.rs[, 4], col = pal[2])
points(japan.rs[, 1], japan.rs[, 4], col = pal[2], bg = "white", pch = 22, cex = .8)
axis(3, 4 +(tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 3, line = 2, at = 4.5)
text(4.4, 4, "U1430", col = pal[2])
arrows(4.7, 5.5, 5, 4.5, length = .1, lwd = 2)
arrows(4.9, 3.4, 5.3, 1.5, length = .1, lwd = 2)

# tooth enamel
sites = pal[factor(teeth$section, levels = c("Lantian", "Baode", "Yushe"))] 
xext = range(teeth$d13, teeth_d13max$d13te_dry)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 2)
teeth.rs = cbind(5 + (teeth$d13 - min(tix)) / diff(range(tix)),
                 teeth$age)
# teeth_max.rs = cbind(4 + (teeth_d13max$d13te_dry - min(tix)) / diff(range(tix)),
#                      teeth_d13max$age)
# lines(teeth_max.rs[, 1], teeth_max.rs[, 2], col = "grey70", lwd = 1)
points(teeth.rs[, 1], teeth.rs[, 2], col = "black", bg = sites, pch = 22, cex = .8)
axis(1, 5 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"13"*"C"[TE]*" (\u2030)"), 1, line = 2, at = 5.5)
legend(x = 5.2, y = -.2, legend = c("Lantian", "Baode", "Yushe"),
       pch = 22, cex = 0.8, pt.cex = 1, pt.bg = pal,
       x.intersp = .8, y.intersp = .8, text.width = 0.4)

# benthic d18O
xext = range(benthic$d18O)
tix = seq(floor(min(xext)), ceiling(max(xext)), by = 1)
benthic.rs = cbind(6 + (benthic$d18O - min(tix)) / diff(range(tix)),
                   benthic$age)
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1])
axis(3, 6 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression(delta^"18"*"O"[benthic]*" (\u2030)"), 3, line = 2, at = 6.5)

rect(xleft = 0, ybottom = 3.5, xright = 7, ytop = .75, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)

text(.1, .2, "a", cex = 1, col = "black", font = 2)
text(1, .2, "b", cex = 1, col = "black", font = 2)
text(2.5, .2, "c", cex = 1, col = "black", font = 2)
text(3, 7.8, "d", cex = 1, col = "black", font = 2)
text(3.8, 7.8, "e", cex = 1, col = "black", font = 2)
text(4.8, 7.8, "f", cex = 1, col = "black", font = 2)
text(5.8, 7.8, "g", cex = 1, col = "black", font = 2)
text(6.8, 7.8, "h", cex = 1, col = "black", font = 2)

axis(4, at = seq(0, 8, 1), cex = 1, mgp = c(0, -0.3, -1))
mtext("Age (Ma)", 4, line = 1)

dev.off()



