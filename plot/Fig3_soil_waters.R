rm(list = ls())
pacman::p_load(tidyverse, ggpubr, readxl)

## read data ----
D47 = read.csv("output/D47.csv")
D47= D47[order(D47$age),] 
d18c = read.csv("output/d18c.csv")
dp17 = read.csv("output/dp17.csv")
ob.am = read.csv("output/ob.am.csv")
ob = read_xlsx("data/global_records/Pliocene_orbital_and_insolation_data.xlsx")
sig.am = read.csv("output/sig.am.csv")

## time series plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
site1 = pal[factor(D47$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
site2 = pal[factor(d18c$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
site3 = pal[factor(dp17$section, levels = c("Lantian", "Shilou", "Jiaxian"))]

png("figures/Fig3.soil_water.png", 4.5, 7, units = "in", res = 500)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2.5, 7.5), ylim = c(0, 5), axes = FALSE,
     xlab = "", ylab = "")
rect(xleft = 3.3, xright = 3.8, ybottom = 0, ytop = 5, border = NA, col = rgb(1,0,0,.1))
rect(xleft = 4.7, xright = 5.3, ybottom = 0, ytop = 5, border = NA, col = rgb(1,0,0,.1))
rect(xleft = 6, xright = 6.4, ybottom = 0, ytop = 5, border = NA, col = rgb(1,0,0,.1))
abline(v = 5.32, col = "black", lty = 2)

yext = range(ob$obliquity)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+1)), by = 5)/10
ob.rs = cbind(ob$age,
               4 + (ob$obliquity - min(tix)) / diff(range(tix)))
lines(ob.rs[, 1], ob.rs[, 2], col = pal[1])
ob.am.rs = cbind(ob.am$age,
              4 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = pal[2], lwd = 2)
axis(2, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext("obliquity", 2, line = 2.5, at = 4.5)

yext = range(ob$gradient)
tix = seq(ceiling(max(yext)), 
          floor(min(yext-2.9)), by = -5)
sig.rs = cbind(ob$age,
              4 - (ob$gradient - min(tix)) / diff(range(tix)))
lines(sig.rs[, 1], sig.rs[, 2], col = pal[5])
sig.am.rs = cbind(sig.am$age,
                 4 - (sig.am$sig.am - min(tix)) / diff(range(tix)))
lines(sig.am.rs[, 1], sig.am.rs[, 2], col = pal[6], lwd = 2)
axis(4, 4 - (tix - min(tix)) / diff(range(tix)), tix)
mtext("SIG", 4, line = 2.5, at = 3.5)

yext = range(D47$st.low, D47$st.high)
tix = seq(floor(min(yext) + 1), 
          ceiling(max(yext)), by = 10)
D47.rs = cbind(D47$age,
               2 + (D47$temp - min(tix)) / diff(range(tix)),
               2 + (D47$st.low - min(tix)) / diff(range(tix)),
               2 + (D47$st.high - min(tix)) / diff(range(tix)))
arrows(D47.rs[, 1], D47.rs[, 3], D47.rs[, 1], D47.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(D47.rs[, 1], D47.rs[, 2], col = "black", bg = site1, pch = 21, cex = 1.5)
loess_fit = loess(data = D47, temp ~ age, span = 0.2)
pred = predict(loess_fit, se = TRUE)
lines(lowess(D47.rs[, 1], D47.rs[, 2], f = 0.3), lwd = 3)
axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("T"[Delta*"47"]*" (", degree, "C)")), 2, line = 2.5, at = 2.5)

# soil water d18O using paired data
# yext = range(D47$d18sw.low, D47$d18sw.high)
# tix = seq(floor(min(yext)),
#           ceiling(max(yext)), by = 2)
# D47.rs = cbind(D47$age,
#                1 + (D47$d18sw - min(tix)) / diff(range(tix)),
#                1 + (D47$d18sw.low - min(tix)) / diff(range(tix)),
#                1 + (D47$d18sw.high - min(tix)) / diff(range(tix)))
# arrows(D47.rs[, 1], D47.rs[, 3], D47.rs[, 1], D47.rs[, 4], col = "black",
#        angle=90, length=0, code = 0)
# points(D47.rs[, 1], D47.rs[, 2], col = "black", bg = site1, pch = 21, cex = 1.5)
# loess_fit = loess(data = D47, d18sw ~ age, span = 0.2)
# pred = predict(loess_fit, se = TRUE)
# lines(lowess(D47.rs[, 1], D47.rs[, 2], f = 0.3), lwd = 3)
# axis(4, 1 + (tix - min(tix)) / diff(range(tix)), tix)
# mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 4, line = 2.5, at = 1.5)

# soil water d18O using interpolated data
yext = range(d18c$d18sw.low, d18c$d18sw.high)
tix = seq(floor(min(yext)),
          ceiling(max(yext)), by = 2)
d18c.rs = cbind(d18c$age,
                1 + (d18c$d18sw - min(tix)) / diff(range(tix)),
                1 + (d18c$d18sw.low - min(tix)) / diff(range(tix)),
                1 + (d18c$d18sw.high - min(tix)) / diff(range(tix)))
arrows(d18c.rs[, 1], d18c.rs[, 3], d18c.rs[, 1], d18c.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(d18c.rs[, 1], d18c.rs[, 2], col = "black", bg = site2, pch = 21, cex = 1.2)
axis(4, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 4, line = 2.5, at = 1.5)

yext = range(dp17$Dp17sw.low, dp17$Dp17sw.high)
tix = seq(ceiling(min(yext)+27), floor(max(yext+20)), 
          by = 50)
dp17.rs = cbind(dp17$age,
                1 - (dp17$Dp17sw - min(tix)) / diff(range(tix)),
                1 - (dp17$Dp17sw.low - min(tix)) / diff(range(tix)),
                1 - (dp17$Dp17sw.high - min(tix)) / diff(range(tix)))
arrows(dp17.rs[, 1], dp17.rs[, 3], dp17.rs[, 1], dp17.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(dp17.rs[, 1], dp17.rs[, 2], col = "black", bg = site3, pch = 21, cex = 1.5)
axis(2, 1 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(Delta^"'17"*"O"[sw]*" (per meg)"), 2, line = 2.5, at = 0.5)

axis(1, at = seq(2.5, 7.5, 1), cex = 1, mgp = c(1, .7, 0))
mtext("Age (Ma)", 1, line = 2)

text(5, 0, "Lantian", col = pal[1])
text(6, 0, "Shilou", col = pal[2])
text(7, 0, "Jiaxian", col = pal[3])
text(6.2, 5.1, "MPB:5.32 Ma", col = "black", cex = .8)
text(3, 4.9, "a", cex = 1.5, col = "black", font = 2)
text(3, 3.9, "b", cex = 1.5, col = "black", font = 2)
text(2.5, 2.9, "c", cex = 1.5, col = "black", font = 2)
text(2.5, 1.9, "d", cex = 1.5, col = "black", font = 2)
text(2.5, .9, "e", cex = 1.5, col = "black", font = 2)

dev.off()

