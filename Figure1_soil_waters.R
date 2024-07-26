library(tidyverse)

## read data ----
D47 = read.csv("out/D47.csv")
d18c = read.csv("out/d18c.csv")
dp17 = read.csv("out/dp17.csv")

## plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
site1 = pal[factor(D47$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
site2 = pal[factor(d18c$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
site3 = pal[factor(dp17$section, levels = c("Lantian", "Shilou", "Jiaxian"))]

# png("figures/soil_water.png", 4.7, 5, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 3.3), axes = FALSE,
     xlab = "", ylab = "")

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
lines(lowess(D47.rs[, 1], D47.rs[, 2], f = 0.3), col = "black", lwd = 3)
axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("T"[Delta*"47"]*" (", degree, "C)")), 2, line = 2.5, at = 2.5)
legend(x = 6.2, y = 3.3, legend = c("Lantian", "Shilou", "Jiaxian"),
       col = pal, pch = 16, cex = 0.8, pt.cex = 1.5)

# # soil water d18O using paired data
# yext = range(D47$d18sw.low, D47$d18sw.high)
# tix = seq(floor(min(yext)), 
#           ceiling(max(yext)), by = 2)
# D47.rs = cbind(D47$age,
#                2 + (D47$d18sw - min(tix)) / diff(range(tix)),
#                2 + (D47$d18sw.low - min(tix)) / diff(range(tix)),
#                2 + (D47$d18sw.high - min(tix)) / diff(range(tix)))
# arrows(D47.rs[, 1], D47.rs[, 3], D47.rs[, 1], D47.rs[, 4], col = "black",
#        angle=90, length=0, code = 0)
# points(D47.rs[, 1], D47.rs[, 2], col = "black", bg = site, pch = 21, cex = 1.5)
# axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
# mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 2, line = 2.5, at = 2.5)

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
tix = seq(ceiling(max(yext) + 2), 
          floor(min(yext)), by = -50)
dp17.rs = cbind(dp17$age,
                1 - (dp17$Dp17sw - min(tix)) / diff(range(tix)),
                1 - (dp17$Dp17sw.low - min(tix)) / diff(range(tix)),
                1 - (dp17$Dp17sw.high - min(tix)) / diff(range(tix)))
arrows(dp17.rs[, 1], dp17.rs[, 3], dp17.rs[, 1], dp17.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(dp17.rs[, 1], dp17.rs[, 2], col = "black", bg = site3, pch = 21, cex = 1.5)
axis(2, 1 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(Delta^"'17"*"O"[sw]*" (per meg)"), 2, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()