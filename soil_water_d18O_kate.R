library(tidyverse)
library(ggpubr)
library(readxl)

D47 = read.csv("out/D47.csv")
D47= D47[order(D47$age),] 
d18c = read.csv("out/d18c.csv")
for (i in 1:nrow(d18c)) {
  d18c_pdb = rnorm(10000, d18c$d18O[i], 0.2)
  temp = rnorm(10000, 20, 5)
  d18c_vsmow = (1.03091 * d18c_pdb) + 30.91
  alpha = exp((16.1*1000/(273.15 + temp)-24.6)/1000) # Tremaine 2011
  d18sw = (1000 + d18c_vsmow) / alpha - 1000
  d18c$d18sw2[i] = mean(d18sw)
  d18c$d18sw2.se[i] = sd(d18sw)
}
d18c = d18c %>%
  mutate(d18sw2.low = d18sw2 - d18sw2.se, d18sw2.high = d18sw2 + d18sw2.se)


pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
site1 = pal[factor(D47$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
site2 = pal[factor(d18c$site, levels = c("Lantian", "Shilou", "Jiaxian"))]

png("figures/soil_water_kate.png", 5, 5, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 3.5), axes = FALSE,
     xlab = "", ylab = "")

legend(x = 6, y = 3.5, legend = c("Lantian", "Shilou", "Jiaxian"),
       col = pal, pch = 16, cex = 0.8, pt.cex = 1.5)

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

yext = range(d18c$d18sw2.low, d18c$d18sw2.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
d18c.rs = cbind(d18c$age,
                0 + (d18c$d18sw2 - min(tix)) / diff(range(tix)),
                0 + (d18c$d18sw2.low - min(tix)) / diff(range(tix)),
                0 + (d18c$d18sw2.high - min(tix)) / diff(range(tix)))
arrows(d18c.rs[, 1], d18c.rs[, 3], d18c.rs[, 1], d18c.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(d18c.rs[, 1], d18c.rs[, 2], col = "black", bg = site2, pch = 21, cex = 1.2)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 2, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()



