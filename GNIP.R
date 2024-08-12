library(tidyverse)
library(readxl)
library(ggpubr)
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
theme <- theme(axis.text.x = element_text(margin = margin(t = 0.1, unit = "cm")),
               axis.text.y = element_text(margin = margin(r = 0.1, unit = "cm")),
               axis.ticks.length=unit(0.15, "cm"),
               axis.ticks = element_line(colour = "black"),
               text = element_text(color = "black", size = 12),
               axis.title = element_text(size = 15), 
               axis.text = element_text(color = "black", size = 12),
               plot.title = element_text(hjust = 0.1, vjust = -10),
               legend.text = element_text(size = 12),
               legend.title = element_text(size = 12),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank())
dat = read_xlsx("data/regional records/GNIP.xlsx") %>%
  mutate(d18.low = d18 - d18.sd,
         d18.high = d18 + d18.sd)

## d18Op ----
png("figures/GNIP_d18Op.png", 4, 6, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(1, 12), ylim = c(0, 5), axes = FALSE,
     xlab = "", ylab = "")
sjz = dat %>% filter(site == "Shijiazhuang")
yext = range(sjz$d18.low, sjz$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
sjz.rs = cbind(sjz$month,
              4 + (sjz$d18 - min(tix)) / diff(range(tix)),
              4 + (sjz$d18.low - min(tix)) / diff(range(tix)),
              4 + (sjz$d18.high - min(tix)) / diff(range(tix)))
arrows(sjz.rs[, 1], sjz.rs[, 3], sjz.rs[, 1], sjz.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(sjz.rs[, 1], sjz.rs[, 2], col = "black")
points(sjz.rs[, 1], sjz.rs[, 2], col = "black", bg = pal[1], pch = 21, cex = 1.2)
axis(2, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 2, line = 2.5, at = 4.5)

xa = dat %>% filter(site == "Xi'an")
yext = range(xa$d18.low, xa$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
xa.rs = cbind(xa$month,
               3 + (xa$d18 - min(tix)) / diff(range(tix)),
               3 + (xa$d18.low - min(tix)) / diff(range(tix)),
               3 + (xa$d18.high - min(tix)) / diff(range(tix)))
arrows(xa.rs[, 1], xa.rs[, 3], xa.rs[, 1], xa.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(xa.rs[, 1], xa.rs[, 2], col = "black")
points(xa.rs[, 1], xa.rs[, 2], col = "black", bg = pal[2], pch = 21, cex = 1.2)
axis(4, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 4, line = 2.5, at = 3.5)

zy = dat %>% filter(site == "Zunyi")
yext = range(zy$d18.low, zy$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
zy.rs = cbind(zy$month,
              2 + (zy$d18 - min(tix)) / diff(range(tix)),
              2 + (zy$d18.low - min(tix)) / diff(range(tix)),
              2 + (zy$d18.high - min(tix)) / diff(range(tix)))
arrows(zy.rs[, 1], zy.rs[, 3], zy.rs[, 1], zy.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(zy.rs[, 1], zy.rs[, 2], col = "black")
points(zy.rs[, 1], zy.rs[, 2], col = "black", bg = pal[3], pch = 21, cex = 1.2)
axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 2, line = 2.5, at = 2.5)

gl = dat %>% filter(site == "Guilin")
yext = range(gl$d18.low, gl$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
gl.rs = cbind(gl$month,
              1 + (gl$d18 - min(tix)) / diff(range(tix)),
              1 + (gl$d18.low - min(tix)) / diff(range(tix)),
              1 + (gl$d18.high - min(tix)) / diff(range(tix)))
arrows(gl.rs[, 1], gl.rs[, 3], gl.rs[, 1], gl.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(gl.rs[, 1], gl.rs[, 2], col = "black")
points(gl.rs[, 1], gl.rs[, 2], col = "black", bg = pal[4], pch = 21, cex = 1.2)
axis(4, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 4, line = 2.5, at = 1.5)

hk = dat %>% filter(site == "Guangzhou")
yext = range(hk$d18.low, hk$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
hk.rs = cbind(hk$month,
              0 + (hk$d18 - min(tix)) / diff(range(tix)),
              0 + (hk$d18.low - min(tix)) / diff(range(tix)),
              0 + (hk$d18.high - min(tix)) / diff(range(tix)))
arrows(hk.rs[, 1], hk.rs[, 3], hk.rs[, 1], hk.rs[, 4], col = "black",
       anhke=90, length=0, code = 0)
lines(hk.rs[, 1], hk.rs[, 2], col = "black")
points(hk.rs[, 1], hk.rs[, 2], col = "black", bg = pal[5], pch = 21, cex = 1.2)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 2, line = 2.5, at = 0.5)

axis(1, 1:12)
mtext("Month", 1, line = 2)
dev.off()

## Precipitation ----
png("figures/GNIP_prep.png", 4, 6, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(1, 12), ylim = c(0, 5), axes = FALSE,
     xlab = "", ylab = "")
yext = range(sjz$prep)
tix = seq(floor(min(yext) - 5), 
          ceiling(max(yext)), by = 50)
sjz.rs = cbind(sjz$month,
               4 + (sjz$prep - min(tix)) / diff(range(tix)))
lines(sjz.rs[, 1], sjz.rs[, 2], col = "black")
points(sjz.rs[, 1], sjz.rs[, 2], col = "black", bg = pal[1], pch = 21, cex = 1.2)
axis(2, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext("rainfall (mm)", 2, line = 2.5, at = 4.5)

yext = range(xa$prep)
tix = seq(floor(min(yext) - 7), 
          ceiling(max(yext) + 5), by = 50)
xa.rs = cbind(xa$month,
              3 + (xa$prep - min(tix)) / diff(range(tix)))
lines(xa.rs[, 1], xa.rs[, 2], col = "black")
points(xa.rs[, 1], xa.rs[, 2], col = "black", bg = pal[2], pch = 21, cex = 1.2)
axis(4, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext("rainfall (mm)", 4, line = 2.5, at = 3.5)

yext = range(zy$prep)
tix = seq(floor(min(yext)-18), 
          ceiling(max(yext)+26), by = 50)
zy.rs = cbind(zy$month,
              2 + (zy$prep - min(tix)) / diff(range(tix)))
lines(zy.rs[, 1], zy.rs[, 2], col = "black")
points(zy.rs[, 1], zy.rs[, 2], col = "black", bg = pal[3], pch = 21, cex = 1.2)
axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext("rainfall (mm)", 2, line = 2.5, at = 2.5)

yext = range(gl$prep)
tix = seq(floor(min(yext)-34), 
          ceiling(max(yext)), by = 100)
gl.rs = cbind(gl$month,
              1 + (gl$prep - min(tix)) / diff(range(tix)))
lines(gl.rs[, 1], gl.rs[, 2], col = "black")
points(gl.rs[, 1], gl.rs[, 2], col = "black", bg = pal[4], pch = 21, cex = 1.2)
axis(4, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext("rainfall (mm)", 4, line = 2.5, at = 1.5)

yext = range(hk$prep)
tix = seq(floor(min(yext)-30), 
          ceiling(max(yext)), by = 100)
hk.rs = cbind(hk$month,
              0 + (hk$prep - min(tix)) / diff(range(tix)))
lines(hk.rs[, 1], hk.rs[, 2], col = "black")
points(hk.rs[, 1], hk.rs[, 2], col = "black", bg = pal[5], pch = 21, cex = 1.2)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext("rainfall (mm)", 2, line = 2.5, at = 0.5)

axis(1, 1:12)
mtext("Month", 1, line = 2)
dev.off()



