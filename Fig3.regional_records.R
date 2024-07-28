library(tidyverse)
library(readxl)

sw = read.csv("out/d18c.csv")
snail = read_xls("data/regional records/land snails.xls", sheet = "Xifeng")
iron = read_xlsx("data/regional records/freeiron.xlsx", sheet = "Pianguan")
smi = read_xlsx("data/regional records/SMI.xlsx") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2 & age < 7.5) %>%
  drop_na(SMI)
bc = read_xlsx("data/regional records/black carbon.xlsx") %>%
  filter(age > 2 & age < 7.5 & d13C < -15) %>%
  drop_na(d13C)
rbsr = read_xlsx("data/regional records/RbSr.xlsx")

## plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
site = pal[factor(sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]

png("figures/regional_records.png", 4, 6, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 4), axes = FALSE,
     xlab = "", ylab = "")
yext = range(sw$d18sw.low, sw$d18sw.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
sw.rs = cbind(sw$age,
                3 + (sw$d18sw - min(tix)) / diff(range(tix)),
                3 + (sw$d18sw.low - min(tix)) / diff(range(tix)),
                3 + (sw$d18sw.high - min(tix)) / diff(range(tix)))
arrows(sw.rs[, 1], sw.rs[, 3], sw.rs[, 1], sw.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(sw.rs[, 1], sw.rs[, 2], col = "black", bg = site, pch = 21, cex = 1.2)
axis(2, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 2, line = 2.5, at = 3.5)

yext = range(smi$SMI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 0.2)
smi.rs = cbind(smi$age,
              2.4 + (smi$SMI - min(tix)) / diff(range(tix)))
lines(smi.rs[, 1], smi.rs[, 2], col = pal[1], pch = 21, cex = 1.2)
axis(4, 2.4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("SMI"), 4, line = 2.5, at = 2.9)

yext = range(iron$Fe)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10)), by = 1) /10
iron.rs = cbind(iron$age,
               1.5 + (iron$Fe - min(tix)) / diff(range(tix)))
lines(iron.rs[, 1], iron.rs[, 2], col = pal[2], pch = 21, cex = 1.2)
axis(2, 1.5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Free iron content"), 2, line = 2.5, at = 2)

yext = range(rbsr$RbSr)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10)), by = 5) / 10
rbsr.rs = cbind(rbsr$age,
                0.8 + (rbsr$RbSr - min(tix)) / diff(range(tix)))
lines(rbsr.rs[, 1], rbsr.rs[, 2], col = pal[3], pch = 21, cex = 1.2)
axis(4, 0.8 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Rb/Sr"), 4, line = 2.5, at = 1.4)

yext = range(bc$d13C)
tix = seq(ceiling(max(yext)), 
          floor(min(yext)), by = -5)
bc.rs = cbind(bc$age,
                0.9 - (bc$d13C - min(tix)) / diff(range(tix)))
lines(bc.rs[, 1], bc.rs[, 2], col = pal[4], pch = 21, cex = 1.2)
axis(2, 0.9 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 2, line = 2.5, at = 0.4)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()
