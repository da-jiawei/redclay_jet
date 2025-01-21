library(tidyverse)
library(readxl)
library(ggpubr)

## load data
sw = read.csv("out/d18c.csv")
smi = read_xlsx("data/regional records/SMI.xlsx") %>%
  mutate(age = age / 1000) %>%
  filter(age > 2 & age < 7.5) %>%
  drop_na(SMI)
gdgt = read_xlsx("data/regional records/gdgt.xlsx")
snail = read_xls("data/regional records/land snails.xls", sheet = "Xifeng")
ca = snail[, 1:2] %>% drop_na()
wh = snail[, 3:4] %>% drop_na()
iron = read_xlsx("data/regional records/freeiron.xlsx", sheet = "Pianguan")
rbsr = read_xlsx("data/regional records/RbSr.xlsx")
ob.am = read.csv("out/ob.am.csv")

## plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
site = pal[factor(sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]

# png("figures/Fig3.regional_records.png", 4, 6, units = "in", res = 300)
pdf("figures/Fig3.regional_records.pdf", width = 4, height = 6)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 5), axes = FALSE,
     xlab = "", ylab = "")
yext = range(sw$d18sw.low, sw$d18sw.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
sw.rs = cbind(sw$age,
                4 + (sw$d18sw - min(tix)) / diff(range(tix)),
                4 + (sw$d18sw.low - min(tix)) / diff(range(tix)),
                4 + (sw$d18sw.high - min(tix)) / diff(range(tix)))
arrows(sw.rs[, 1], sw.rs[, 3], sw.rs[, 1], sw.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(sw.rs[, 1], sw.rs[, 2], col = "black", bg = site, pch = 21, cex = 1.2)
axis(2, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 2, line = 2.5, at = 4.5)
yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
               4.4 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

yext = range(smi$SMI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 0.2)
smi.rs = cbind(smi$age,
              3.4 + (smi$SMI - min(tix)) / diff(range(tix)))
lines(smi.rs[, 1], smi.rs[, 2], col = pal[1], pch = 21, cex = 1.2)
axis(4, 3.4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("SMI"), 4, line = 2.5, at = 3.9)
yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
                 3.4 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

yext = range(gdgt$BIT)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10 + 1)), by = 2) / 10
gdgt.rs = cbind(gdgt$age,
                2.5 + (gdgt$BIT - min(tix)) / diff(range(tix)))
lines(gdgt.rs[, 1], gdgt.rs[, 2], col = pal[2])
axis(2, 2.5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("BIT"), 2, line = 2.5, at = 2.9)

yext = range(iron$Fe)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10)), by = 1) /10
iron.rs = cbind(iron$age,
               1.6 + (iron$Fe - min(tix)) / diff(range(tix)))
lines(iron.rs[, 1], iron.rs[, 2], col = pal[3], pch = 21, cex = 1.2)
axis(4, 1.6 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Fe"[2]*"O"[3]*"(f)/Fe"[2]*"O"[3]*"(t)"), 4, line = 2.5, at = 2)
yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
                 1.6 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

yext = range(ca$CA, wh$WH)
tix = seq(floor(min(yext)), 
          ceiling(max(yext) + 20), by = 20)
ca.rs = cbind(ca$age1,
              1 + (ca$CA - min(tix)) / diff(range(tix)))
wh.rs = cbind(wh$age2,
              1 + (wh$WH - min(tix)) / diff(range(tix)))
lines(ca.rs[, 1], ca.rs[, 2], col = pal[1], pch = 21, cex = 1.2)
lines(wh.rs[, 1], wh.rs[, 2], col = pal[5], pch = 21, cex = 1.2)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CA/WH (%)"), 2, line = 2.5, at = 1.5)

yext = range(rbsr$RbSr)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10)), by = 5) / 10
rbsr.rs = cbind(rbsr$age,
                -0.2 + (rbsr$RbSr - min(tix)) / diff(range(tix)))
lines(rbsr.rs[, 1], rbsr.rs[, 2], col = pal[2], pch = 21, cex = 1.2)
axis(4, -0.2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("Rb/Sr"), 4, line = 2.5, at = 0.4)
yext = range(ob.am$ob.am)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+3)), by = 5)/10
ob.am.rs = cbind(ob.am$age,
                 0 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "tomato", lwd = 2)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()
