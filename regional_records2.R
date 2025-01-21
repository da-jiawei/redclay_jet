library(tidyverse)
library(readxl)
library(ggpubr)

D47 = read.csv("out/D47.csv")
D47= D47[order(D47$age),] 
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

pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
site1 = pal[factor(D47$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
site2 = pal[factor(sw$site, levels = c("Lantian", "Shilou", "Jiaxian"))]

## plot ----
png("figures/regional_records.png", 4, 6, units = "in", res = 300)
# pdf("figures/Fig3.regional_records.pdf", width = 4, height = 6)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 5), axes = FALSE,
     xlab = "", ylab = "")

yext = range(D47$st.low, D47$st.high)
tix = seq(floor(min(yext + 1)), 
          ceiling(max(yext)), by = 10)
D47.rs = cbind(D47$age,
              4 + (D47$temp - min(tix)) / diff(range(tix)),
              4 + (D47$st.low - min(tix)) / diff(range(tix)),
              4 + (D47$st.high - min(tix)) / diff(range(tix)))
arrows(D47.rs[, 1], D47.rs[, 3], D47.rs[, 1], D47.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(D47.rs[, 1], D47.rs[, 2], col = "black", bg = site1, pch = 21, cex = 1.2)
loess_fit = loess(data = D47, temp ~ age, span = 0.2)
pred = predict(loess_fit, se = TRUE)
lines(lowess(D47.rs[, 1], D47.rs[, 2], f = 0.3), lwd = 3)
axis(2, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("T"[Delta*"47"]*" (", degree, "C)")), 2, line = 2.5, at = 4.5)

yext = range(sw$d18sw.low, sw$d18sw.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
sw.rs = cbind(sw$age,
              3 + (sw$d18sw - min(tix)) / diff(range(tix)),
              3 + (sw$d18sw.low - min(tix)) / diff(range(tix)),
              3 + (sw$d18sw.high - min(tix)) / diff(range(tix)))
arrows(sw.rs[, 1], sw.rs[, 3], sw.rs[, 1], sw.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(sw.rs[, 1], sw.rs[, 2], col = "black", bg = site2, pch = 21, cex = 1.2)
axis(4, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 4, line = 2.5, at = 3.5)

yext = range(smi$SMI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 0.2)
smi.rs = cbind(smi$age,
               2 + (smi$SMI - min(tix)) / diff(range(tix)))
lines(smi.rs[, 1], smi.rs[, 2], col = pal[1], pch = 21, cex = 1.2)
axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("SMI"), 2, line = 2.5, at = 2.5)

yext = range(gdgt$BIT)
tix = seq(floor(min(yext * 10)), 
          ceiling(max(yext * 10 + 1)), by = 2) / 10
gdgt.rs = cbind(gdgt$age,
                1 + (gdgt$BIT - min(tix)) / diff(range(tix)))
lines(gdgt.rs[, 1], gdgt.rs[, 2], col = pal[2])
axis(4, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("BIT"), 4, line = 2.5, at = 1.5)

yext = range(ca$CA, wh$WH)
tix = seq(floor(min(yext)), 
          ceiling(max(yext) + 20), by = 20)
ca.rs = cbind(ca$age1,
              0 + (ca$CA - min(tix)) / diff(range(tix)))
wh.rs = cbind(wh$age2,
              0 + (wh$WH - min(tix)) / diff(range(tix)))
lines(ca.rs[, 1], ca.rs[, 2], col = pal[1], pch = 21, cex = 1.2)
lines(wh.rs[, 1], wh.rs[, 2], col = pal[5], pch = 21, cex = 1.2)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("snail abundance (%)"), 2, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()
