library(tidyverse)
library(readxl)
library(ggpubr)
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
sw = read.csv("out/d18c.csv")
# lr04 = read_xlsx("data/global records/LR04.xlsx") %>%
#   mutate(age = age / 1000) %>%
#   filter(age > 2.5 & age < 7.5)
orbital = read_xlsx("data/global records/Pliocene_orbital_and_insolation_data.xlsx")
age = seq(0, 7.5, 0.01)
obliquity = cos(2*pi*(1/1200)*age*1000)
ob.am2 = data.frame(age, obliquity)

## find the obliquity maxima ----
ob = orbital[, c("age", "obliquity")]
ob.max = ob[8,]
num = round((nrow(ob)-8)/41)
row = 8
for (i in 1: num) {
  row.min = row + 39
  row.max = row + 43
  subset = ob[row.min:row.max, ]
  index = which.max(subset$obliquity)
  ob.max[i+1,] = subset[index,]
  row = row.min + index - 1
}
m1 = loess(obliquity ~ age, data = ob.max, span = 0.2)
age = seq(2.51, 7.48, 0.01)
ob.am = predict(m1, age)
ob.am = data.frame(age, ob.am)
m2 = loess(ob.am ~ age, data = ob.am, span = 0.1)
ob.am$ob.am = predict(m2, age)
write_csv(ob.am, "out/ob.am.csv")
ggplot(ob.am, aes(x = age, y = ob.am)) +
  geom_line(data = ob, aes(x = age, y = obliquity), color = "grey") +
  geom_line()

## summer insolation gradient minima ----
sig = orbital[, c("age", "gradient")]
sig.min = sig[9,]
num = round((nrow(sig)-9)/41)
row = 9
for (i in 1: num) {
  row.min = row + 33
  row.max = row + 45
  subset = sig[row.min:row.max, ]
  index = which.min(subset$gradient)
  sig.min[i+1,] = subset[index,]
  row = row.min + index - 1
}
m1 = loess(gradient ~ age, data = sig.min, span = 0.2)
age = seq(2.51, 7.48, 0.01)
sig.am = predict(m1, age)
sig.am = data.frame(age, sig.am)
write_csv(sig.am, "out/sig.am.csv")
ggplot(sig.am, aes(x = age, y = sig.am)) +
  geom_line(data = sig, aes(x = age, y = gradient), color = "grey") +
  geom_smooth(se = FALSE, span = 0.1) + 
  scale_y_reverse()

# png("figures/obliquity.jpg", 5.5, 5, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(2, 7.5), ylim = c(0, 3), axes = FALSE,
     xlab = "", ylab = "")
yext = range(ob$obliquity)
tix = seq(floor(min(yext*10)), 
          ceiling(max(yext*10+1)), by = 5)/10
ob.rs = cbind(ob$age,
              2 + (ob$obliquity - min(tix)) / diff(range(tix)))
lines(ob.rs[, 1], ob.rs[, 2], col = "ivory3")
ob.am.rs = cbind(ob.am$age,
              2 + (ob.am$ob.am - min(tix)) / diff(range(tix)))
lines(ob.am.rs[, 1], ob.am.rs[, 2], col = "black", lwd = 2)
axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("obliquity"), 2, line = 2.5, at = 2.5)

yext = range(sig$gradient)
tix = seq(ceiling(max(yext)), 
          floor(min(yext-2.9)), by = -5)
sig.rs = cbind(sig$age,
                   2 - (sig$gradient - min(tix)) / diff(range(tix)))
lines(sig.rs[, 1], sig.rs[, 2], col = "ivory3")
sig.am.rs = cbind(sig.am$age,
               2 - (sig.am$sig.am - min(tix)) / diff(range(tix)))
lines(sig.am.rs[, 1], sig.am.rs[, 2], col = "black", lwd = 2)
axis(4, 2 - (tix - min(tix)) / diff(range(tix)), tix)
mtext("insolation gradient", 4, line = 2.5, at = 1.5)

yext = range(sw$d18sw.low, sw$d18sw.high)
tix = seq(floor(min(yext*10-3)), 
          ceiling(max(yext*10)), by = 20)/10
sw.rs = cbind(sw$age,
              0 + (sw$d18sw - min(tix)) / diff(range(tix)),
              0 + (sw$d18sw.low - min(tix)) / diff(range(tix)),
              0 + (sw$d18sw.high - min(tix)) / diff(range(tix)))
arrows(sw.rs[, 1], sw.rs[, 3], sw.rs[, 1], sw.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
points(sw.rs[, 1], sw.rs[, 2], col = "black", bg = "white", pch = 21, cex = 1.2)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 2, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
# dev.off()
