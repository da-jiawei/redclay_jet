rm(list = ls())
pacman::p_load(tidyverse, readxl)

#### load and groom data ----
this_study = read_csv("output/d18c.csv") 

# Jiaxian (occluded organic matter)
jiaxian_age = read_xlsx("data/regional_records/d13_org/jiaxian/Jiaxian_summary_MS.xlsx")
jiaxian_d13o = read_xlsx("data/regional_records/d13_org/jiaxian/Jiaxian_summary_om.xlsx")
jiaxian_d13o$age = approx(x = jiaxian_age$D_cm, y = jiaxian_age$T_Ma, xout = jiaxian_d13o$depth_cm_om)$y
names(jiaxian_d13o) = c("depth_cm", "d13o", "n", "d13o.se", "age")
jiaxian_d13o = jiaxian_d13o |>
  filter(age < 8) |>
  mutate(d13o.low = d13o - d13o.se,
         d13o.high = d13o + d13o.se)

# Lingtai black carbon (Zhou et al., 2015)
lingtai_bc = read_xlsx("data/regional_records/d13_org/lingtai_black_carbon_zhou2014.xlsx") |>
  filter(age > 2) |>
  drop_na(d13C)

# Northern China (Lu et al., 2020)
northern_china = read_xlsx("data/regional_records/d13_org/northern_china_lu2020.xlsx")
names(northern_china) = c("depth_m", "age", "d13o", "CaCO3", "TOC")
northern_china = northern_china |>
  filter(age > 2 & age < 8)
ggplot(northern_china, aes(x = age, y = d13o)) +
  geom_point()

# # Japan sea black carbon (Shen et al., 2018) 
# japan_bc = read_xlsx("data/regional_records/d13_org/japan_sea_black_carbon_shen2018.xlsx")
# japan_bc = japan_bc[3:356, 2:4]
# names(japan_bc) = c("age", "d13o", "bcc")
# japan_bc = japan_bc |>
#   mutate_all(as.numeric) |>
#   filter(age > 2.58 & age < 8)


#### Plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99")
png("figures/d13_organic_matter_time_series.png", 4.1, 5.5, units = "in", res = 500)
par(mar = c(4, 4, 3, 4))
plot(0, 0, xlim = c(2, 8), ylim = c(0, 4.2), axes = FALSE,
     xlab = "", ylab = "")
axis(3, cex = 1)
mtext("Age (Ma)", 3, line = 2)
# jiaxian
yext = range(jiaxian_d13o$d13o.low, jiaxian_d13o$d13o.high)
tix = seq(floor(min(yext)), ceiling(max(yext)), by = 1)
jiaxian_d13o.rs = cbind(jiaxian_d13o$age,
              3 + (jiaxian_d13o$d13o - min(tix)) / diff(range(tix)),
              3 + (jiaxian_d13o$d13o.low - min(tix)) / diff(range(tix)),
              3 + (jiaxian_d13o$d13o.high - min(tix)) / diff(range(tix)))
arrows(jiaxian_d13o.rs[, 1], jiaxian_d13o.rs[, 3], 
       jiaxian_d13o.rs[, 1], jiaxian_d13o.rs[, 4], 
       col = "grey80", angle=90, length=0, code = 0)
points(jiaxian_d13o.rs[, 1], jiaxian_d13o.rs[, 2], col = pal[1], bg = "white", pch = 21, cex = .8)
axis(2, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[OOM]*" (\u2030)"), 2, line = 2.5, at = 3.5)
# lingtai
yext = range(lingtai_bc$d13C)
tix = seq(floor(min(yext)), ceiling(max(yext)), by = 2)
lingtai.rs = cbind(lingtai_bc$age,
                   2 + (lingtai_bc$d13C - min(tix)) / diff(range(tix)))
lines(lingtai.rs[, 1], lingtai.rs[, 2], col = pal[2])
axis(4, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[bc]*" (\u2030)"), 4, line = 2.5, at = 2.5)
# Northern China
yext = range(northern_china$d13o)
tix = seq(floor(min(yext)), ceiling(max(yext)), by = 2)
northern_china.rs = cbind(northern_china$age,
                   1 + (northern_china$d13o - min(tix)) / diff(range(tix)))
lines(northern_china.rs[, 1], northern_china.rs[, 2], col = pal[3])
points(northern_china.rs[, 1], northern_china.rs[, 2], col = pal[3], bg = "white", pch = 21, cex = .8)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[SOM]*" (\u2030)"), 2, line = 2.5, at = 1.5)
# Soil water d18O 
site = pal[factor(this_study$site, levels = c("Lantian", "Shilou", "Jiaxian"))]
yext = range(this_study$d18sw)
tix = seq(floor(min(yext)), ceiling(max(yext)), by = 1)
soil_water.rs = cbind(this_study$age,
                          0 + (this_study$d18sw - min(tix)) / diff(range(tix)))
points(soil_water.rs[, 1], soil_water.rs[, 2], col = site, bg = "white", pch = 21, cex = .8)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[sw]*" (\u2030)"), 4, line = 2.5, at = .5)

rect(xleft = 4.6, ybottom = 0, xright = 5.3, ytop = 4.2, border = NA, col = rgb(0, 0, 1, 0.1), lwd = 0, alpha = .5)
rect(xleft = 2, ybottom = 0, xright = 3, ytop = 4.2, border = NA, col = rgb(1, 0, 0, 0.1), lwd = 0, alpha = .5)
text(7.5, 4, "Jiaxian", cex = .8, col = "black", font = 1)
text(7.5, 3, "Lingtai", cex = .8, col = "black", font = 1)
text(7, 1.7, "Northern China", cex = .8, col = "black", font = 1)
text(2.2, 4, "a", cex = 1, col = "black", font = 2)
text(2.2, 3, "b", cex = 1, col = "black", font = 2)
text(2.2, 1.1, "c", cex = 1, col = "black", font = 2)
text(2.2, .2, "d", cex = 1, col = "black", font = 2)

axis(1, cex = 1)
mtext("Age (Ma)", 1, line = 2)
dev.off()
