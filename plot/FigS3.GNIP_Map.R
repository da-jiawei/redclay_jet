rm(list = ls())
# load library
libs = c("rnaturalearth", "terra", "sf", "tidyterra", "tidyverse", "readxl", "scales", "ggspatial")
invisible(lapply(libs, library, character.only = T))

# GNIP map ----
# coordinate system
default_crs = "EPSG:4326"

# load data
site = read_xlsx("data/sites_info.xlsx")
GNIP = site |> filter(work == "GNIP")

# load shapefile
clp_sf = read_sf("/Users/dajiawei/Documents/work/geo_mapping/geo_mapping/resources/LoessPlateauRegion/LoessPlateauRegion.shp")
clp_sf = st_transform(clp_sf, crs = default_crs)
plot(st_geometry(clp_sf))

# load world map
map = terra::rast("/Users/dajiawei/Documents/work/geo_mapping/geo_mapping/resources/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")
plot(map)

bbox_rect_lonlat = c(xmin = 90, xmax = 150, ymin = 10, ymax = 60)
bbox_rect = st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs))
GNIP_map = terra::crop(
  map, bbox_rect, mask = TRUE
)
plot(GNIP_map)

# ggplot
GNIP_map_df = as.data.frame(
  GNIP_map, xy = TRUE, na.rm = TRUE
)

GNIP_map_df$fill_col = rgb(
  red = GNIP_map_df$HYP_HR_SR_OB_DR_1,
  green = GNIP_map_df$HYP_HR_SR_OB_DR_2,
  blue = GNIP_map_df$HYP_HR_SR_OB_DR_3,
  maxColorValue = 255
)

GNIP_site_sf = st_as_sf(GNIP, coords = c("longitude", "latitude"), crs = default_crs)
ggplot() +
  geom_raster(data = GNIP_map_df,
              aes(x = x, y = y, fill = fill_col)) +
  scale_fill_identity() +
  geom_sf(data = clp_sf, fill = "transparent", color = "yellow", linewidth = 1) +
  geom_sf(data = GNIP_site_sf, aes(color = location), 
          fill = "white", shape = 21, stroke = 1, size = 4) +
  # geom_sf_text(data = GNIP_site_sf, aes(label = location), size = 3, 
  #              nudge_x = .1, nudge_y = .2) +
  scale_color_brewer(palette = "Paired") +
  coord_sf(expand = FALSE) +
  labs(x = "", y = "", title = "", subtitle = "", caption = "") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    plot.margin = margin(0,0,0,0, unit = "mm"),
    legend.position = "none"
  ) +
  ggspatial::annotation_north_arrow(location = "tl",
                                    which_north = TRUE,
                                    style = north_arrow_orienteering,
                                    height = unit(1, "cm"),
                                    width = unit(1, "cm"),
                                    pad_x = unit(.5, "cm"), pad_y = unit(.5, "cm")) +
  ggspatial::annotation_scale(location = "br",
                              style = "ticks",
                              width_hint = .25)

ggsave("figures/GNIP_map.png", width = 5.2, height = 5.7, dpi = 500)

# plot monthly rainfall d18O ----
# pal = c("#F58D8D", "#E33438", "#FBB167", "#2D9551")
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99")
dat = read_xlsx("data/regional_records//GNIP.xlsx") |>
  mutate(d18.low = d18 - d18.sd,
         d18.high = d18 + d18.sd)
png("figures/GNIP_d18Op.png", 4, 5.5, units = "in", res = 500)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(1, 12), ylim = c(0, 4), axes = FALSE,
     xlab = "", ylab = "")
sjz = dat %>% filter(site == "Shijiazhuang")
yext = range(sjz$d18.low, sjz$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
sjz.rs = cbind(sjz$month,
               3 + (sjz$d18 - min(tix)) / diff(range(tix)),
               3 + (sjz$d18.low - min(tix)) / diff(range(tix)),
               3 + (sjz$d18.high - min(tix)) / diff(range(tix)))
arrows(sjz.rs[, 1], sjz.rs[, 3], sjz.rs[, 1], sjz.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(sjz.rs[, 1], sjz.rs[, 2], col = "black")
points(sjz.rs[, 1], sjz.rs[, 2], col = "black", bg = pal[2], pch = 21, cex = 1.2)
axis(2, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 2, line = 2.5, at = 3.5)

xa = dat %>% filter(site == "Xi'an")
yext = range(xa$d18.low, xa$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
xa.rs = cbind(xa$month,
              2 + (xa$d18 - min(tix)) / diff(range(tix)),
              2 + (xa$d18.low - min(tix)) / diff(range(tix)),
              2 + (xa$d18.high - min(tix)) / diff(range(tix)))
arrows(xa.rs[, 1], xa.rs[, 3], xa.rs[, 1], xa.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(xa.rs[, 1], xa.rs[, 2], col = "black")
points(xa.rs[, 1], xa.rs[, 2], col = "black", bg = pal[3], pch = 21, cex = 1.2)
axis(4, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 4, line = 2.5, at = 2.5)

zy = dat %>% filter(site == "Zunyi")
yext = range(zy$d18.low, zy$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
zy.rs = cbind(zy$month,
              1 + (zy$d18 - min(tix)) / diff(range(tix)),
              1 + (zy$d18.low - min(tix)) / diff(range(tix)),
              1 + (zy$d18.high - min(tix)) / diff(range(tix)))
arrows(zy.rs[, 1], zy.rs[, 3], zy.rs[, 1], zy.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(zy.rs[, 1], zy.rs[, 2], col = "black")
points(zy.rs[, 1], zy.rs[, 2], col = "black", bg = pal[4], pch = 21, cex = 1.2)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 2, line = 2.5, at = 1.5)

gl = dat %>% filter(site == "Guilin")
yext = range(gl$d18.low, gl$d18.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
gl.rs = cbind(gl$month,
              0 + (gl$d18 - min(tix)) / diff(range(tix)),
              0 + (gl$d18.low - min(tix)) / diff(range(tix)),
              0 + (gl$d18.high - min(tix)) / diff(range(tix)))
arrows(gl.rs[, 1], gl.rs[, 3], gl.rs[, 1], gl.rs[, 4], col = "black",
       angle=90, length=0, code = 0)
lines(gl.rs[, 1], gl.rs[, 2], col = "black")
points(gl.rs[, 1], gl.rs[, 2], col = "black", bg = pal[1], pch = 21, cex = 1.2)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 4, line = 2.5, at = 0.5)

axis(1, 1:12)
mtext("Month", 1, line = 2)

text(2, .1, "Guilin", cex = 1, col = "black", font = 2)
text(2, 1.2, "Zunyi", cex = 1, col = "black", font = 2)
text(11, 2.9, "Xi'an", cex = 1, col = "black", font = 2)
text(5, 3.2, "Shijiazhuang", cex = 1, col = "black", font = 2)

dev.off()
