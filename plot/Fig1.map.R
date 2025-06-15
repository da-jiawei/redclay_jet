rm(list = ls())
# load library
libs = c("rnaturalearth", "terra", "sf", "tidyterra", "tidyverse", "readxl", "scales", "ggspatial")
invisible(lapply(libs, library, character.only = T))

# coordinate system
default_crs = "EPSG:4326"

# load natural earth map
map = terra::rast("/Users/geo-checkout-user/Documents/geo_mapping/geo_spatial/resources/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")

# load data
site = read_xlsx("data/sites_info.xlsx")
sampling_site = site |> filter(work == "this study")
sampling_site$location = factor(sampling_site$location, levels = c("Lantian", "Shilou", "Jiaxian"))
GNIP = site |> filter(work == "GNIP")
literature = site |> filter(pacific == TRUE)
clp_sites = site |> filter(CLP == TRUE & work != "this study")

# load shapefile
clp_sf = read_sf("/Users/geo-checkout-user/Documents/geo_mapping/geo_spatial/resources/LoessPlateauRegion/LoessPlateauRegion.shp")
clp_sf = st_transform(clp_sf, crs = default_crs)
plot(st_geometry(clp_sf))

# Pacific MAP ----
bbox_rect_lonlat = c(xmin = 100, xmax = 200, ymin = 25, ymax = 60)
bbox_pacific_rect = st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs))

land = ne_countries(scale = "medium", returnclass = "sf")
plot(st_geometry(land))
pacific_crs = "EPSG:10601"
land_proj = st_transform(land, crs = pacific_crs)
plot(st_geometry(land_proj))
bbox_proj = st_transform(bbox_pacific_rect, crs = pacific_crs)
plot(st_geometry(bbox_proj))

bbox_rect = st_bbox(bbox_proj)

clp_sf = st_transform(clp_sf, crs = pacific_crs)
literature_sf = st_as_sf(literature, coords = c("longitude", "latitude"), crs = default_crs)
literature_proj = st_transform(literature_sf, crs = pacific_crs)

ggplot() +
  geom_sf(data = land_proj, fill = "grey90", color = "grey", linewidth = 0.3) +
  geom_sf(data = clp_sf, fill = "transparent", color = "yellow") +
  geom_sf(data = literature_proj, shape = 21, size = 3, fill = "white") +
  geom_sf_text(data = literature_proj, aes(label = location), size = 3, 
               nudge_x = 3e5, nudge_y = -2e5) +
  coord_sf(
    crs = pacific_crs,
    xlim = c(bbox_rect["xmin"], bbox_rect["xmax"]),
    ylim = c(bbox_rect["ymin"], bbox_rect["ymax"]),
    expand = FALSE
  ) +
  labs(x = "", y = "", title = "", subtitle = "", caption = "") +
  scale_x_continuous(breaks = seq(-170, 180, 20)) +
  scale_y_continuous(breaks = seq(30, 60, 10)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    aspect.ratio = 0.5
  )
ggsave(filename = "figures/Pacific_map.png", width = 6, height = 4, bg = "white", dpi = 500)


#### plot CLP ----
xian_crs = "EPSG:4610"
bbox_rect_lonlat = c(xmin = 100, xmax = 115, ymin = 33, ymax = 42)
bbox_clp_rect = st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs))
east_clp_map = terra::crop(
  map, bbox_clp_rect
) |> project(xian_crs)
plot(east_clp_map)
east_clp_map_df = as.data.frame(
  east_clp_map,
  xy = TRUE,
  na.rm = TRUE
)
east_clp_map_df$fill_col = rgb(
  red = east_clp_map_df$HYP_HR_SR_OB_DR_1,
  green = east_clp_map_df$HYP_HR_SR_OB_DR_2,
  blue = east_clp_map_df$HYP_HR_SR_OB_DR_3,
  maxColorValue = 255
)

sampling_site_sf = st_as_sf(sampling_site, coords = c("longitude", "latitude"), crs = default_crs)
sampling_site_proj = st_transform(sampling_site_sf, crs = xian_crs)
clp_sites_df = st_as_sf(clp_sites, coords = c("longitude", "latitude"), crs = default_crs)
clp_sites_proj = st_transform(clp_sites_df, crs = xian_crs)
ggplot() +
  geom_raster(data = east_clp_map_df,
              aes(x = x, y = y, fill = fill_col)) +
  scale_fill_identity() +
  geom_sf(data = sampling_site_proj, aes(color = location), 
          fill = "white", shape = 21, stroke = 1, size = 4) +
  scale_color_brewer(palette = "Paired") +
  geom_sf(data = clp_sites_proj, shape = 22, fill = "white", size = 4) +
  geom_sf_text(data = sampling_site_proj, aes(label = location), nudge_x = -.5, nudge_y = 0.4) +
  geom_sf_text(data = clp_sites_proj, aes(label = location), nudge_x = -.5, nudge_y = 0.4) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "mm"),
        legend.position = "none",
        axis.text = element_text(size = 10, color = "black")) +
  coord_sf(expand = FALSE,
           crs = xian_crs) +
  labs(x = "", y = "", color = "")
ggsave(filename = "figures/CLP_map.png", width = 5.7, height = 4.3, bg = "white", dpi = 500)

# plot monthly rainfall d18O ----
pal = c("#F58D8D", "#E33438", "#FBB167", "#2D9551")
dat = read_xlsx("data/regional_records//GNIP.xlsx") %>%
  mutate(d18.low = d18 - d18.sd,
         d18.high = d18 + d18.sd)
png("figures/GNIP_d18Op.png", 4, 3.6, units = "in", res = 600)
# pdf("figures/GNIP_d18Op.pdf", width = 4, height = 3.6)
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
points(sjz.rs[, 1], sjz.rs[, 2], col = "black", bg = pal[1], pch = 21, cex = 1.2)
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
points(xa.rs[, 1], xa.rs[, 2], col = "black", bg = pal[2], pch = 21, cex = 1.2)
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
points(zy.rs[, 1], zy.rs[, 2], col = "black", bg = pal[3], pch = 21, cex = 1.2)
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
points(gl.rs[, 1], gl.rs[, 2], col = "black", bg = pal[4], pch = 21, cex = 1.2)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[p]*" (\u2030)"), 4, line = 2.5, at = 0.5)

axis(1, 1:12)
mtext("Month", 1, line = 2)
dev.off()
