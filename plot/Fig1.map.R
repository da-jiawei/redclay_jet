# load library
libs = c("elevatr", "terra", "tidyverse","sf", "giscoR", "osmdata", "marmap", "countrycode", "readxl", "ggspatial")
invisible(lapply(libs, library, character.only = T))

# coordinate system
crsLONGLAT = "+proj=longlat +datum=WGS84 +no_defs"

# load data
clp = read_sf("data/sf/LoessPlateauRegion/LoessPlateauRegion.shp") |>
  sf::st_transform(crs = crsLONGLAT)
site = read_xlsx("data/sites_info.xlsx")
sampling_site = site %>% 
  filter(work == "this study")
sampling_site$location =  factor(sampling_site$location, levels = c("Lantian", "Shilou", "Jiaxian"))
GNIP = site %>% 
  filter(work == "GNIP")
literature = site %>% filter(work == "ref")

## plot region ----
# get bbox elevation data
xmin = 90
xmax = 130
ymin = 20
ymax = 50
loc_df = data.frame(x = runif(100, xmin, xmax),
                     y = runif(100, ymin, ymax))
region_elevation = elevatr::get_elev_raster(locations = loc_df, prj = crsLONGLAT, z = 6, clip = "locations") |>
  terra::rast()
# terra::plot(region_elevation)


# plot region
region_elevation_df = region_elevation |>
  as.data.frame(xy = T) |>
  na.omit()
names(region_elevation_df)[3] = "elevation"

# Map of East Asia and sampling locations
eastasia = ggplot(data = region_elevation_df) +
  geom_raster(aes(x = x, y = y, fill = elevation), alpha = 1) +
  geom_sf(data = clp, fill = "transparent", color = "tomato") +
  geom_point(data = sampling_site, aes(x = longitude, y = latitude, color = location),
             shape = 21, size = 3, fill = "white") +
  geom_point(data = GNIP, aes(x = longitude, y = latitude, color = location),
             shape = 22, size = 3, fill = "white") +
  scale_color_brewer(palette = "Paired") +
  marmap::scale_fill_etopo() +
  coord_sf(crs = crsLONGLAT) +
  guides(fill = guide_colorbar(direction = "vertical", barheight = unit(30, "mm"), barwidth = unit(3, "mm"),
                               # title.position = "top", label.position = "right",
                               title.hjust = .5, label.hjust = .5,
                               ncol = 1, byrow = FALSE)) +
  labs(x = "", y = "", title = "", subtitle = "", caption = "") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "cm"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())
eastasia
ggsave(filename = "figures/EastAsia_map.pdf", width = 5.4, height = 5, eastasia, bg = "white")

## plot CLP with shape file ----
loess = st_read("data/sf/loess.shp")
desert = st_read("data/sf/Desert.shp", crs = crsLONGLAT)
river = st_read("data/sf/hyd1_4l.shp", crs = crsLONGLAT)
# mount = st_read("sf/Mountains.shp", crs = crsLONGLAT)

xmin = 100
xmax = 115
ymin = 33
ymax = 42

loess2 = st_intersection(loess, clp)
desert2 = st_intersection(desert, clp)
river2 = st_intersection(river, clp)
# mount2 = st_intersection(mount, clp)

ggplot() +
  geom_sf(data = clp, fill = "transparent", color = "black") +
  geom_sf(data = desert2, fill = "lightyellow2", color = "transparent") +
  geom_sf(data = loess2, fill = "navajowhite1", color = "transparent") +
  geom_sf(data = river2, color = "blue") +
  geom_point(data = literature, aes(x = longitude, y = latitude), shape = 23, stroke = 1, size = 4) +
  geom_point(data = sampling_site, aes(x = longitude, y = latitude, fill = location), shape = 21, stroke = 1, size = 4) +
  # geom_text(data = site, aes(x = longitude, y = latitude, label = location), vjust = -1, hjust = -0.3) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10),
        panel.background = element_blank(),
        legend.position = "none") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(x = "", y = "", fill = "") + 
  annotation_scale(location = "bl", style = "ticks", width_hint = 0.2)
ggsave(filename = "figures/CLP_map.pdf", width = 5.4, height = 5.1, bg = "white")

# plot monthly rainfall d18O ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
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