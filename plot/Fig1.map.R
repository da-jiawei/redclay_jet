rm(list = ls())
# load library
libs = c("elevatr", "terra", "sf", "tidyterra", "tidyverse", "readxl", "scales", "ggspatial")
invisible(lapply(libs, library, character.only = T))

# coordinate system
crsLONGLAT = "EPSG:4326"
crs_xian = "+proj=longlat +ellps=IAU76 +no_defs +type=crs"

# load data
site = read_xlsx("data/sites_info.xlsx")
sampling_site = site |> 
  filter(work == "this study")
sampling_site$location = factor(sampling_site$location, levels = c("Lantian", "Shilou", "Jiaxian"))
GNIP = site |>
  filter(work == "GNIP")
literature = site |>
  filter(work == "ref")

# load shapefile
clp_sf = read_sf("data/sf/LoessPlateauRegion/LoessPlateauRegion.shp")

# load dem
xmin = 80
xmax = 140
ymin = 10
ymax = 50
bbox = st_polygon(list(rbind(
  c(xmin, ymin),
  c(xmin, ymax),
  c(xmax, ymax),
  c(xmax, ymin),
  c(xmin, ymin)
))) |>
  st_sfc(crs = crsLONGLAT) |>
  st_as_sf()
dem = elevatr::get_elev_raster(
  locations = bbox,
  z = 5,
  clip = "bbox"
) |>
  terra::rast()
tidyterra::autoplot(dem)

xmin = 100
xmax = 115
ymin = 33
ymax = 42
clp_bbox = bbox = st_polygon(list(rbind(
  c(xmin, ymin),
  c(xmin, ymax),
  c(xmax, ymax),
  c(xmax, ymin),
  c(xmin, ymin)
))) |>
  st_sfc(crs = crsLONGLAT) |>
  st_as_sf()
clp_dem = elevatr::get_elev_raster(
 locations = clp_bbox,
 z = 7,
 clip = "bbox"
) |>
  terra::rast()
tidyterra::autoplot(clp_dem)

# Map of East Asia and sampling locations
elev_range = as.numeric(minmax(dem))
elev_limits = c(floor(elev_range[1] / 500), ceiling(elev_range[2] / 500)) * 500

ggplot() +
  geom_spatraster(data = dem,
                  maxcell = Inf) +
  scale_fill_hypso_tint_c(palette = "wiki-2.0",
                          limits = elev_limits,
                          alpha = 1) +
  geom_sf(data = clp_bbox, fill = "transparent", color = "black") +
  geom_point(data = sampling_site, aes(x = longitude, y = latitude, color = location),
             shape = 21, size = 3, fill = "white") +
  geom_point(data = GNIP, aes(x = longitude, y = latitude, color = location),
             shape = 22, size = 3, fill = "white") +
  scale_color_brewer(palette = "Paired") +
  coord_sf(crs = crsLONGLAT,
           expand = FALSE) +
  labs(x = "", y = "", title = "", subtitle = "", caption = "") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())
ggsave(filename = "figures/EastAsia_map.pdf", width = 5.4, height = 5, bg = "white")

## plot CLP with shape file ----
# shade 
slope = terra::terrain(clp_dem, "slope", unit = "radians")
aspect = terra::terrain(clp_dem, "aspect", unit = "radians")
hill = terra::shade(slope, aspect, angle = 30, direction = 270)
names(hill) = "shades"
pal_greys = hcl.colors(1e3, palette = "Grays")
ggplot() +
  geom_spatraster(data = hill, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA)

index = hill |>
  mutate(index.col = rescale(shades, to = c(1, length(pal_greys)))) |>
  mutate(index.col = round(index.col)) |>
  pull(index.col)
pal_cols = pal_greys[index]
hill_shade = ggplot() +
  geom_spatraster(data = hill,
                  fill = pal_cols,
                  maxcell = Inf) +
  coord_sf(crs = crsLONGLAT)
hill_shade
# river
rivers = read_sf("data/sf/HydroRIVERS_v10_as_shp/HydroRIVERS_v10_as.shp") |>
  select(ORD_FLOW, geometry) |>
  st_transform(crs = crsLONGLAT)
clp_rivers = st_intersection(
  rivers,
  clp_bbox
)
sort(unique(clp_rivers$ORD_FLOW))
clp_rivers_width = clp_rivers |>
  mutate(width = as.numeric(ORD_FLOW))|>
  mutate(width = case_when(
    width == 3 ~ 0.5,
    width == 4 ~ 0.3,
    # width == 5 ~ 0.1,
    TRUE ~ 0
  )) |>
  filter(width > 0)
ggplot() +
  geom_sf(data = clp_rivers,
          linewidth = clp_rivers$width)

elev_range = as.numeric(minmax(clp_dem))
elev_limits = c(floor(elev_range[1] / 500), ceiling(elev_range[2] / 500)) * 500
elev_limits = pmax(elev_limits, 0)

hill_shade +
  geom_spatraster(data = clp_dem,
                  maxcell = Inf) +
  scale_fill_hypso_tint_c(palette = "dem_poster",
                          limits = elev_limits,
                          alpha = 0.6,
                          labels = label_comma()) +
  geom_sf(data = clp_rivers_width, 
          linewidth = clp_rivers_width$width,
          color = "lightblue1") +
  geom_point(data = literature, 
             aes(x = longitude, y = latitude),
             fill = "white",
             shape = 23, 
             stroke = 1, 
             size = 4) +
  geom_point(data = sampling_site, 
             aes(x = longitude, y = latitude, color = location), 
             fill = "white",
             shape = 21, 
             stroke = 1, 
             size = 4) +
  scale_color_brewer(palette = "Paired") +
  geom_text(data = literature, aes(x = longitude, y = latitude, label = location), nudge_x = -1.3, nudge_y = 0.4) +
  geom_text(data = sampling_site, aes(x = longitude, y = latitude, label = location), nudge_x = -1.3, nudge_y = 0.4) +
  guides(fill = guide_colorbar(
    title = "height (m)",
    direction = "vertical",
    label.position = "right",
    title.position = "top",
    barwidth = unit(3, "mm"),
    barheight = unit(30, "mm"),
    override.aes = list(alpha = 1)),
    color = "none") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        # plot.margin = margin(0,0,0,0, unit = "mm"),
        # legend.position = "none",
        legend.ticks = element_line(color = "black"),
        axis.text = element_text(size = 10)) +
  coord_sf(expand = FALSE,
           crs = crs_xian) +
  labs(x = "", y = "", color = "") + 
  annotation_scale(location = "tl", 
                   style = "ticks", 
                   width_hint = 0.2,
                   pad_y = unit(0.1, "npc"))
ggsave(filename = "figures/CLP_map.pdf", width = 5.4, height = 5.1, bg = "white")

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