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
  geom_sf(data = clp_sf, fill = "transparent", color = "yellow", linewidth = 1) +
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
bbox_rect_lonlat = c(xmin = 105, xmax = 115, ymin = 33, ymax = 42)
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
  labs(x = "", y = "", color = "") +
  ggspatial::annotation_north_arrow(location = "tl",
                                    which_north = TRUE,
                                    style = north_arrow_orienteering,
                                    height = unit(1, "cm"),
                                    width = unit(1, "cm"),
                                    pad_x = unit(.5, "cm"), pad_y = unit(.5, "cm")) +
  ggspatial::annotation_scale(location = "br",
                              style = "ticks",
                              width_hint = .25)
  
ggsave(filename = "figures/CLP_map.png", width = 4.3, height = 4.4, bg = "white", dpi = 500)
