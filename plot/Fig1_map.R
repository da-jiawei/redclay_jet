rm(list = ls())
pacman::p_load(rnaturalearth, terra, sf, tidyverse, tidyterra, readxl, scales, ggspatial, elevatr)
default_crs = "EPSG:4326"
xian_crs = "EPSG:4610"

# Prepare Data ----
# load base map
map = terra::rast("/Users/dajiawei/Documents/work/geo_mapping/geo_mapping/resources/Natural_Earth/HYP_50M_SR_W/HYP_50M_SR_W.tif")
bbox_rect_lonlat = c(xmin = 90, xmax = 150, ymin = 10, ymax = 50)
bbox_rect = st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs))
eastasia_map = terra::crop(map, bbox_rect)
plot(eastasia_map)

bbox_rect_lonlat = c(xmin = 116, xmax = 118.5, ymin = 38, ymax = 40.5)
bbox_rect = st_as_sf(st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs)))
G3_dem = elevatr::get_elev_raster(
  locations = bbox_rect,
  z = 8,
  clip = "bbox"
) |> rast()

bbox_wkt = st_as_text(st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs)))
rivers = st_read(
  "/Users/dajiawei/Documents/work/geo_mapping/geo_mapping/resources/Hydro_Rivers/HydroRIVERS_v10_as_shp/HydroRIVERS_v10_as_shp/HydroRIVERS_v10_as.shp",
  wkt_filter = bbox_wkt)
rivers = rivers |> 
  filter(ORD_FLOW < 6) |>
  st_cast("LINESTRING") |>
  st_intersection(bbox_rect)
plot(st_geometry(rivers))


# load data
site = read_xlsx("data/sites_info.xlsx")
sampling_site = site |> filter(work == "this study")
sampling_site$location = factor(sampling_site$location, levels = c("Lantian", "Shilou", "Jiaxian"))
GNIP = site |> filter(work == "GNIP")
d13_sites = site |> filter(pacific == TRUE) |>
  filter(location %in% c("G3", "U1430", "Jiaxian", "Lingtai"))

# load shapefile
clp_sf = read_sf("/Users/dajiawei/Documents/work/geo_mapping/geo_mapping/resources/LoessPlateauRegion/LoessPlateauRegion.shp") |>
  st_transform(crs = xian_crs)
st_bbox(clp_sf)
bbox_rect_lonlat = c(xmin = 105, xmax = 114.6, ymin = 33.7, ymax = 41)
clp_bbox = st_as_sf(st_as_sfc(st_bbox(bbox_rect_lonlat, crs = xian_crs)))

# East Asia Map ----
eastasia_map = eastasia_map |>
  project(xian_crs)
base_map_df = as.data.frame(eastasia_map,
                            xy = TRUE, na.rm = TRUE)
base_map_df$fill_col = rgb(
  red = base_map_df$HYP_50M_SR_W_1,
  green = base_map_df$HYP_50M_SR_W_2,
  blue = base_map_df$HYP_50M_SR_W_3,
  maxColorValue = 255
)

sample_sf = st_as_sf(sampling_site, 
                     coords = c("longitude", "latitude"),
                     crs = default_crs) |>
  st_transform(crs = xian_crs)
d13C_sf = st_as_sf(d13_sites,
                   coords = c("longitude", "latitude"),
                   crs = default_crs) |>
  st_transform(crs = xian_crs)
GNIP_sf = st_as_sf(GNIP,
                   coords = c("longitude", "latitude"),
                   crs = default_crs) |>
  st_transform(crs = xian_crs)

ggplot() +
  geom_raster(data = base_map_df,
              aes(x = x, y = y, fill = fill_col)) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_curve(aes(x = 118, y = 42, xend = 130, yend = 38),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = -.3, size = 1, color = "grey40") +
  geom_curve(aes(x = 120, y = 44, xend = 130, yend = 40),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = -.3, size = 1, color = "grey40") +
  geom_segment(aes(x = 95, y = 39, xend = 145, yend = 39),
               arrow = arrow(length = unit(.5, "cm")),
               size = 1, color = "royalblue") +
  geom_curve(aes(x = 95, y = 15, xend = 112, yend = 32),
             arrow = arrow(length = unit(.5, "cm")),
             curvature = .3, size = 1, color = "orange") +
  geom_curve(aes(x = 115, y = 15, xend = 114, yend = 33),
               arrow = arrow(length = unit(.5, "cm")),
             curvature = .1, size = 1, color = "orange") +
  geom_curve(aes(x = 140, y = 15, xend = 116, yend = 34),
             arrow = arrow(length = unit(.5, "cm")),
             curvature = -.3, size = 1, color = "orange") +
  geom_sf(data = clp_bbox, color = "yellow", fill = "transparent", linewidth = 1) +
  geom_sf(data = d13C_sf, shape = 22, fill = "white", size = 4, stroke = 1) +
  geom_sf(data = GNIP_sf, aes(fill = location), show.legend = FALSE,
          shape = 23, size = 4, color = "white", stroke = 1) +
  scale_fill_brewer(palette = "Set1") +
  annotate("text", 104, 33.5, label = "Lingtai") +
  annotate("text", 108, 40, label = "Jiaxian") +
  annotate("text", 120, 40, label = "G3") +
  annotate("text", 133, 40, label = "U1430") +
  coord_sf(expand = FALSE) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(5,5,5,5, unit = "mm"),
        legend.text = element_text(size = 10),
        legend.margin = margin(5,5,5,5),
        legend.title = element_blank(),
        axis.text = element_text(size = 10, color = "black")) +
  labs(x = "", y = "") +
  ggspatial::annotation_north_arrow(location = "tl",
                                    which_north = TRUE,
                                    style = north_arrow_orienteering,
                                    height = unit(1, "cm"),
                                    width = unit(1, "cm"),
                                    pad_x = unit(.5, "cm"), pad_y = unit(.5, "cm")) +
  ggspatial::annotation_scale(location = "br",
                              style = "ticks",
                              width_hint = .25)

ggsave("figures/Fig1.East_Asia_Map.png", width = 7, height = 5.8, dpi = 500)

# G3 drilling core ----
G3_df = as.data.frame(G3_dem, xy = TRUE)
colnames(G3_df)[3] = "elevation" 
range(G3_df$elevation)

rivers = rivers |>
  mutate(width = rivers$ORD_FLOW) |>
  mutate(width = case_when(
    width == 4 ~ 1,
    width == 5 ~ 0.8,
    width == 6 ~ 0.6
  )) |>
  select(width, geometry)
remove_box = st_as_sfc(st_bbox(c(xmin = 116, xmax = 117.1, ymin = 38, ymax = 39.2), crs = 4326))
rivers_filtered = rivers[!st_intersects(rivers, remove_box, sparse = FALSE), ]

ggplot() +
  geom_raster(data = G3_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_hypso_tint_c(palette = "dem_poster") +
  geom_sf(data = rivers_filtered, linewidth = rivers_filtered$width, color = "lightblue") +
  geom_point(aes(x = 117.4333, y = 38.83333), shape = 22, size = 4, stroke = 1,
             fill = "white") +
  annotate("label", 117.2, 38.6, label = "G3", size = 3, fontface = "bold", fill = "lightyellow") +
  annotate("label", 117.5, 39.3, label = "Hai He", size = 3, fontface = "bold", fill = "lightyellow") +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(116, 120, 1)) +
  scale_y_continuous(breaks = seq(38, 41, 1)) +
  guides(fill = "none") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 10, color = "black"))
ggsave("figures/Fig1.G3_Map.png", width = 2.3, height = 2.5, dpi = 300)

#### plot CLP ----
map = rast("/Users/dajiawei/Documents/work/geo_mapping/geo_mapping/resources/Natural_Earth/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")
bbox_rect_lonlat = c(xmin = 106, xmax = 115, ymin = 33, ymax = 41)
bbox_clp_rect = st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs))
east_clp_map = terra::crop(
  map, bbox_clp_rect
) |> project(xian_crs)
plot(east_clp_map)
east_clp_map_df = as.data.frame(east_clp_map, xy = TRUE)
east_clp_map_df$fill_col = rgb(
  red = east_clp_map_df$HYP_HR_SR_OB_DR_1,
  green = east_clp_map_df$HYP_HR_SR_OB_DR_2,
  blue = east_clp_map_df$HYP_HR_SR_OB_DR_3,
  maxColorValue = 255
)

clp_sites = site |> filter(CLP == TRUE & work == "ref")
clp_sites_df = st_as_sf(clp_sites, coords = c("longitude", "latitude"), crs = default_crs)
clp_sites_proj = st_transform(clp_sites_df, crs = xian_crs)
ggplot() +
  geom_raster(data = east_clp_map_df,
              aes(x = x, y = y, fill = fill_col)) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_sf(data = sample_sf, aes(fill = location), 
          color = "white", shape = 21, stroke = 1, size = 4) +
  scale_fill_brewer(palette = "Paired") +
  geom_sf(data = clp_sites_proj, shape = 22, fill = "white", size = 4) +
  geom_sf_text(data = sample_sf, aes(label = location), nudge_x = -.5, nudge_y = 0.4) +
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
ggsave(filename = "figures/CLP_map.png", width = 3.4, height = 3.7, bg = "white", dpi = 500)
