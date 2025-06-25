rm(list = ls())
pacman::p_load(rnaturalearth, sf, tidyverse, readxl, patchwork)
default_crs = "EPSG:4326"
# pacific_crs = "EPSG:3857"

# Prepare Data ---
clp_bbox = c(xmin = 105, xmax = 114.6, ymin = 33.7, ymax = 41)
clp_bbox_sf = st_as_sf(st_as_sfc(st_bbox(clp_bbox, crs = default_crs)))
bbox_rect_lonlat = c(xmin = 100, xmax = 170, ymin = 10 , ymax = 50)
bbox_pacific_rect = st_as_sfc(st_bbox(bbox_rect_lonlat, crs = default_crs))

land = rnaturalearth::ne_download(scale = "medium", 
                                  type = "coastline",
                                  category = "physical",
                                  returnclass = "sf")
plot(st_geometry(land))
# land_proj = st_transform(land, crs = pacific_crs)
# plot(st_geometry(land_proj))
# bbox_proj = st_transform(bbox_pacific_rect, crs = pacific_crs)
# plot(st_geometry(bbox_proj))

pacific = st_intersection(land, bbox_pacific_rect) 
plot(st_geometry(pacific))

site = read_xlsx("data/sites_info.xlsx")
d13_sites = site |> filter(pacific == TRUE & location != "ODP 885/886")

# bbox_rect = st_bbox(bbox_proj)
d13_sf = st_as_sf(d13_sites, coords = c("longitude", "latitude"), crs = default_crs)
# d13_sf_proj = st_transform(d13_sf, crs = pacific_crs)

# Pacific MAP ----
# ~5.0 Ma
p1 = ggplot() +
  geom_sf(data = pacific, color = "grey80", linewidth = .5) +
  geom_sf(data = clp_bbox_sf, fill = "transparent", color = "yellow", linewidth = 1) +
  geom_segment(aes(x = 105, y = 45, xend = 160, yend = 45),
               arrow = arrow(length = unit(.5, "cm")),
               linewidth = 1, color = "royalblue") +
  geom_segment(aes(x = 105, y = 40, xend = 160, yend = 40),
               arrow = arrow(length = unit(.5, "cm")),
               linewidth = 1, linetype = "dashed", color = "royalblue") +
  geom_curve(aes(x = 100, y = 15, xend = 105, yend = 40),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = .3, size = 1, color = "orange") +
  geom_curve(aes(x = 115, y = 15, xend = 111, yend = 41),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = .1, size = 1, color = "orange") +
  geom_curve(aes(x = 140, y = 15, xend = 115, yend = 42),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = -.3, size = 1, color = "orange") +
  geom_sf(data = d13_sf, shape = 21, size = 3, fill = "white", stroke = 1) +
  geom_sf_text(data = d13_sf, aes(label = location), size = 3, 
               nudge_x = 3, nudge_y = -2) +
  annotate("text", 160, 15, label = "a", size = 5, fontface = "bold") +
  ggtitle("ca. 5.0 Ma") +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(100, 160, 20)) +
  scale_y_continuous(breaks = seq(20, 50, 10)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.1, vjust = -10, size = 15),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 10, color = "black"))

# ca. 2.8 Ma
p2 = ggplot() +
  geom_sf(data = pacific, color = "grey80", linewidth = .5) +
  geom_sf(data = clp_bbox_sf, fill = "transparent", color = "yellow", linewidth = 1) +
  geom_segment(aes(x = 105, y = 40, xend = 160, yend = 40),
               arrow = arrow(length = unit(.5, "cm")),
               linewidth = 1, color = "royalblue") +
  geom_segment(aes(x = 105, y = 36.5, xend = 155, yend = 36.5),
               arrow = arrow(length = unit(.5, "cm")),
               linewidth = 1, linetype = "dashed", color = "royalblue") +
  geom_curve(aes(x = 100, y = 15, xend = 105, yend = 35),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = .3, size = 1, color = "orange") +
  geom_curve(aes(x = 115, y = 15, xend = 111, yend = 36),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = .1, size = 1, color = "orange") +
  geom_curve(aes(x = 140, y = 15, xend = 115, yend = 37),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = -.3, size = 1, color = "orange") +
  geom_sf(data = d13_sf, shape = 21, size = 3, fill = "white", stroke = 1) +
  geom_sf_text(data = d13_sf, aes(label = location), size = 3, 
               nudge_x = -3, nudge_y = 2) +
  annotate("text", 160, 15, label = "b", size = 5, fontface = "bold") +
  ggtitle("ca. 2.8 Ma") +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(100, 160, 20)) +
  scale_y_continuous(breaks = seq(20, 50, 10)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.1, vjust = -10, size = 15),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 10, color = "black"))

# ca. 1.0 Ma
p3 = ggplot() +
  geom_sf(data = pacific, color = "grey80", linewidth = .5) +
  geom_sf(data = clp_bbox_sf, fill = "transparent", color = "yellow", linewidth = 1) +
  geom_segment(aes(x = 105, y = 35, xend = 160, yend = 35),
               arrow = arrow(length = unit(.5, "cm")),
               linewidth = 1, color = "royalblue") +
  geom_segment(aes(x = 105, y = 31, xend = 155, yend = 31),
               arrow = arrow(length = unit(.5, "cm")),
               linewidth = 1, linetype = "dashed", color = "royalblue") +
  geom_curve(aes(x = 100, y = 15, xend = 105, yend = 30),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = .3, size = 1, color = "orange") +
  geom_curve(aes(x = 115, y = 15, xend = 111, yend = 31),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = .1, size = 1, color = "orange") +
  geom_curve(aes(x = 140, y = 15, xend = 115, yend = 32),
             arrow = arrow(length = unit(.3, "cm")),
             curvature = -.3, size = 1, color = "orange") +
  geom_sf(data = d13_sf, shape = 21, size = 3, fill = "white", stroke = 1) +
  geom_sf_text(data = d13_sf, aes(label = location), size = 3, 
               nudge_x = -3, nudge_y = 2) +
  annotate("text", 160, 15, label = "c", size = 5, fontface = "bold") +
  ggtitle("ca. 1.0 Ma") +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(100, 160, 20)) +
  scale_y_continuous(breaks = seq(20, 50, 10)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.1, vjust = -10, size = 15),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 10, color = "black"))

p1 + p2 + p3

ggsave(filename = "figures/Fig4_Pacific_map.png", width = 12, height = 3.5, bg = "white", dpi = 500)
