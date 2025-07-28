rm(list = ls())
pacman::p_load(rnaturalearth, sf, tidyverse, readxl)

# Prepare Data ----
odp_location = read_xlsx("data/ODP_locations.xlsx")
colnames(odp_location) = odp_location[1,]
odp_selected = odp_location |>
  filter(Site %in% c("907", "982", "883/884", "850", "U1338", "846", "1241", "722")) |>
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude))

odp_sf = st_as_sf(odp_selected,
                  coords = c("Longitude", "Latitude"),
                  crs = "EPSG:4326")

world_map = rnaturalearth::ne_download(
  scale = 'small',
  type = 'coastline',
  category = 'physical',
  returnclass = 'sf'
)
plot(st_geometry(world_map))

ggplot() +
  geom_sf(data = world_map, color = "grey80") +
  geom_sf(data = odp_sf, shape = 21, size = 5, stroke = 1, 
          color = "tomato", fill = "white") +
  scale_y_continuous(limits = c(-20, 90)) +
  scale_x_continuous(limits = c(-150, 170)) +
  annotate("text", -130, -12, label = "U1338", size = 4) +
  annotate("text", -111, 10, label = "850", size = 4) +
  annotate("text", -70, 6, label = "1241", size = 4) +
  annotate("text", -91, -12, label = "846", size = 4) +
  annotate("text", 1, 69, label = "903", size = 4) +
  annotate("text", -30, 58, label = "982", size = 4) +
  annotate("text", 70, 10, label = "722", size = 4) +
  annotate("text", 168, 41, label = "883/884", size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 1),
        axis.title = element_blank(),
        axis.text = element_text(size = 15, color = "black"),
        axis.ticks.length = unit(.3, "cm"))
ggsave("figures/ODP_map.png", width = 6.8, height = 3.2, dpi = 300)
