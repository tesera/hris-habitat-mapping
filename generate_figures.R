library(tidyverse)
library(sf)
library(cowplot)

plot_width <- 7
plot_height <- 5

# 1. Load dataset
# NOTE: May need to download the data to the repository first
hris_geom <- st_read("hris-sierra_nevada_CA.gpkg")

# 2. Canopy cover percentage
canopy_gg <- hris_geom |> 
    # slice_sample(prop = 0.1) |> # For testing
    ggplot() +
    geom_sf(aes(fill = pred_cncvr_pct),
            colour = NA) + 
    theme_map() +
    theme(legend.position = "left") +
    scale_fill_viridis_c(name = "Canopy Cover",
                         direction = -1,
                         option = "plasma")

ggsave("canopy_cover.jpeg",
       canopy_gg,
       width = plot_width,
       height = plot_height,
       units = "in")
ggsave("canopy_cover.pdf",
       canopy_gg,
       width = plot_width,
       height = plot_height,
       units = "in")

# 3. Basal area covered by snags
snag_gg <- hris_geom |> 
    # slice_sample(prop = 0.1) |> # For testing
    ggplot() +
    geom_sf(aes(fill = pred_bapa_snag),
            colour = NA) + 
    theme_map() +
    theme(legend.position = "left") +
    scale_fill_viridis_c(name = "Snag\nBasal Area",
                         direction = -1,
                         option = "mako")

ggsave("snag_area.jpeg",
       snag_gg,
       width = plot_width,
       height = plot_height,
       units = "in")
ggsave("snag_area.pdf",
       snag_gg,
       width = plot_width,
       height = plot_height,
       units = "in")

# 4. Habitat mapping----
# Want to flag areas where:
#  a. pred_tpa > 9
#  b. pred_dia > 25
#  c. pred_cncvr_pct > 60
hris_geom <- hris_geom |> 
    mutate(habitat = if_else(
        pred_tpa > 9 &
            pred_dia > 25 &
            pred_cncvr_pct > 60,
        "Likely", "Unlikely"
    ))

count(as_tibble(hris_geom), habitat)

habitat_gg <- hris_geom |> 
    # slice_sample(prop = 0.1) |> # For testing
    ggplot() +
    geom_sf(aes(fill = habitat),
            colour = NA) + 
    theme_map() +
    theme(legend.position = "top") +
    scale_fill_viridis_d(name = "Habitat",
                         na.translate = FALSE)

ggsave("habitat_map.jpeg",
       habitat_gg,
       width = plot_width,
       height = plot_height,
       units = "in")
ggsave("habitat_map.pdf",
       habitat_gg,
       width = plot_width,
       height = plot_height,
       units = "in")