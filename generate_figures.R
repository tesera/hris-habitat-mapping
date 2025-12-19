library(tidyverse)
library(sf)
library(units)
library(cowplot)
library(maptiles)
library(tidyterra)

PDF_FIGS <- FALSE

plot_width <- 7
plot_height <- 5

# 1. Load dataset
# NOTE: May need to download the data to the repository first
hris_geom <- st_read("hris-sierra_nevada_CA.gpkg")

# Add polygon area
# By default: m^2, but we convert to acres
hris_geom <- hris_geom |> 
    mutate(sptl_area = st_area(.data[["geom"]]),
           sptl_area = set_units(sptl_area, "acre"))

# Get tiles for the extent of your data
# These will be used as a basemap for our figures
bg_tiles <- get_tiles(hris_geom, provider = "Esri.WorldImagery",
                      zoom = 12)

# 2. Canopy cover percentage
canopy_gg <- hris_geom |> 
    # slice_sample(prop = 0.1) |> # For testing
    ggplot() +
    geom_spatraster_rgb(data = bg_tiles) +
    geom_sf(aes(fill = pred_cncvr_pct),
            colour = NA) + 
    theme_map() +
    theme(legend.position = "left") +
    scale_fill_viridis_c(name = "Canopy Cover",
                         direction = -1,
                         option = "plasma",
                         na.value = "transparent")

ggsave("canopy_cover.jpeg",
       canopy_gg,
       width = plot_width,
       height = plot_height,
       units = "in")

if (PDF_FIGS) {
    ggsave("canopy_cover.pdf",
           canopy_gg,
           width = plot_width,
           height = plot_height,
           units = "in")
}

# 3. Basal area covered by snags
snag_gg <- hris_geom |> 
    # slice_sample(prop = 0.1) |> # For testing
    ggplot() +
    geom_spatraster_rgb(data = bg_tiles) +
    geom_sf(aes(fill = pred_bapa_snag),
            colour = NA) + 
    theme_map() +
    theme(legend.position = "left") +
    scale_fill_viridis_c(name = "Snag\nBasal Area",
                         direction = -1,
                         option = "mako",
                         na.value = "transparent")

ggsave("snag_area.jpeg",
       snag_gg,
       width = plot_width,
       height = plot_height,
       units = "in")

if (PDF_FIGS) {
    ggsave("snag_area.pdf",
           snag_gg,
           width = plot_width,
           height = plot_height,
           units = "in")
}

# 4. Habitat mapping----
# California Spotted Owl
# Want to flag areas where:
#  a. pred_tpa > 9
#  b. pred_dia > 25
#  c. pred_cncvr_pct > 60
#  d. pred_bapa_softwood > 0.5 * pred_bapa
hris_geom <- hris_geom |> 
    mutate(habitat_cso = case_when(
        pred_tpa > 9 &
            pred_dia > 25 &
            pred_cncvr_pct > 60 & 
            pred_bapa_softwood > 0.5 * pred_bapa ~ "Nesting",
        pred_tpa > 9 &
            pred_dia > 25 &
            pred_cncvr_pct > 40 & 
            pred_bapa_softwood > 0.5 * pred_bapa ~ "Foraging",
        TRUE ~ "Unlikely"
    ))

# Number of polygons per habitat type
# along with total area
hris_geom |> 
    as_tibble() |> # For faster calculation
    summarise(num_polys = n(),
              total_area = sum(sptl_area),
              .by = "habitat_cso")

# Map of the habitat
cso_colors <- c(
    "Nesting" = "#1b7837", 
    "Foraging" = "#addd8e", 
    "Unlikely" = "#d9d9d9"
)

habitat_cso_gg <- hris_geom |> 
    # slice_sample(prop = 0.1) |> # For testing
    ggplot() +
    geom_spatraster_rgb(data = bg_tiles) +
    geom_sf(aes(fill = habitat_cso),
            colour = NA) + 
    theme_map() +
    theme(legend.position = "top") +
    scale_fill_manual(name = "Habitat",
                      values = cso_colors,
                      na.translate = FALSE,
                      na.value = "transparent")

ggsave("habitat_cso_map.jpeg",
       habitat_cso_gg,
       width = plot_width,
       height = plot_height,
       units = "in")

if (PDF_FIGS) {
    ggsave("habitat_cso_map.pdf",
           habitat_cso_gg,
           width = plot_width,
           height = plot_height,
           units = "in")
}

# Pacific Fisher
# Want to flag areas where:
#  a. pred_tpa > 9
#  b. pred_dia > 25
#  c. pred_cncvr_pct > 60
#  d. pred_bapa_softwood > 0.5 * pred_bapa
hris_geom <- hris_geom |> 
    mutate(habitat_pf = case_when(
        pred_dia > 25 &
            pred_cncvr_pct > 60 & 
            pred_bapa_softwood > 0.5 * pred_bapa ~ "Likely",
        TRUE ~ "Unlikely"
    ))

# Number of polygons per habitat type
# along with total area
hris_geom |> 
    as_tibble() |> # For faster calculation
    summarise(num_polys = n(),
              total_area = sum(sptl_area),
              .by = "habitat_pf")

# Map of the habitat
pf_colors <- c(
    "Likely" = "#005f73", 
    "Unlikely" = "#e9d8a6"
)

habitat_pf_gg <- hris_geom |> 
    # slice_sample(prop = 0.1) |> # For testing
    ggplot() +
    geom_spatraster_rgb(data = bg_tiles) +
    geom_sf(aes(fill = habitat_cso),
            colour = NA) + 
    theme_map() +
    theme(legend.position = "top") +
    scale_fill_manual(name = "Habitat",
                      values = pf_colors,
                      na.translate = FALSE,
                      na.value = "transparent")

ggsave("habitat_pf_map.jpeg",
       habitat_pf_gg,
       width = plot_width,
       height = plot_height,
       units = "in")

if (PDF_FIGS) {
    ggsave("habitat_pf_map.pdf",
           habitat_pf_gg,
           width = plot_width,
           height = plot_height,
           units = "in")
}
