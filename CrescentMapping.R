#### Script to map out Crescent Watershed in MDVs 

# ============================================================
# Publication-Ready Watershed Map — McMurdo Dry Valleys, Antarctica
# ============================================================
# Requirements:
#   install.packages(c("sf", "ggplot2", "ggspatial", "cowplot",
#                      "RColorBrewer", "viridis", "scales"))
# ============================================================

library(sf)
library(ggplot2)
library(ggspatial)
library(maptiles)
library(tidyterra)
library(terra)
library(cowplot)


# ============================================================
# 1. LOAD YOUR SHAPEFILES
#    Replace the paths below with your actual file locations.
# ============================================================

watershed  <- st_read(here("Crescent_Watershed.shp"))
# streamlines <- st_read("path/to/your/streamlines.shp")


watershed_4326 <- st_transform(watershed, 4326)
watershed_3031 <- st_transform(watershed, 3031)

# ============================================================
# 3. MAP EXTENT — centered on Taylor Valley / Lake Fryxell
#
#    These bbox coords cover the full valley from the glaciers
#    in the west to the coast in the east. Adjust if needed.
#
#    Approx bounds (WGS84):
#      Longitude: 162.0 – 164.0
#      Latitude:  -77.75 – -77.55
# ============================================================

valley_bbox_4326 <- st_bbox(
  c(xmin = 162.0, xmax = 164.0,
    ymin = -77.75, ymax = -77.55),
  crs = st_crs(4326)
)

# ============================================================
# 4. FETCH SATELLITE BASEMAP via maptiles
#
#    Provider options: "Esri.WorldImagery" (best for Antarctica),
#    "CartoDB.Positron", "OpenTopoMap", etc.
#    Zoom 11–12 gives good detail for a valley-scale map.
#    Note: requires an internet connection at render time.
# ============================================================

basemap_tiles <- get_tiles(
  x        = st_as_sfc(valley_bbox_4326),
  provider = "Esri.WorldImagery",
  zoom     = 12,
  crop     = TRUE
)

# Reproject tiles to Antarctic Polar Stereographic
basemap_3031 <- project(basemap_tiles, "EPSG:3031")

# Reproject the valley bbox to 3031 for plot limits
valley_sf_3031 <- st_transform(
  st_as_sfc(valley_bbox_4326), 3031
)
plot_bbox <- st_bbox(valley_sf_3031)

# ============================================================
# 5. LAKE FRYXELL — approximate polygon (WGS84)
#
#    Hand-digitised outline; accurate to ~100 m.
#    Replace with your own shapefile if you have one.
# ============================================================

fryxell_coords <- matrix(c(
  163.145, -77.608,
  163.175, -77.612,
  163.215, -77.616,
  163.255, -77.618,
  163.290, -77.617,
  163.320, -77.613,
  163.340, -77.608,
  163.335, -77.603,
  163.305, -77.599,
  163.265, -77.597,
  163.220, -77.597,
  163.180, -77.600,
  163.150, -77.604,
  163.145, -77.608   # close the ring
), ncol = 2, byrow = TRUE)

fryxell_sf <- st_sfc(
  st_polygon(list(fryxell_coords)),
  crs = 4326
) |>
  st_as_sf() |>
  st_transform(3031)

# ============================================================
# 6. STREAMLINES — if you have a streamlines shapefile,
#    uncomment and load it here.
# ============================================================

# streamlines <- st_read("path/to/your/streamlines.shp") |>
#   st_transform(3031)

# ============================================================
# 7. COLOR PALETTE
# ============================================================

col_watershed_border <- "#FF6B35"    # vivid orange — stands out on satellite
col_lake             <- "#2980B9"    # medium blue, semi-transparent
col_stream           <- "#85C1E9"    # light blue for streams
col_text             <- "#FFFFFF"    # white labels on dark satellite bg
col_shadow           <- "#000000"    # label shadow

# ============================================================
# 8. MAIN MAP
# ============================================================

main_map <- ggplot() +
  
  # --- Satellite basemap ---
  geom_spatraster_rgb(data = basemap_3031) +
  
  # # --- Lake Fryxell ---
  # geom_sf(data      = fryxell_sf,
  #         fill      = col_lake,
  #         color     = "#1A5276",
  #         linewidth = 0.5,
  #         alpha     = 0.55) +
  
  # --- Streamlines (uncomment if loaded above) ---
  # geom_sf(data      = streamlines,
  #         color     = col_stream,
  #         linewidth = 0.7,
  #         alpha     = 0.9) +
  
  # --- Watershed boundary (outline only, no fill) ---
  geom_sf(data      = watershed_3031,
          fill      = NA,
          color     = col_watershed_border,
          linewidth = 1.1,
          linetype  = "solid") +
  
  # --- Lake label ---
  annotate("text",
           x     = mean(st_bbox(fryxell_sf)[c("xmin","xmax")]),
           y     = mean(st_bbox(fryxell_sf)[c("ymin","ymax")]),
           label = "Lake Fryxell",
           color = "white", fontface = "italic",
           size  = 3.2, family = "serif") +
  
  # --- Map extent ---
  coord_sf(
    xlim   = c(plot_bbox["xmin"], plot_bbox["xmax"]),
    ylim   = c(plot_bbox["ymin"], plot_bbox["ymax"]),
    expand = FALSE,
    crs    = st_crs(3031)
  ) +
  
  # --- North arrow ---
  annotation_north_arrow(
    location    = "tl",
    which_north = "true",
    style       = north_arrow_fancy_orienteering(
      fill     = c("white", "grey30"),
      line_col = "white",
      text_col = "white"
    ),
    height = unit(1.1, "cm"),
    width  = unit(1.1, "cm"),
    pad_x  = unit(0.4, "cm"),
    pad_y  = unit(0.4, "cm")
  ) +
  
  # --- Scale bar ---
  annotation_scale(
    location   = "br",
    width_hint = 0.22,
    line_width = 0.7,
    text_cex   = 0.72,
    pad_x      = unit(0.4, "cm"),
    pad_y      = unit(0.4, "cm"),
    line_col   = "white",
    text_col   = "white",
    bar_cols   = c("white", "grey40")
  ) +
  
  # --- Labels ---
  labs(
    title    = "Taylor Valley, McMurdo Dry Valleys, Antarctica",
    subtitle = "Watershed boundary shown in orange  |  Lake Fryxell",
    caption  = "Basemap: Esri World Imagery  |  Projection: Antarctic Polar Stereographic (EPSG:3031)"
  ) +
  
  # --- Theme ---
  theme_void(base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = "#0D1117", color = NA),
    panel.background = element_rect(fill = "#0D1117", color = NA),
    
    panel.border = element_rect(fill  = NA,
                                color = "grey50",
                                linewidth = 0.6),
    
    plot.title    = element_text(color = "white", face = "bold",
                                 size = 13, margin = margin(b = 3),
                                 hjust = 0),
    plot.subtitle = element_text(color = "#AABBC8", size = 9,
                                 margin = margin(b = 5), hjust = 0),
    plot.caption  = element_text(color = "grey55", size = 7,
                                 hjust = 0, margin = margin(t = 6)),
    plot.margin   = margin(12, 14, 10, 12)
  )

# ============================================================
# 9. OPTIONAL INSET — Antarctica locator map
#    Shows where Taylor Valley sits on the continent.
#    Requires rnaturalearth + rnaturalearthdata.
# ============================================================

library(rnaturalearth)
library(rnaturalearthdata)

antarctica <- ne_countries(continent  = "Antarctica",
                           returnclass = "sf",
                           scale       = "medium") |>
  st_transform(3031)

# Centroid of the valley extent for the red locator dot
valley_centroid <- st_centroid(st_as_sfc(valley_bbox_4326)) |>
  st_transform(3031)

inset <- ggplot() +
  geom_sf(data = antarctica,
          fill  = "#2C3E50", color = "#7F8C8D", linewidth = 0.25) +
  geom_sf(data  = valley_centroid,
          color = "#E74C3C", size = 2.5, shape = 16) +
  coord_sf(crs = st_crs(3031)) +
  theme_void() +
  theme(
    panel.border     = element_rect(color = "grey60", fill = NA,
                                    linewidth = 0.5),
    plot.background  = element_rect(fill = "#0D1117", color = NA)
  )

# Combine main map + inset
final_map <- ggdraw(main_map) +
  draw_plot(inset,
            x      = 0.01,   # left side — adjust if it overlaps features
            y      = 0.62,
            width  = 0.22,
            height = 0.22)

# ============================================================
# 10. EXPORT
#     PDF = vector (best for journals); PNG = quick preview.
#     Common journal widths: 3.5" (1-col) or 7" (2-col).
# ============================================================

ggsave(
  filename = "taylor_valley_fryxell_map.pdf",
  plot     = final_map,
  width    = 7,
  height   = 5.5,
  dpi      = 600,
  device   = "pdf"
)

ggsave(
  filename = "taylor_valley_fryxell_map.png",
  plot     = final_map,
  width    = 7,
  height   = 5.5,
  dpi      = 300,
  bg       = "#0D1117"
)

message("✓ Map exported: taylor_valley_fryxell_map.pdf / .png")