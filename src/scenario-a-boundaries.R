## Scenario A Boundary Comparison
## October 2026
## Dave Moyer

library(tidyverse)
library(sf)
library(ggspatial)
library(hrbrthemes)
library(here)

# data ####

# work in Oregon North state plane (ft) so areas and buffers are in real units
work_crs <- 2913

# scenario A was digitized from a PDF map, so edges don't line up exactly with the
# current boundaries. pieces that disappear after shrinking by this many feet are
# treated as edge noise rather than real reassignments
sliver_ft    <- 150
sliver_acres <- 5

current <- st_read(here("raw/boundaries/PPS_AttendanceBoundaries_20260423.shp"), quiet = TRUE) |>
  st_zm() |>
  st_transform(work_crs) |>
  st_make_valid() |>
  group_by(school = K5) |>
  summarise(.groups = "drop")

scenario <- st_read(here("raw/boundaries/PPS_ScenarioA_K5_Attendance_2027_28.shp"), quiet = TRUE) |>
  st_zm() |>
  st_transform(work_crs) |>
  st_make_valid() |>
  mutate(school = recode(school,
                         "Bridger Creative Science" = "Bridger",
                         "Sunnyside Environmental"  = "Sunnyside",
                         "(K-8 area north of Sitton)" = "Unassigned K-8 area")) |>
  group_by(school) |>
  summarise(.groups = "drop")

hillsdale <- st_read(here("raw/boundaries/Neighborhood_Boundaries.shp"), quiet = TRUE) |>
  st_zm() |>
  st_transform(work_crs) |>
  st_make_valid() |>
  filter(NAME %in% c('HILLSDALE','MULTONOMAH'))

closed_schools <- setdiff(current$school, scenario$school)
new_schools    <- setdiff(scenario$school, current$school)
closed_schools
new_schools

# changed areas ####

# overlay the two maps; any piece where the assigned school differs is a reassigned area
overlay <- st_intersection(
  scenario |> rename(new_school = school),
  current  |> rename(old_school = school)
) |>
  st_collection_extract("POLYGON")

drop_slivers <- function(x) {
  keep <- !st_is_empty(st_buffer(x, -sliver_ft))
  x[keep, ] |>
    mutate(acres = as.numeric(st_area(geometry)) / 43560) |>
    filter(acres >= sliver_acres)
}

reassigned <- overlay |>
  filter(new_school != old_school) |>
  drop_slivers()

# scenario A territory that was outside every current boundary
added <- st_difference(scenario, st_union(current)) |>
  st_collection_extract("POLYGON") |>
  rename(new_school = school) |>
  mutate(old_school = "Outside current boundaries") |>
  drop_slivers()

new_areas <- bind_rows(reassigned, added) |>
  group_by(new_school, old_school) |>
  summarise(acres = sum(acres), .groups = "drop") |>
  mutate(from_closed = old_school %in% closed_schools)

new_areas_summary <- new_areas |>
  st_drop_geometry() |>
  arrange(new_school, desc(acres))
new_areas_summary

write_csv(new_areas_summary, here("prc/scenario-a-new-areas.csv"))

# SW map ####

sw_schools <- c("Ainsworth", 
                "Bridlemile", 
                "Capitol Hill", 
                "Hayhurst",
                "Maplewood", 
                "Markham", 
                "Rieke", 
                "Stephenson")

# buffer around the SW schools' current and scenario boundaries
sw_bbox <- bind_rows(
  filter(current, school %in% sw_schools),
  filter(scenario, school %in% sw_schools)
) |>
  st_union() |>
  st_buffer(500) |>
  st_bbox()

sw_streets <- st_read(here("raw/boundaries/Streets.shp"), quiet = TRUE) |>
  st_zm() |>
  st_transform(work_crs) |>
  st_crop(sw_bbox)

sw_scenario <- st_crop(scenario, sw_bbox)
sw_current  <- st_crop(current, sw_bbox)

maplewood  <- c(-122.73025883624236, 45.47087272875078)
stephenson <- c(-122.70397203129284, 45.440846087570804)
rieke <- c(-122.6951463020293,45.476384894023845)
hayhurst <- c(-122.72912815229604,45.48011260329334)
capitol_hill <- c(-122.69538301552195, 45.464083746341515)
markham <- c(-122.72455397329051,45.449526434268996)
ainsworth <- c(45.5098799,-122.6998559)

sw_points <- tribble(
  ~label,         ~lon,                ~lat,
  "Maplewood",    -122.73025883624236, 45.47087272875078,
  "Stephenson",   -122.70397203129284, 45.440846087570804,
  "Rieke",        -122.6951463020293,  45.476384894023845,
  "Hayhurst",     -122.72912815229604, 45.48011260329334,
  "Capitol Hill", -122.69538301552195, 45.464083746341515,
  "Markham",      -122.72455397329051,45.449526434268996,
  'Ainsworth',    -122.6998559, 45.5098799,
  'Bridlemile',     -122.7242829, 45.4917482
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(work_crs) |>
  mutate(status = if_else(label %in% closed_schools, "Closed", "Open"))

sw_new_areas <- st_crop(new_areas, sw_bbox)

# Rieke's color from the default fill palette, lightened to match the 0.6 alpha shading
fill_levels <- sort(unique(sw_new_areas$new_school))
rieke_fill  <- scales::hue_pal()(length(fill_levels))[fill_levels == "Rieke"]
rieke_fill  <- colorRampPalette(c("white", rieke_fill))(11)[7]

callouts <- new_areas |>
  filter(new_school == "Rieke", old_school %in% c("Hayhurst", "Maplewood")) |>
  st_point_on_surface() |>
  mutate(label = str_glue("From {old_school}"),
         x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2],
         # label offsets from the area, in feet (east/north are positive)
         label_x = x + case_match(old_school, "Hayhurst" ~ 3000, "Maplewood" ~ 4400),
         label_y = y + case_match(old_school, "Hayhurst" ~ 300,  "Maplewood" ~ -2900)) |>
  st_drop_geometry()
callouts

sw_plt <- ggplot() +
  #annotation_map_tile(type = "cartolight", zoom = 14, quiet = TRUE) +
  geom_sf(data = sw_new_areas, aes(fill = new_school),
          color = NA, alpha = 0.6) +
  geom_sf(data = sw_streets, color = "grey50", linewidth = 0.15) +
  geom_sf(data = sw_current, fill = NA, color = "grey20",
          linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = sw_scenario, fill = NA, color = "black", linewidth = 0.8) +
  #geom_sf(data = hillsdale, fill = NA, color = "#7b3294", linewidth = 1.2) +   # Hillsdale neighborhood outline
  geom_sf(data = sw_points, aes(shape = status, color = status),
          size = 2, stroke = 1.5, show.legend = FALSE) +
  geom_sf_label(data = st_point_on_surface(sw_scenario), aes(label = school),
                size = 2.6, label.size = 0, fill = alpha("white", 0.8)) +
  # callouts: leader line from the area to a label parked in open space
  geom_segment(data = callouts,
               aes(x = x, y = y, xend = label_x, yend = label_y),
               color = "grey20", linewidth = 0.3) +
  geom_label(data = callouts,
             aes(x = label_x, y = label_y, label = label),
             size = 2.6, label.size = 0, fill = rieke_fill) +
  coord_sf(crs = work_crs, datum = NA,
           xlim = sw_bbox[c("xmin", "xmax")], ylim = sw_bbox[c("ymin", "ymax")],
           expand = FALSE) +
  scale_fill_discrete(name = "Newly assigned to") +
  scale_shape_manual(name = "School site",
                     values = c("Closed" = 4, "Open" = 16),
                     guide = 'none') +
  scale_color_manual(name = "School site",
                     values = c("Closed" = "#d7191c", "Open" = "black"),
                     guide = 'none') +
  labs(title = "Scenario A: SW Portland Changing Elementary Schools",
       subtitle = "Shaded areas change schools. Solid lines are Scenario A, dashed lines are current boundaries.\nPurple outline is the Hillsdale neighborhood.",
       caption = "Scenario A digitized from PPS map - may contain errors",
       x = NULL, y = NULL) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    axis.text  = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 10, color = "grey40")
  )
sw_plt

walk(c("png", "pdf"), \(ext) {
  ggsave(plot = sw_plt,
         here(str_glue("prc/scenario-a-new-areas-sw.{ext}")),
         width = 8.5, height = 11, units = "in", dpi = 600,
         device = if (ext == "pdf") cairo_pdf else NULL)
})
