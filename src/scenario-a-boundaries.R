## Scenario A Boundary Comparison
## October 2026
## Dave Moyer

library(tidyverse)
library(sf)
library(ggspatial)
library(hrbrthemes)
library(osmdata)
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

# map helper ####

get_streets <- function(bbox, major_only = FALSE) {
  road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary")
  if (!major_only) road_types <- c(road_types, "residential", "unclassified", "living_street")

  bbox_4326 <- bbox |> st_as_sfc() |> st_transform(4326) |> st_bbox()

  opq(bbox = bbox_4326, timeout = 180) |>
    add_osm_feature(key = "highway", value = road_types) |>
    osmdata_sf() |>
    pluck("osm_lines") |>
    select(name, highway) |>
    st_transform(work_crs) |>
    st_crop(bbox) |>
    mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))
}

map_new_areas <- function(bbox, streets, tile_zoom, title, subtitle) {
  scen_crop <- st_crop(scenario, bbox)
  curr_crop <- st_crop(current, bbox)
  labels    <- st_point_on_surface(scen_crop)

  ggplot() +
    annotation_map_tile(type = "cartolight", zoom = tile_zoom, quiet = TRUE) +
    geom_sf(data = st_crop(new_areas, bbox), aes(fill = new_school),
            color = NA, alpha = 0.6) +
    geom_sf(data = filter(streets, !major), color = "grey50", linewidth = 0.15) +
    geom_sf(data = filter(streets, major), color = "grey30", linewidth = 0.5) +
    geom_sf(data = curr_crop, fill = NA, color = "grey20",
            linewidth = 0.4, linetype = "dashed") +
    geom_sf(data = scen_crop, fill = NA, color = "black", linewidth = 0.8) +
    geom_sf_label(data = labels, aes(label = school), size = 2.6,
                  label.size = 0, fill = alpha("white", 0.8)) +
    coord_sf(crs = work_crs, datum = NA,
             xlim = bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")],
             expand = FALSE) +
    scale_fill_discrete(name = "Newly assigned to") +
    labs(title = title, subtitle = subtitle,
         caption = "Streets: OpenStreetMap contributors. Scenario A digitized from PPS map.") +
    theme_ipsum_pub(grid = FALSE) +
    theme(
      axis.text  = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom",
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
}

map_subtitle <- "Shaded areas change schools. Solid lines are Scenario A, dashed lines are current boundaries."

# district map ####

district_bbox    <- st_bbox(st_union(st_union(current), st_union(scenario)))
district_streets <- get_streets(district_bbox, major_only = TRUE)

district_plt <- map_new_areas(district_bbox, district_streets, tile_zoom = 12,
                              title = "Scenario A: Areas Changing Elementary Schools",
                              subtitle = map_subtitle)
district_plt

ggsave(plot = district_plt,
       here("prc/scenario-a-new-areas-district.png"),
       width = 12, height = 12, units = "in", dpi = 600)

# SW map ####

sw_schools <- c("Ainsworth", "Bridlemile", "Capitol Hill", "Hayhurst",
                "Maplewood", "Markham", "Rieke", "Stephenson")

sw_bbox <- bind_rows(
  filter(current, school %in% sw_schools),
  filter(scenario, school %in% sw_schools)
) |>
  st_union() |>
  st_buffer(1320) |>
  st_bbox()

sw_streets <- get_streets(sw_bbox)

sw_plt <- map_new_areas(sw_bbox, sw_streets, tile_zoom = 14,
                        title = "Scenario A: SW Portland Areas Changing Elementary Schools",
                        subtitle = map_subtitle)
sw_plt

ggsave(plot = sw_plt,
       here("prc/scenario-a-new-areas-sw.png"),
       width = 9, height = 11, units = "in", dpi = 600)
