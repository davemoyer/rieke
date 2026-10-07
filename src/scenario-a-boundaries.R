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
sliver_acres <- 0.5  # drop pieces smaller than this; mostly digitizing noise between the two files

current <- st_read(here("raw/boundaries/PPS_AttendanceBoundaries_20260423.shp"), quiet = TRUE) |>
  st_transform(work_crs) |>
  st_make_valid() |>
  group_by(school = K5) |>
  summarise(.groups = "drop")

scen_path <- list.files(here("raw"), pattern = "scenario.?a.*\\.shp$",
                        recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
stopifnot("Scenario A shapefile not found under raw/" = length(scen_path) == 1)

scen_raw <- st_read(scen_path, quiet = TRUE) |>
  st_transform(work_crs) |>
  st_make_valid()

# pick the text column whose values best match current K5 names; override if it guesses wrong
scen_col <- scen_raw |>
  st_drop_geometry() |>
  select(where(is.character)) |>
  map_int(\(x) sum(unique(x) %in% current$school)) |>
  which.max() |>
  names()
message("Using scenario A school column: ", scen_col)

scenario <- scen_raw |>
  group_by(school = .data[[scen_col]]) |>
  summarise(.groups = "drop")

setdiff(current$school, scenario$school)  # schools without a boundary in scenario A
setdiff(scenario$school, current$school)  # new or renamed schools

# changed areas ####

# overlay the two maps; any piece where the assigned school differs is a reassigned area
overlay <- st_intersection(
  scenario |> rename(new_school = school),
  current  |> rename(old_school = school)
) |>
  st_collection_extract("POLYGON") |>
  mutate(acres = as.numeric(st_area(geometry)) / 43560)

reassigned <- overlay |>
  filter(new_school != old_school, acres >= sliver_acres)

# scenario A territory that was outside every current boundary
added <- st_difference(scenario, st_union(current)) |>
  st_collection_extract("POLYGON") |>
  rename(new_school = school) |>
  mutate(old_school = "Outside current boundaries",
         acres = as.numeric(st_area(geometry)) / 43560) |>
  filter(acres >= sliver_acres)

new_areas <- bind_rows(reassigned, added) |>
  group_by(new_school, old_school) |>
  summarise(acres = sum(acres), .groups = "drop")

new_areas_summary <- new_areas |>
  st_drop_geometry() |>
  arrange(new_school, desc(acres))
new_areas_summary

write_csv(new_areas_summary, here("prc/scenario-a-new-areas.csv"))

# streets ####

# map window: changed areas plus a half mile of context
focus <- new_areas |>
  st_union() |>
  st_buffer(2640) |>
  st_bbox()

focus_4326 <- focus |>
  st_as_sfc() |>
  st_transform(4326) |>
  st_bbox()

streets <- tryCatch(
  opq(bbox = focus_4326, timeout = 120) |>
    add_osm_feature(key = "highway",
                    value = c("motorway", "trunk", "primary", "secondary", "tertiary",
                              "residential", "unclassified", "living_street")) |>
    osmdata_sf() |>
    pluck("osm_lines") |>
    select(name, highway) |>
    st_transform(work_crs),
  error = function(e) {
    message("OSM download failed, falling back to TIGER roads: ", conditionMessage(e))
    tigris::roads("OR", "Multnomah", progress_bar = FALSE) |>
      transmute(name = FULLNAME,
                highway = if_else(MTFCC %in% c("S1100", "S1200"), "primary", "residential")) |>
      st_transform(work_crs)
  }
) |>
  st_crop(focus) |>
  mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))

# plot ####

scenario_focus <- st_crop(scenario, focus)
current_focus  <- st_crop(current, focus)

labels <- scenario_focus |>
  st_point_on_surface()

new_areas_plt <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 14, quiet = TRUE) +
  geom_sf(data = new_areas, aes(fill = new_school), color = NA, alpha = 0.55) +
  geom_sf(data = filter(streets, !major), color = "grey45", linewidth = 0.2) +
  geom_sf(data = filter(streets, major), color = "grey25", linewidth = 0.6) +
  geom_sf(data = current_focus, fill = NA, color = "grey20",
          linewidth = 0.5, linetype = "dashed") +
  geom_sf(data = scenario_focus, fill = NA, color = "black", linewidth = 0.9) +
  geom_sf_label(data = labels, aes(label = school), size = 3,
                label.size = 0, fill = alpha("white", 0.8)) +
  coord_sf(crs = work_crs, datum = NA,
           xlim = focus[c("xmin", "xmax")], ylim = focus[c("ymin", "ymax")],
           expand = FALSE) +
  scale_fill_brewer(palette = "Set2", name = "Newly assigned to") +
  labs(
    title    = "Scenario A: Areas Changing Elementary Schools",
    subtitle = "Shaded areas move to a new school. Solid lines are Scenario A, dashed lines are current boundaries.",
    caption  = "Streets: OpenStreetMap contributors"
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 10, color = "grey40")
  )
new_areas_plt

ggsave(plot = new_areas_plt,
       here("prc/scenario-a-new-areas-map.png"),
       width = 9, height = 10, units = "in", dpi = 600)
