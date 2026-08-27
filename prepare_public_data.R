# =============================================================================
# PREPARE PUBLIC-SAFE SHINY DATA
# WNC CHILD CARE ACCESS — FINAL 18-COUNTY RUTHERFORD-REPAIRED VERSION
# =============================================================================
#
# PURPOSE
# -------
# Build the public data layer used by the WNC Child Care Access Shiny app.
#
# This version uses the FINAL repaired 18-county demand surface:
#
#   Original fixed demand surface:   37,982 locations
#   Added Rutherford locations:       3,096 locations
#   Final 18-county demand surface:  41,078 locations
#
# Rutherford demand locations were reconstructed from the
# September 7, 2023 Rutherford County E911 address-point file.
#
# PUBLIC OUTPUTS
# --------------
#   data/child_access_hex.geojson
#   data/county_summary_public.csv
#
# PRIVACY
# -------
# Exact residential locations are NEVER written to the public repository.
# Child-level results are aggregated to a 5-km hexagonal grid.
#
# IMPORTANT
# ---------
# This script does NOT rerun routing.
#
# =============================================================================


# =============================================================================
# 1. PACKAGES
# =============================================================================

library(tidyverse)
library(sf)
library(tigris)

options(
  tigris_use_cache = TRUE,
  scipen = 999
)


# =============================================================================
# 2. PROJECT PATHS
# =============================================================================

project_dir <- paste0(
  "~/Library/CloudStorage/",
  "OneDrive-WesternCarolinaUniversity(WCU)/",
  "WNC_Helene_ECRQ_Coauthor_Package_2026-08-19"
)

results_dir <- file.path(
  project_dir,
  "04_Results"
)

repair_dir <- file.path(
  project_dir,
  "10_Rutherford_Repair"
)

repair_results_dir <- file.path(
  repair_dir,
  "18county_e2sfca_recomputed"
)


# Shiny project folder.
#
# This assumes you run this script from:
#
# /Users/spedonti/Downloads/wnc-childcare-access-shiny-v1
#

public_dir <- "data"

dir.create(
  public_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# =============================================================================
# 3. FINAL REPAIRED INPUT FILES
# =============================================================================

demand_file <- file.path(
  repair_dir,
  "sampled_residences_wgs84_18COUNTY_REPAIRED.gpkg"
)

access23_file <- file.path(
  repair_results_dir,
  "child_access_2023_18COUNTY_RECOMPUTED.csv"
)

access26_file <- file.path(
  repair_results_dir,
  "child_access_2026_18COUNTY_RECOMPUTED.csv"
)

change_file <- file.path(
  repair_results_dir,
  "child_level_access_change_18COUNTY_RECOMPUTED.csv"
)


# =============================================================================
# 4. VERIFY REQUIRED REPAIRED FILES
# =============================================================================

required_files <- c(
  demand_file,
  access23_file,
  access26_file,
  change_file
)

missing_files <- required_files[
  !file.exists(
    path.expand(required_files)
  )
]

if (length(missing_files) > 0) {
  
  stop(
    "\nThe following repaired Rutherford files were not found:\n\n",
    paste(
      missing_files,
      collapse = "\n"
    ),
    "\n\nCheck the project paths before continuing."
  )
}


message(
  "\nUsing repaired 18-county analysis files."
)

message(
  "Repair directory: ",
  normalizePath(
    repair_results_dir
  )
)


# =============================================================================
# 5. READ FINAL REPAIRED DEMAND SURFACE
# =============================================================================

children <- st_read(
  path.expand(demand_file),
  quiet = TRUE
) |>
  st_transform(4326) |>
  arrange(child_row_id)


# Critical validation.

stopifnot(
  nrow(children) == 41078
)

stopifnot(
  length(
    unique(
      children$child_row_id
    )
  ) == 41078
)


message(
  "Final repaired demand surface: ",
  format(
    nrow(children),
    big.mark = ","
  ),
  " locations."
)


# Rutherford should occupy the appended IDs.

n_rutherford <- sum(
  children$child_row_id > 37982
)

stopifnot(
  n_rutherford == 3096
)


message(
  "Rutherford repaired demand locations: ",
  format(
    n_rutherford,
    big.mark = ","
  )
)


# =============================================================================
# 6. READ FINAL 18-COUNTY ACCESS RESULTS
# =============================================================================

a23 <- read_csv(
  access23_file,
  show_col_types = FALSE
)

a26 <- read_csv(
  access26_file,
  show_col_types = FALSE
)

chg <- read_csv(
  change_file,
  show_col_types = FALSE
)


# Validate row counts.

stopifnot(
  nrow(a23) == 41078,
  nrow(a26) == 41078,
  nrow(chg) == 41078
)


# Validate IDs.

stopifnot(
  setequal(
    children$child_row_id,
    a23$child_row_id
  ),
  setequal(
    children$child_row_id,
    a26$child_row_id
  ),
  setequal(
    children$child_row_id,
    chg$child_row_id
  )
)


message(
  "Repaired 2023/2026 access results validated."
)


# =============================================================================
# 7. STUDY COUNTIES
# =============================================================================

wnc_counties <- c(
  "Avery",
  "Buncombe",
  "Burke",
  "Cherokee",
  "Clay",
  "Graham",
  "Haywood",
  "Henderson",
  "Jackson",
  "Macon",
  "Madison",
  "McDowell",
  "Mitchell",
  "Polk",
  "Rutherford",
  "Swain",
  "Transylvania",
  "Yancey"
)


# =============================================================================
# 8. COUNTY BOUNDARIES
# =============================================================================

counties <- tigris::counties(
  state = "NC",
  cb = TRUE,
  year = 2024,
  class = "sf"
) |>
  filter(
    NAME %in% wnc_counties
  ) |>
  select(
    county = NAME
  ) |>
  st_make_valid() |>
  st_transform(4326)


stopifnot(
  nrow(counties) == 18
)


# =============================================================================
# 9. ASSIGN COUNTY TO EACH DEMAND LOCATION
# =============================================================================

# Remove any old county fields first to prevent duplicate-column problems.

children <- children |>
  select(
    -any_of(
      c(
        "county",
        "COUNTY",
        "County"
      )
    )
  )


children <- st_join(
  children,
  counties,
  join = st_within,
  left = TRUE
)


# Boundary-point fallback.

missing_county <- which(
  is.na(
    children$county
  )
)

if (length(missing_county) > 0) {
  
  message(
    "Assigning ",
    length(missing_county),
    " boundary points to nearest county."
  )
  
  nearest <- st_nearest_feature(
    children[
      missing_county,
    ],
    counties
  )
  
  children$county[
    missing_county
  ] <- counties$county[
    nearest
  ]
}


stopifnot(
  !any(
    is.na(
      children$county
    )
  )
)


# Validate Rutherford.

ruth_count <- sum(
  children$county == "Rutherford"
)

message(
  "Demand points assigned to Rutherford County: ",
  format(
    ruth_count,
    big.mark = ","
  )
)


# =============================================================================
# 10. COMBINE CHILD-LEVEL ACCESS RESULTS
# =============================================================================

child_access <- children |>
  select(
    child_row_id,
    county
  ) |>
  
  left_join(
    
    a23 |>
      select(
        child_row_id,
        
        providers_2023 =
          providers_within_20,
        
        access_center_2023 =
          access_center,
        
        access_capacity_2023 =
          access_capacity
      ),
    
    by = "child_row_id"
  ) |>
  
  left_join(
    
    a26 |>
      select(
        child_row_id,
        
        providers_2026 =
          providers_within_20,
        
        access_center_2026 =
          access_center,
        
        access_capacity_2026 =
          access_capacity
      ),
    
    by = "child_row_id"
  ) |>
  
  left_join(
    
    chg |>
      select(
        child_row_id,
        
        provider_change,
        
        center_access_change,
        
        capacity_access_change,
        
        lost_all_access,
        
        gained_any_access
      ),
    
    by = "child_row_id"
  )


# Final validation.

stopifnot(
  nrow(child_access) == 41078
)

stopifnot(
  !any(
    is.na(
      child_access$access_capacity_2023
    )
  )
)

stopifnot(
  !any(
    is.na(
      child_access$access_capacity_2026
    )
  )
)


# =============================================================================
# 11. PERSISTENT LOW ACCESS
# =============================================================================

cut23 <- quantile(
  child_access$access_capacity_2023,
  0.10,
  na.rm = TRUE
)

cut26 <- quantile(
  child_access$access_capacity_2026,
  0.10,
  na.rm = TRUE
)


child_access <- child_access |>
  mutate(
    
    low_2023 =
      access_capacity_2023 <= cut23,
    
    low_2026 =
      access_capacity_2026 <= cut26,
    
    persistence = case_when(
      
      low_2023 &
        low_2026 ~
        "Persistently lowest-access",
      
      low_2023 &
        !low_2026 ~
        "Moved out of lowest-access decile",
      
      !low_2023 &
        low_2026 ~
        "Moved into lowest-access decile",
      
      TRUE ~
        "Not in lowest-access decile"
    )
  )


# =============================================================================
# 12. COUNTY + REGIONAL SUMMARY
# =============================================================================

summarise_area <- function(x, area_name) {
  
  tibble(
    
    county = area_name,
    
    n_locations =
      nrow(x),
    
    mean_centers_2023 =
      mean(
        x$providers_2023,
        na.rm = TRUE
      ),
    
    mean_centers_2026 =
      mean(
        x$providers_2026,
        na.rm = TRUE
      ),
    
    mean_capacity_2023 =
      mean(
        x$access_capacity_2023,
        na.rm = TRUE
      ),
    
    mean_capacity_2026 =
      mean(
        x$access_capacity_2026,
        na.rm = TRUE
      ),
    
    mean_capacity_change =
      mean(
        x$capacity_access_change,
        na.rm = TRUE
      ),
    
    pct_improved_capacity =
      mean(
        x$capacity_access_change > 0,
        na.rm = TRUE
      ),
    
    pct_declined_capacity =
      mean(
        x$capacity_access_change < 0,
        na.rm = TRUE
      ),
    
    pct_no_center_2023 =
      mean(
        x$providers_2023 == 0,
        na.rm = TRUE
      ),
    
    pct_no_center_2026 =
      mean(
        x$providers_2026 == 0,
        na.rm = TRUE
      ),
    
    pct_persistent_low =
      mean(
        x$persistence ==
          "Persistently lowest-access",
        na.rm = TRUE
      )
  )
}


child_plain <- st_drop_geometry(
  child_access
)


overall_summary <- summarise_area(
  child_plain,
  "All WNC counties"
)


county_summary <- child_plain |>
  group_split(
    county
  ) |>
  map_dfr(
    function(d) {
      
      summarise_area(
        d,
        unique(
          d$county
        )
      )
    }
  )


summary_out <- bind_rows(
  overall_summary,
  county_summary
)


stopifnot(
  nrow(summary_out) == 19
)


write_csv(
  summary_out,
  file.path(
    public_dir,
    "county_summary_public.csv"
  )
)


# =============================================================================
# 13. RUTHERFORD QA
# =============================================================================

ruth_summary <- summary_out |>
  filter(
    county == "Rutherford"
  )


message(
  "\nRUTHERFORD PUBLIC SUMMARY"
)

print(
  ruth_summary
)


# =============================================================================
# 14. BUILD PUBLIC 5-KM HEX GRID
# =============================================================================

# Project to CONUS Albers.
# Units are meters.

work_crs <- 5070


children_proj <- child_access |>
  st_transform(
    work_crs
  )


counties_proj <- counties |>
  st_transform(
    work_crs
  )


wnc_outline <- counties_proj |>
  summarise()


# 5 km hexagonal grid.

hex_grid <- st_make_grid(
  
  wnc_outline,
  
  cellsize = 5000,
  
  square = FALSE
  
) |>
  st_as_sf() |>
  mutate(
    hex_id =
      row_number()
  )


# Keep only cells intersecting the study region.

hex_grid <- hex_grid[
  lengths(
    st_intersects(
      hex_grid,
      wnc_outline
    )
  ) > 0,
]


# =============================================================================
# 15. ASSIGN CHILD-DEMAND POINTS TO HEXES
# =============================================================================

child_hex <- st_join(
  
  children_proj,
  
  hex_grid |>
    select(
      hex_id
    ),
  
  join = st_within,
  
  left = FALSE
)


stopifnot(
  nrow(child_hex) == 41078
)


# =============================================================================
# 16. HEX-LEVEL ACCESS SUMMARY
# =============================================================================

hex_stats <- child_hex |>
  st_drop_geometry() |>
  group_by(
    hex_id
  ) |>
  summarise(
    
    n_locations =
      n(),
    
    access_capacity_2023 =
      mean(
        access_capacity_2023,
        na.rm = TRUE
      ),
    
    access_capacity_2026 =
      mean(
        access_capacity_2026,
        na.rm = TRUE
      ),
    
    capacity_access_change =
      mean(
        capacity_access_change,
        na.rm = TRUE
      ),
    
    mean_providers_2023 =
      mean(
        providers_2023,
        na.rm = TRUE
      ),
    
    mean_providers_2026 =
      mean(
        providers_2026,
        na.rm = TRUE
      ),
    
    pct_improved =
      mean(
        capacity_access_change > 0,
        na.rm = TRUE
      ),
    
    pct_persistent_low =
      mean(
        persistence ==
          "Persistently lowest-access",
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# =============================================================================
# 17. DOMINANT PERSISTENCE CATEGORY
# =============================================================================

dominant_persistence <- child_hex |>
  st_drop_geometry() |>
  count(
    hex_id,
    persistence,
    name = "n"
  ) |>
  group_by(
    hex_id
  ) |>
  slice_max(
    n,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    hex_id,
    persistence
  )


# =============================================================================
# 18. ATTACH HEX GEOMETRIES
# =============================================================================

hex_public <- hex_grid |>
  inner_join(
    hex_stats,
    by = "hex_id"
  ) |>
  left_join(
    dominant_persistence,
    by = "hex_id"
  )


# =============================================================================
# 19. ASSIGN DISPLAY COUNTY TO HEX
# =============================================================================

# Use point-on-surface rather than centroid because some cells cross
# county borders.

hex_points <- st_point_on_surface(
  hex_public
)


hex_county <- st_join(
  
  hex_points,
  
  counties_proj,
  
  join = st_within,
  
  left = TRUE
  
) |>
  st_drop_geometry() |>
  select(
    hex_id,
    county
  )


hex_public <- hex_public |>
  left_join(
    hex_county,
    by = "hex_id"
  )


# =============================================================================
# 20. PRIVACY / STABILITY SUPPRESSION
# =============================================================================

# Do not publish cells represented by very few modeled residential points.

MIN_PUBLIC_LOCATIONS <- 10


hex_public <- hex_public |>
  filter(
    n_locations >=
      MIN_PUBLIC_LOCATIONS
  )


message(
  "\nPublic hexagons after n >= ",
  MIN_PUBLIC_LOCATIONS,
  " suppression: ",
  nrow(hex_public)
)


# =============================================================================
# 21. VERIFY RUTHERFORD IS PRESENT IN PUBLIC HEXES
# =============================================================================

ruth_hex_n <- hex_public |>
  st_drop_geometry() |>
  filter(
    county == "Rutherford"
  ) |>
  nrow()


if (ruth_hex_n == 0) {
  
  stop(
    "No Rutherford County hexagons survived the public aggregation. ",
    "Do not publish until this is investigated."
  )
}


message(
  "Public Rutherford County hexagons: ",
  ruth_hex_n
)


# =============================================================================
# 22. TRANSFORM FOR LEAFLET
# =============================================================================

hex_public <- hex_public |>
  st_transform(
    4326
  )


# =============================================================================
# 23. WRITE PUBLIC GEOJSON
# =============================================================================

geojson_file <- file.path(
  public_dir,
  "child_access_hex.geojson"
)


if (
  file.exists(
    geojson_file
  )
) {
  
  file.remove(
    geojson_file
  )
}


st_write(
  
  hex_public,
  
  geojson_file,
  
  driver = "GeoJSON",
  
  quiet = TRUE
)


# =============================================================================
# 24. WRITE PUBLIC METADATA
# =============================================================================

metadata <- tibble(
  
  item = c(
    "analysis_version",
    "demand_surface_n",
    "original_demand_n",
    "rutherford_added_n",
    "rutherford_source",
    "hex_cellsize_m",
    "minimum_locations_per_public_hex",
    "access_measure"
  ),
  
  value = c(
    "18-county Rutherford-repaired",
    "41078",
    "37982",
    "3096",
    "Rutherford E911 address points, 2023-09-07",
    "5000",
    as.character(
      MIN_PUBLIC_LOCATIONS
    ),
    "Enhanced two-step floating catchment area (E2SFCA)"
  )
)


write_csv(
  metadata,
  file.path(
    public_dir,
    "public_data_metadata.csv"
  )
)


# =============================================================================
# 25. FINAL QA
# =============================================================================

message(
  "\n============================================================"
)

message(
  "PUBLIC SHINY DATA PREPARATION COMPLETE"
)

message(
  "============================================================"
)

message(
  "Final analytic demand surface: 41,078"
)

message(
  "Original 17-county demand points: 37,982"
)

message(
  "Added Rutherford demand points: 3,096"
)

message(
  "Public hexes: ",
  nrow(hex_public)
)

message(
  "Rutherford public hexes: ",
  ruth_hex_n
)

message(
  "\nFiles written:"
)

message(
  "  ",
  geojson_file
)

message(
  "  ",
  file.path(
    public_dir,
    "county_summary_public.csv"
  )
)

message(
  "  ",
  file.path(
    public_dir,
    "public_data_metadata.csv"
  )
)

message(
  "\nNo exact residential coordinates were exported."
)

message(
  "No routing was rerun."
)

message(
  "============================================================\n"
)