# ==============================================================================
# PREPARE PUBLIC DATA — EC-ELIGIBILITY CORRECTED
# WNC child-care accessibility dashboard, 2023 vs 2026
#
# Inputs are the corrected 41,078-location E2SFCA and quality outputs.
# Public output is aggregated to 5-km hexagons; cells with <10 modeled demand
# locations are suppressed.
#
# NO HERE/ROUTING/API CALLS ARE MADE.
# ==============================================================================

library(tidyverse)
library(sf)

# ------------------------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------------------------

repair_dir <- paste0(
  "~/Library/CloudStorage/",
  "OneDrive-WesternCarolinaUniversity(WCU)/",
  "WNC_Helene_ECRQ_Coauthor_Package_2026-08-19/",
  "10_Rutherford_Repair"
)

e2_dir <- file.path(
  repair_dir,
  "18county_e2sfca_recomputed"
)

ec_dir <- file.path(
  e2_dir,
  "EC_AGE_ELIGIBILITY_CORRECTED"
)

quality_dir <- file.path(
  ec_dir,
  "QUALITY_EC_CORRECTED"
)

# Run this script from the root of your Shiny/GitHub project.
public_dir <- "data"
dir.create(public_dir, showWarnings = FALSE, recursive = TRUE)

child_file <- file.path(
  repair_dir,
  "sampled_residences_wgs84_18COUNTY_REPAIRED.gpkg"
)

access23_file <- file.path(
  ec_dir,
  "child_access_2023_EC_CORRECTED.csv"
)

access26_file <- file.path(
  ec_dir,
  "child_access_2026_EC_CORRECTED.csv"
)

change_file <- file.path(
  ec_dir,
  "child_access_change_EC_CORRECTED.csv"
)

quality_file <- file.path(
  quality_dir,
  "QUALITY_ACCESS_CHILD_LEVEL_EC_CORRECTED.csv"
)

quality_headline_file <- file.path(
  quality_dir,
  "QUALITY_ACCESS_HEADLINE_EC_CORRECTED.csv"
)

quality_coverage_file <- file.path(
  quality_dir,
  "QUALITY_RATING_COVERAGE_EC_CORRECTED.csv"
)

# ------------------------------------------------------------------------------
# 2. READ CORRECTED ANALYTIC OUTPUTS
# ------------------------------------------------------------------------------

children <- st_read(
  child_file,
  quiet = TRUE
)

if (!"child_row_id" %in% names(children)) {
  children <- children |>
    mutate(child_row_id = row_number())
}

stopifnot(nrow(children) == 41078)

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

qual <- read_csv(
  quality_file,
  show_col_types = FALSE
)

qheadline <- read_csv(
  quality_headline_file,
  show_col_types = FALSE
)

qcoverage <- read_csv(
  quality_coverage_file,
  show_col_types = FALSE
)

stopifnot(
  nrow(a23) == 41078,
  nrow(a26) == 41078,
  nrow(chg) == 41078,
  nrow(qual) == 41078
)

# ------------------------------------------------------------------------------
# 3. BUILD ONE CHILD-LEVEL SPATIAL TABLE
# ------------------------------------------------------------------------------

child <- children |>
  select(
    child_row_id,
    any_of(c("UniquID", "county"))
  ) |>
  left_join(
    a23 |>
      select(
        child_row_id,
        providers_2023 = providers_within_20,
        access_center_2023 = access_center,
        access_capacity_2023 = access_capacity
      ),
    by = "child_row_id"
  ) |>
  left_join(
    a26 |>
      select(
        child_row_id,
        providers_2026 = providers_within_20,
        access_center_2026 = access_center,
        access_capacity_2026 = access_capacity
      ),
    by = "child_row_id"
  ) |>
  left_join(
    chg |>
      select(
        child_row_id,
        any_of(c(
          "provider_change",
          "center_access_change",
          "capacity_access_change"
        ))
      ),
    by = "child_row_id"
  ) |>
  left_join(
    qual |>
      select(
        child_row_id,
        quality_access_2023 =
          quality_weighted_capacity_access_2023,
        quality_access_2026 =
          quality_weighted_capacity_access_2026,
        quality_access_change =
          quality_weighted_capacity_access_change,
        high_quality_centers_2023 =
          high_quality_centers_within_20_2023,
        high_quality_centers_2026 =
          high_quality_centers_within_20_2026
      ),
    by = "child_row_id"
  )

# Recalculate changes if an older saved change file lacks them.
child <- child |>
  mutate(
    provider_change = coalesce(
      provider_change,
      providers_2026 - providers_2023
    ),
    center_access_change = coalesce(
      center_access_change,
      access_center_2026 - access_center_2023
    ),
    capacity_access_change = coalesce(
      capacity_access_change,
      access_capacity_2026 - access_capacity_2023
    ),
    quality_access_change = coalesce(
      quality_access_change,
      quality_access_2026 - quality_access_2023
    )
  )

# ------------------------------------------------------------------------------
# 4. COUNTY # ------------------------------------------------------------------------------
# 4. COUNTY ASSIGNMENT
# ------------------------------------------------------------------------------

# Prefer an existing county variable on the repaired demand surface.
if ("county" %in% names(child) &&
    any(!is.na(child$county))) {
  
  child <- child |>
    mutate(
      county = str_to_title(as.character(county))
    )
  
} else {
  
  message(
    "No usable county field found on child points; assigning Census counties."
  )
  
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop(
      "Install tigris or add a county field to the repaired child-demand layer."
    )
  }
  
  counties <- tigris::counties(
    state = "NC",
    cb = TRUE,
    year = 2024,
    class = "sf"
  ) |>
    select(county = NAME) |>
    st_make_valid() |>
    st_transform(st_crs(child))
  
  # Use intersects rather than within.
  # This also captures points lying exactly on county boundaries.
  child <- st_join(
    child,
    counties,
    join = st_intersects,
    left = TRUE
  )
  
  # --------------------------------------------------------------------------
  # Handle any remaining unmatched points.
  # These are usually tiny boundary/geometry precision mismatches.
  # --------------------------------------------------------------------------
  
  missing_county <- which(
    is.na(child$county)
  )
  
  message(
    "Points still missing county after polygon join: ",
    length(missing_county)
  )
  
  if (length(missing_county) > 0) {
    
    # Work in a projected CRS so distances are measured in meters.
    child_proj <- st_transform(
      child,
      5070
    )
    
    counties_proj <- st_transform(
      counties,
      5070
    )
    
    nearest_idx <- st_nearest_feature(
      child_proj[missing_county, ],
      counties_proj
    )
    
    nearest_dist <- st_distance(
      child_proj[missing_county, ],
      counties_proj[nearest_idx, ],
      by_element = TRUE
    )
    
    nearest_dist_m <- as.numeric(
      nearest_dist
    )
    
    cat(
      "\nDistance of unmatched points to nearest NC county:\n"
    )
    
    print(
      summary(nearest_dist_m)
    )
    
    # Assign nearest county only for the unmatched points.
    child$county[missing_county] <-
      counties_proj$county[nearest_idx]
    
    # Flag anything suspiciously far away.
    if (any(nearest_dist_m > 5000)) {
      
      warning(
        sum(nearest_dist_m > 5000),
        " unmatched demand locations were more than 5 km ",
        "from the nearest NC county polygon. Review these points."
      )
    }
  }
}

cat(
  "\nFinal missing county assignments: ",
  sum(is.na(child$county)),
  "\n"
)

stopifnot(
  sum(is.na(child$county)) == 0
)
# ------------------------------------------------------------------------------

# Prefer an existing county variable on the repaired demand surface.
if ("county" %in% names(child) &&
    any(!is.na(child$county))) {

  child <- child |>
    mutate(
      county = str_to_title(as.character(county))
    )

} else {

  message(
    "No usable county field found on child points; using Census counties via tigris."
  )

  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop(
      "Install tigris or add a county field to the repaired child-demand layer."
    )
  }

  counties <- tigris::counties(
    state = "NC",
    cb = TRUE,
    year = 2024,
    class = "sf"
  ) |>
    select(county = NAME) |>
    st_transform(st_crs(child))

  child <- st_join(
    child,
    counties,
    join = st_within,
    left = TRUE
  )
}

stopifnot(
  sum(is.na(child$county)) == 0
)

# ------------------------------------------------------------------------------
# 5. LOWEST-ACCESS PERSISTENCE
# ------------------------------------------------------------------------------

q10_23 <- quantile(
  child$access_capacity_2023,
  0.10,
  na.rm = TRUE
)

q10_26 <- quantile(
  child$access_capacity_2026,
  0.10,
  na.rm = TRUE
)

child <- child |>
  mutate(
    low23 = access_capacity_2023 <= q10_23,
    low26 = access_capacity_2026 <= q10_26,
    persistence = case_when(
      low23 & low26 ~ "Persistently lowest-access",
      low23 & !low26 ~ "Moved out of lowest-access decile",
      !low23 & low26 ~ "Moved into lowest-access decile",
      TRUE ~ "Not in lowest-access decile"
    )
  )

# ------------------------------------------------------------------------------
# 6. PUBLIC 5-KM HEXAGONS
# ------------------------------------------------------------------------------

# EPSG:5070 = NAD83 / Conus Albers, meters.
child_5070 <- st_transform(
  child,
  5070
)

hex_grid <- st_make_grid(
  child_5070,
  cellsize = 5000,
  square = FALSE
) |>
  st_as_sf() |>
  mutate(
    hex_id = row_number()
  )

# Assign each modeled demand location to one hexagon.
child_hex <- st_join(
  child_5070,
  hex_grid,
  join = st_within,
  left = FALSE
)

# Dominant persistence category helper.
mode_chr <- function(x) {
  z <- na.omit(as.character(x))
  if (length(z) == 0) return(NA_character_)
  names(sort(table(z), decreasing = TRUE))[1]
}

hex_summary <- child_hex |>
  st_drop_geometry() |>
  group_by(hex_id) |>
  summarise(
    county = mode_chr(county),
    n_locations = n(),

    providers_2023 = mean(providers_2023, na.rm = TRUE),
    providers_2026 = mean(providers_2026, na.rm = TRUE),
    provider_change = mean(provider_change, na.rm = TRUE),

    access_center_2023 = mean(access_center_2023, na.rm = TRUE),
    access_center_2026 = mean(access_center_2026, na.rm = TRUE),
    center_access_change = mean(center_access_change, na.rm = TRUE),

    access_capacity_2023 = mean(access_capacity_2023, na.rm = TRUE),
    access_capacity_2026 = mean(access_capacity_2026, na.rm = TRUE),
    capacity_access_change = mean(capacity_access_change, na.rm = TRUE),

    quality_access_2023 = mean(quality_access_2023, na.rm = TRUE),
    quality_access_2026 = mean(quality_access_2026, na.rm = TRUE),
    quality_access_change = mean(quality_access_change, na.rm = TRUE),

    high_quality_centers_2023 =
      mean(high_quality_centers_2023, na.rm = TRUE),

    high_quality_centers_2026 =
      mean(high_quality_centers_2026, na.rm = TRUE),

    persistence = mode_chr(persistence),

    .groups = "drop"
  ) |>
  filter(
    n_locations >= 10
  )

hex_public <- hex_grid |>
  inner_join(
    hex_summary,
    by = "hex_id"
  ) |>
  st_transform(4326)

# ------------------------------------------------------------------------------
# 7. COUNTY + REGIONAL PUBLIC SUMMARIES
# ------------------------------------------------------------------------------

summarise_area <- function(d, label) {

  tibble(
    county = label,
    n_locations = nrow(d),

    mean_centers_2023 =
      mean(d$providers_2023, na.rm = TRUE),

    mean_centers_2026 =
      mean(d$providers_2026, na.rm = TRUE),

    mean_capacity_2023 =
      mean(d$access_capacity_2023, na.rm = TRUE),

    mean_capacity_2026 =
      mean(d$access_capacity_2026, na.rm = TRUE),

    pct_improved_capacity =
      mean(d$capacity_access_change > 0, na.rm = TRUE),

    pct_declined_capacity =
      mean(d$capacity_access_change < 0, na.rm = TRUE),

    pct_no_center_2023 =
      mean(d$providers_2023 == 0, na.rm = TRUE),

    pct_no_center_2026 =
      mean(d$providers_2026 == 0, na.rm = TRUE),

    mean_quality_2023 =
      mean(d$quality_access_2023, na.rm = TRUE),

    mean_quality_2026 =
      mean(d$quality_access_2026, na.rm = TRUE),

    pct_improved_quality =
      mean(d$quality_access_change > 0, na.rm = TRUE),

    pct_declined_quality =
      mean(d$quality_access_change < 0, na.rm = TRUE)
  )
}

county_summary <- bind_rows(
  child |>
    st_drop_geometry() |>
    group_split(county) |>
    map_dfr(
      ~summarise_area(
        .x,
        unique(.x$county)[1]
      )
    ),

  summarise_area(
    st_drop_geometry(child),
    "All WNC counties"
  )
)

# Add corrected regional system-level headline values.
county_summary <- county_summary |>
  mutate(
    ec_providers_2023 =
      if_else(
        county == "All WNC counties",
        332L,
        NA_integer_
      ),

    ec_providers_2026 =
      if_else(
        county == "All WNC counties",
        327L,
        NA_integer_
      ),

    ec_capacity_2023 =
      if_else(
        county == "All WNC counties",
        20000,
        NA_real_
      ),

    ec_capacity_2026 =
      if_else(
        county == "All WNC counties",
        21883,
        NA_real_
      )
  )

# ------------------------------------------------------------------------------
# 8. METADATA
# ------------------------------------------------------------------------------

metadata <- tribble(
  ~field, ~value,

  "study_footprint",
  "18 Western North Carolina counties present in the 2023 baseline",

  "modeled_demand_locations",
  "41078",

  "public_hex_size",
  "5 km",

  "public_suppression_rule",
  "Hexagons with fewer than 10 modeled child-demand locations are not displayed",

  "provider_eligibility",
  paste(
    "Analytic provider universe restricted to licenses serving at least",
    "some children younger than age 5; school-age-only licenses excluded"
  ),

  "capacity_definition",
  paste(
    "Full licensed capacity at EC-serving providers; mixed-age programs",
    "may include school-age capacity and values must not be interpreted",
    "as preschool or under-5 slots"
  ),

  "travel_weights",
  "0-10 minutes = 1.0; 10-20 minutes = 0.5",

  "ec_providers_2023",
  "332",

  "ec_providers_2026",
  "327",

  "licensed_capacity_ec_serving_2023",
  "20000",

  "licensed_capacity_ec_serving_2026",
  "21883",

  "quality_rating_coverage_2023",
  percent(qcoverage$pct_with_star[qcoverage$year == 2023], accuracy = 0.1),

  "quality_rating_coverage_2026",
  percent(qcoverage$pct_with_star[qcoverage$year == 2026], accuracy = 0.1),

  "quality_caution",
  paste(
    "North Carolina's quality-rating framework changed after the 2023",
    "baseline; longitudinal quality comparisons are descriptive"
  ),

  "causal_caution",
  paste(
    "2023-2026 differences are descriptive longitudinal changes and",
    "should not be interpreted as causal effects of Hurricane Helene"
  )
)

# ------------------------------------------------------------------------------
# 9. WRITE PUBLIC FILES
# ------------------------------------------------------------------------------

st_write(
  hex_public,
  file.path(
    public_dir,
    "child_access_hex.geojson"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)

write_csv(
  county_summary,
  file.path(
    public_dir,
    "county_summary_public.csv"
  )
)

write_csv(
  metadata,
  file.path(
    public_dir,
    "public_data_metadata.csv"
  )
)

message("\n======================================================")
message("PUBLIC DATA REBUILT WITH EC-ELIGIBILITY CORRECTION")
message("======================================================")
message("Demand locations: ", nrow(child))
message("Public hexagons: ", nrow(hex_public))
message("Suppression threshold: >=10 modeled locations per hex")
message("EC providers: 332 (2023) -> 327 (2026)")
message("EC-serving licensed capacity: 20,000 -> 21,883")
message("No routing/HERE calls were made.")
