# =============================================================================
# RECOMPUTE QUALITY-WEIGHTED ACCESS + PREPARE PUBLIC SHINY DATA
# FINAL RUTHERFORD-REPAIRED 18-COUNTY VERSION
# =============================================================================
#
# FINAL DEMAND SURFACE
# --------------------
# Original fixed demand surface:   37,982
# Added Rutherford locations:       3,096
# Final repaired surface:          41,078
#
# QUALITY METRIC
# --------------
# The quality-weighted capacity-access measure preserves the original
# analytic definition:
#
#   provider capacity ratio =
#       licensed capacity / geographically weighted competing demand
#
#   provider quality weight =
#       star rating / 5
#
#   quality-weighted capacity ratio =
#       provider capacity ratio * quality weight
#
#   child quality-weighted access =
#       sum(quality-weighted capacity ratio * travel weight)
#
# Unrated providers contribute zero to the quality-weighted measure.
#
# PUBLIC OUTPUTS
# --------------
# data/child_access_hex.geojson
# data/county_summary_public.csv
# data/public_data_metadata.csv
#
# ANALYTIC OUTPUTS
# ----------------
# 10_Rutherford_Repair/18county_e2sfca_recomputed/
#   QUALITY_ACCESS_CHILD_LEVEL_18COUNTY_RECOMPUTED.csv
#   QUALITY_ACCESS_HEADLINE_18COUNTY_RECOMPUTED.csv
#   QUALITY_ACCESS_DISTRIBUTION_18COUNTY_RECOMPUTED.csv
#
# NO ROUTING/API CALLS ARE MADE.
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

inputs_dir <- file.path(
  project_dir,
  "05_Processed_Inputs"
)

repair_dir <- file.path(
  project_dir,
  "10_Rutherford_Repair"
)

recomputed_dir <- file.path(
  repair_dir,
  "18county_e2sfca_recomputed"
)

dir.create(
  recomputed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Public app output folder.
# Run this script from the Shiny project directory.

public_dir <- "data"

dir.create(
  public_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 3. REQUIRED INPUT FILES
# =============================================================================

demand_file <- file.path(
  repair_dir,
  "sampled_residences_wgs84_18COUNTY_REPAIRED.gpkg"
)

rings23_file <- file.path(
  recomputed_dir,
  "provider_rings_2023_POLYGON_CLEAN.rds"
)

rings26_file <- file.path(
  recomputed_dir,
  "provider_rings_2026_POLYGON_CLEAN.rds"
)

access23_file <- file.path(
  recomputed_dir,
  "child_access_2023_18COUNTY_RECOMPUTED.csv"
)

access26_file <- file.path(
  recomputed_dir,
  "child_access_2026_18COUNTY_RECOMPUTED.csv"
)

change_file <- file.path(
  recomputed_dir,
  "child_level_access_change_18COUNTY_RECOMPUTED.csv"
)

provider23_file <- file.path(
  inputs_dir,
  "newDHTcenters_geocodio_orig_FINAL_7.7.23.csv"
)

provider26_quality_file <- file.path(
  inputs_dir,
  "dcdee_2026_wnc_centers_enriched_CORRECTED.csv"
)


# =============================================================================
# 4. VERIFY REQUIRED FILES
# =============================================================================

required_files <- c(
  demand_file,
  rings23_file,
  rings26_file,
  access23_file,
  access26_file,
  change_file,
  provider23_file,
  provider26_quality_file
)

missing_files <- required_files[
  !file.exists(
    path.expand(required_files)
  )
]

if (length(missing_files) > 0) {
  
  stop(
    "\nThese required files were not found:\n\n",
    paste(
      missing_files,
      collapse = "\n"
    ),
    "\n\nDo not continue until these paths are resolved."
  )
}


message(
  "\nAll required repaired-analysis files found."
)


# =============================================================================
# 5. HELPERS
# =============================================================================

clean_license <- function(x) {
  
  z <- toupper(
    gsub(
      "[^A-Z0-9]",
      "",
      as.character(x)
    )
  )
  
  numeric_only <- grepl(
    "^[0-9]+$",
    z
  )
  
  z[numeric_only] <- as.character(
    as.numeric(
      z[numeric_only]
    )
  )
  
  z[z == ""] <- NA_character_
  
  z
}


parse_2023_star <- function(x) {
  
  suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
}


parse_2026_star <- function(x) {
  
  x <- toupper(
    as.character(x)
  )
  
  case_when(
    
    str_detect(
      x,
      "FIVE STAR"
    ) ~ 5,
    
    str_detect(
      x,
      "FOUR STAR"
    ) ~ 4,
    
    str_detect(
      x,
      "THREE STAR"
    ) ~ 3,
    
    str_detect(
      x,
      "TWO STAR"
    ) ~ 2,
    
    str_detect(
      x,
      "ONE STAR"
    ) ~ 1,
    
    TRUE ~
      NA_real_
  )
}


# =============================================================================
# 6. READ REPAIRED DEMAND SURFACE
# =============================================================================

children <- st_read(
  path.expand(
    demand_file
  ),
  quiet = TRUE
) |>
  st_transform(
    4326
  ) |>
  arrange(
    child_row_id
  )


stopifnot(
  nrow(children) == 41078
)

stopifnot(
  n_distinct(
    children$child_row_id
  ) == 41078
)


message(
  "Repaired demand surface loaded: ",
  format(
    nrow(children),
    big.mark = ","
  ),
  " locations."
)


# Check repaired Rutherford additions.

stopifnot(
  sum(
    children$child_row_id > 37982
  ) == 3096
)


message(
  "Rutherford repaired locations confirmed: 3,096."
)


# =============================================================================
# 7. READ CLEANED PROVIDER RINGS
# =============================================================================

rings23 <- readRDS(
  rings23_file
)

rings26 <- readRDS(
  rings26_file
)


message(
  "2023 cleaned provider rings: ",
  nrow(rings23)
)

message(
  "2026 cleaned provider rings: ",
  nrow(rings26)
)


# Expected two rings per provider.

stopifnot(
  nrow(rings23) == 676
)

stopifnot(
  nrow(rings26) == 742
)


# =============================================================================
# 8. QUALITY TABLE — 2023
# =============================================================================

providers23_raw <- read_csv(
  provider23_file,
  show_col_types = FALSE
)


q23 <- providers23_raw |>
  transmute(
    
    license_id =
      clean_license(
        License.Number
      ),
    
    star =
      parse_2023_star(
        stars
      )
  ) |>
  filter(
    !is.na(
      license_id
    )
  ) |>
  distinct(
    license_id,
    .keep_all = TRUE
  )


message(
  "\n2023 quality records: ",
  nrow(q23)
)

message(
  "2023 providers with parsed star rating: ",
  sum(
    !is.na(
      q23$star
    )
  )
)


# =============================================================================
# 9. QUALITY TABLE — 2026
# =============================================================================

providers26_quality_raw <- read_csv(
  provider26_quality_file,
  show_col_types = FALSE
)


q26 <- providers26_quality_raw |>
  transmute(
    
    license_id =
      clean_license(
        license_number
      ),
    
    star =
      parse_2026_star(
        current_license_type
      )
  ) |>
  filter(
    !is.na(
      license_id
    )
  ) |>
  distinct(
    license_id,
    .keep_all = TRUE
  )


message(
  "\n2026 quality records: ",
  nrow(q26)
)

message(
  "2026 providers with parsed star rating: ",
  sum(
    !is.na(
      q26$star
    )
  )
)


# =============================================================================
# 10. QUALITY-ACCESS FUNCTION
# =============================================================================
#
# This deliberately reproduces the prior quality-access definition.
#
# Provider denominators are recomputed using ALL 41,078 demand locations.
#
# =============================================================================

quality_access <- function(
    rings,
    qtable,
    children_sf,
    year_label
) {
  
  
  # ---------------------------------------------------------------------------
  # Join ratings to provider catchments.
  # ---------------------------------------------------------------------------
  
  rq <- rings |>
    mutate(
      license_id =
        clean_license(
          license_id
        )
    ) |>
    left_join(
      qtable,
      by = "license_id"
    ) |>
    mutate(
      
      rated =
        !is.na(
          star
        ),
      
      high_quality_4plus =
        !is.na(star) &
        star >= 4,
      
      five_star =
        !is.na(star) &
        star == 5,
      
      quality_weight =
        if_else(
          !is.na(star),
          star / 5,
          NA_real_
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Match child-demand points to provider rings.
  # ---------------------------------------------------------------------------
  
  child_work <- children_sf |>
    st_transform(
      st_crs(rq)
    )
  
  
  memberships <- st_join(
    
    child_work |>
      select(
        child_row_id
      ),
    
    rq |>
      select(
        license_id,
        facility_name,
        county,
        capacity,
        year,
        band,
        weight,
        star,
        rated,
        high_quality_4plus,
        five_star,
        quality_weight
      ),
    
    join =
      st_within,
    
    left =
      FALSE
    
  ) |>
    st_drop_geometry()
  
  
  message(
    "\n",
    year_label,
    " child-provider-band memberships: ",
    format(
      nrow(memberships),
      big.mark = ","
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Provider denominators.
  # ---------------------------------------------------------------------------
  
  provider_ratio <- memberships |>
    group_by(
      license_id,
      capacity,
      star,
      rated,
      high_quality_4plus,
      five_star,
      quality_weight
    ) |>
    summarise(
      
      weighted_child_demand =
        sum(
          weight,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    ) |>
    mutate(
      
      ratio_capacity =
        if_else(
          
          weighted_child_demand > 0 &
            !is.na(
              capacity
            ),
          
          as.numeric(
            capacity
          ) /
            weighted_child_demand,
          
          NA_real_
        ),
      
      
      ratio_hq_capacity =
        if_else(
          
          high_quality_4plus &
            !is.na(
              ratio_capacity
            ),
          
          ratio_capacity,
          
          0
        ),
      
      
      ratio_5star_capacity =
        if_else(
          
          five_star &
            !is.na(
              ratio_capacity
            ),
          
          ratio_capacity,
          
          0
        ),
      
      
      ratio_quality_weighted_capacity =
        if_else(
          
          !is.na(
            quality_weight
          ) &
            !is.na(
              ratio_capacity
            ),
          
          ratio_capacity *
            quality_weight,
          
          0
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Child-level quality access.
  # ---------------------------------------------------------------------------
  
  child_nonzero <- memberships |>
    left_join(
      
      provider_ratio |>
        select(
          license_id,
          ratio_capacity,
          ratio_hq_capacity,
          ratio_5star_capacity,
          ratio_quality_weighted_capacity
        ),
      
      by =
        "license_id"
    ) |>
    group_by(
      child_row_id
    ) |>
    summarise(
      
      centers_within_20 =
        n_distinct(
          license_id
        ),
      
      rated_centers_within_20 =
        n_distinct(
          license_id[
            rated
          ]
        ),
      
      high_quality_centers_within_20 =
        n_distinct(
          license_id[
            high_quality_4plus
          ]
        ),
      
      five_star_centers_within_20 =
        n_distinct(
          license_id[
            five_star
          ]
        ),
      
      capacity_access =
        sum(
          ratio_capacity *
            weight,
          na.rm = TRUE
        ),
      
      high_quality_capacity_access =
        sum(
          ratio_hq_capacity *
            weight,
          na.rm = TRUE
        ),
      
      five_star_capacity_access =
        sum(
          ratio_5star_capacity *
            weight,
          na.rm = TRUE
        ),
      
      quality_weighted_capacity_access =
        sum(
          ratio_quality_weighted_capacity *
            weight,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    )
  
  
  # ---------------------------------------------------------------------------
  # Restore zero-access locations.
  # ---------------------------------------------------------------------------
  
  child <- children_sf |>
    st_drop_geometry() |>
    select(
      child_row_id
    ) |>
    left_join(
      child_nonzero,
      by = "child_row_id"
    ) |>
    mutate(
      
      across(
        
        c(
          centers_within_20,
          rated_centers_within_20,
          high_quality_centers_within_20,
          five_star_centers_within_20,
          capacity_access,
          high_quality_capacity_access,
          five_star_capacity_access,
          quality_weighted_capacity_access
        ),
        
        ~replace_na(
          .x,
          0
        )
      ),
      
      
      pct_reachable_rated_4plus =
        if_else(
          
          rated_centers_within_20 > 0,
          
          high_quality_centers_within_20 /
            rated_centers_within_20,
          
          NA_real_
        ),
      
      
      pct_reachable_rated_5star =
        if_else(
          
          rated_centers_within_20 > 0,
          
          five_star_centers_within_20 /
            rated_centers_within_20,
          
          NA_real_
        ),
      
      
      year =
        as.integer(
          year_label
        )
    )
  
  
  stopifnot(
    nrow(child) == 41078
  )
  
  
  child
}


# =============================================================================
# 11. RECOMPUTE QUALITY ACCESS
# =============================================================================

message(
  "\n============================================================"
)

message(
  "RECOMPUTING 2023 QUALITY-WEIGHTED ACCESS"
)

message(
  "============================================================"
)


qa23 <- quality_access(
  rings =
    rings23,
  qtable =
    q23,
  children_sf =
    children,
  year_label =
    2023
)


message(
  "\n============================================================"
)

message(
  "RECOMPUTING 2026 QUALITY-WEIGHTED ACCESS"
)

message(
  "============================================================"
)


qa26 <- quality_access(
  rings =
    rings26,
  qtable =
    q26,
  children_sf =
    children,
  year_label =
    2026
)


# =============================================================================
# 12. LONGITUDINAL QUALITY COMPARISON
# =============================================================================

quality_comp <- qa23 |>
  select(
    -year
  ) |>
  rename_with(
    ~paste0(
      .x,
      "_2023"
    ),
    -child_row_id
  ) |>
  left_join(
    
    qa26 |>
      select(
        -year
      ) |>
      rename_with(
        ~paste0(
          .x,
          "_2026"
        ),
        -child_row_id
      ),
    
    by =
      "child_row_id"
  ) |>
  mutate(
    
    high_quality_centers_change =
      high_quality_centers_within_20_2026 -
      high_quality_centers_within_20_2023,
    
    
    five_star_centers_change =
      five_star_centers_within_20_2026 -
      five_star_centers_within_20_2023,
    
    
    high_quality_capacity_access_change =
      high_quality_capacity_access_2026 -
      high_quality_capacity_access_2023,
    
    
    quality_weighted_capacity_access_change =
      quality_weighted_capacity_access_2026 -
      quality_weighted_capacity_access_2023
  )


stopifnot(
  nrow(
    quality_comp
  ) == 41078
)


# =============================================================================
# 13. SAVE QUALITY CHILD-LEVEL FILE
# =============================================================================

quality_child_file <- file.path(
  recomputed_dir,
  "QUALITY_ACCESS_CHILD_LEVEL_18COUNTY_RECOMPUTED.csv"
)


write_csv(
  quality_comp,
  quality_child_file
)


message(
  "\nSaved quality child-level results:"
)

message(
  quality_child_file
)


# =============================================================================
# 14. QUALITY HEADLINE SUMMARY
# =============================================================================

quality_headline <- quality_comp |>
  summarise(
    
    n_child_points =
      n(),
    
    
    mean_hq_centers_2023 =
      mean(
        high_quality_centers_within_20_2023
      ),
    
    mean_hq_centers_2026 =
      mean(
        high_quality_centers_within_20_2026
      ),
    
    
    median_hq_centers_2023 =
      median(
        high_quality_centers_within_20_2023
      ),
    
    median_hq_centers_2026 =
      median(
        high_quality_centers_within_20_2026
      ),
    
    
    pct_locations_hq_choice_improved =
      mean(
        high_quality_centers_change > 0
      ),
    
    pct_locations_hq_choice_declined =
      mean(
        high_quality_centers_change < 0
      ),
    
    
    mean_hq_capacity_access_2023 =
      mean(
        high_quality_capacity_access_2023
      ),
    
    mean_hq_capacity_access_2026 =
      mean(
        high_quality_capacity_access_2026
      ),
    
    
    mean_quality_weighted_capacity_2023 =
      mean(
        quality_weighted_capacity_access_2023
      ),
    
    mean_quality_weighted_capacity_2026 =
      mean(
        quality_weighted_capacity_access_2026
      ),
    
    
    median_quality_weighted_capacity_2023 =
      median(
        quality_weighted_capacity_access_2023
      ),
    
    median_quality_weighted_capacity_2026 =
      median(
        quality_weighted_capacity_access_2026
      ),
    
    
    pct_locations_quality_weighted_improved =
      mean(
        quality_weighted_capacity_access_change >
          0
      ),
    
    pct_locations_quality_weighted_declined =
      mean(
        quality_weighted_capacity_access_change <
          0
      )
  )


write_csv(
  quality_headline,
  file.path(
    recomputed_dir,
    "QUALITY_ACCESS_HEADLINE_18COUNTY_RECOMPUTED.csv"
  )
)


# =============================================================================
# 15. QUALITY DISTRIBUTION SUMMARY
# =============================================================================

quality_distribution <- bind_rows(
  
  quality_comp |>
    summarise(
      
      metric =
        "Quality-weighted capacity access, 2023",
      
      p10 =
        quantile(
          quality_weighted_capacity_access_2023,
          0.10
        ),
      
      p25 =
        quantile(
          quality_weighted_capacity_access_2023,
          0.25
        ),
      
      median =
        median(
          quality_weighted_capacity_access_2023
        ),
      
      p75 =
        quantile(
          quality_weighted_capacity_access_2023,
          0.75
        ),
      
      p90 =
        quantile(
          quality_weighted_capacity_access_2023,
          0.90
        )
    ),
  
  
  quality_comp |>
    summarise(
      
      metric =
        "Quality-weighted capacity access, 2026",
      
      p10 =
        quantile(
          quality_weighted_capacity_access_2026,
          0.10
        ),
      
      p25 =
        quantile(
          quality_weighted_capacity_access_2026,
          0.25
        ),
      
      median =
        median(
          quality_weighted_capacity_access_2026
        ),
      
      p75 =
        quantile(
          quality_weighted_capacity_access_2026,
          0.75
        ),
      
      p90 =
        quantile(
          quality_weighted_capacity_access_2026,
          0.90
        )
    ),
  
  
  quality_comp |>
    summarise(
      
      metric =
        "Quality-weighted capacity access change",
      
      p10 =
        quantile(
          quality_weighted_capacity_access_change,
          0.10
        ),
      
      p25 =
        quantile(
          quality_weighted_capacity_access_change,
          0.25
        ),
      
      median =
        median(
          quality_weighted_capacity_access_change
        ),
      
      p75 =
        quantile(
          quality_weighted_capacity_access_change,
          0.75
        ),
      
      p90 =
        quantile(
          quality_weighted_capacity_access_change,
          0.90
        )
    )
)


write_csv(
  quality_distribution,
  file.path(
    recomputed_dir,
    "QUALITY_ACCESS_DISTRIBUTION_18COUNTY_RECOMPUTED.csv"
  )
)


# =============================================================================
# 16. READ CAPACITY-WEIGHTED REPAIRED RESULTS
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


stopifnot(
  nrow(a23) == 41078,
  nrow(a26) == 41078,
  nrow(chg) == 41078
)


# =============================================================================
# 17. STUDY COUNTIES
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
# 18. COUNTY BOUNDARIES
# =============================================================================

counties <- tigris::counties(
  state =
    "NC",
  cb =
    TRUE,
  year =
    2024,
  class =
    "sf"
) |>
  filter(
    NAME %in%
      wnc_counties
  ) |>
  select(
    county =
      NAME
  ) |>
  st_make_valid() |>
  st_transform(
    4326
  )


stopifnot(
  nrow(counties) == 18
)


# =============================================================================
# 19. ASSIGN COUNTY TO DEMAND LOCATIONS
# =============================================================================

children <- children |>
  select(
    -any_of(
      c(
        "county",
        "County",
        "COUNTY"
      )
    )
  )


children <- st_join(
  children,
  counties,
  join =
    st_within,
  left =
    TRUE
)


missing_county <- which(
  is.na(
    children$county
  )
)


if (
  length(
    missing_county
  ) > 0
) {
  
  nearest <- st_nearest_feature(
    
    children[
      missing_county,
    ],
    
    counties
  )
  
  
  children$county[
    missing_county
  ] <-
    counties$county[
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


message(
  "\nRutherford demand locations assigned to county: ",
  sum(
    children$county ==
      "Rutherford"
  )
)


# =============================================================================
# 20. COMBINE ALL CAPACITY + QUALITY RESULTS
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
        
        access_capacity_2023 =
          access_capacity
      ),
    
    by =
      "child_row_id"
  ) |>
  
  left_join(
    
    a26 |>
      select(
        
        child_row_id,
        
        providers_2026 =
          providers_within_20,
        
        access_capacity_2026 =
          access_capacity
      ),
    
    by =
      "child_row_id"
  ) |>
  
  left_join(
    
    chg |>
      select(
        child_row_id,
        capacity_access_change
      ),
    
    by =
      "child_row_id"
  ) |>
  
  left_join(
    
    quality_comp |>
      select(
        
        child_row_id,
        
        quality_weighted_capacity_access_2023,
        
        quality_weighted_capacity_access_2026,
        
        quality_weighted_capacity_access_change
      ),
    
    by =
      "child_row_id"
  )


stopifnot(
  nrow(
    child_access
  ) == 41078
)


# =============================================================================
# 21. PERSISTENT LOW ACCESS
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
      access_capacity_2023 <=
      cut23,
    
    low_2026 =
      access_capacity_2026 <=
      cut26,
    
    
    persistence =
      case_when(
        
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
# 22. COUNTY SUMMARY
# =============================================================================

summarise_area <- function(
    x,
    area_name
) {
  
  tibble(
    
    county =
      area_name,
    
    n_locations =
      nrow(x),
    
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
        x$capacity_access_change >
          0,
        na.rm = TRUE
      ),
    
    pct_no_center_2023 =
      mean(
        x$providers_2023 ==
          0,
        na.rm = TRUE
      ),
    
    pct_no_center_2026 =
      mean(
        x$providers_2026 ==
          0,
        na.rm = TRUE
      ),
    
    mean_quality_2023 =
      mean(
        x$quality_weighted_capacity_access_2023,
        na.rm = TRUE
      ),
    
    mean_quality_2026 =
      mean(
        x$quality_weighted_capacity_access_2026,
        na.rm = TRUE
      ),
    
    mean_quality_change =
      mean(
        x$quality_weighted_capacity_access_change,
        na.rm = TRUE
      ),
    
    pct_improved_quality =
      mean(
        x$quality_weighted_capacity_access_change >
          0,
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
  nrow(
    summary_out
  ) == 19
)


write_csv(
  summary_out,
  file.path(
    public_dir,
    "county_summary_public.csv"
  )
)


# =============================================================================
# 23. CREATE 5-KM HEX GRID
# =============================================================================

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


hex_grid <- st_make_grid(
  
  wnc_outline,
  
  cellsize =
    5000,
  
  square =
    FALSE
  
) |>
  st_as_sf() |>
  mutate(
    hex_id =
      row_number()
  )


hex_grid <- hex_grid[
  lengths(
    st_intersects(
      hex_grid,
      wnc_outline
    )
  ) > 0,
]


# =============================================================================
# 24. ASSIGN DEMAND LOCATIONS TO HEXES
# =============================================================================

child_hex <- st_join(
  
  children_proj,
  
  hex_grid |>
    select(
      hex_id
    ),
  
  join =
    st_within,
  
  left =
    FALSE
)


stopifnot(
  nrow(
    child_hex
  ) == 41078
)


# =============================================================================
# 25. HEX-LEVEL CAPACITY + QUALITY SUMMARY
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
    
    
    quality_access_2023 =
      mean(
        quality_weighted_capacity_access_2023,
        na.rm = TRUE
      ),
    
    quality_access_2026 =
      mean(
        quality_weighted_capacity_access_2026,
        na.rm = TRUE
      ),
    
    quality_access_change =
      mean(
        quality_weighted_capacity_access_change,
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
    
    
    pct_capacity_improved =
      mean(
        capacity_access_change >
          0,
        na.rm = TRUE
      ),
    
    pct_quality_improved =
      mean(
        quality_weighted_capacity_access_change >
          0,
        na.rm = TRUE
      ),
    
    
    pct_persistent_low =
      mean(
        persistence ==
          "Persistently lowest-access",
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


# =============================================================================
# 26. DOMINANT PERSISTENCE CATEGORY
# =============================================================================

dominant_persistence <- child_hex |>
  st_drop_geometry() |>
  count(
    hex_id,
    persistence,
    name =
      "n"
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
# 27. JOIN HEX RESULTS
# =============================================================================

hex_public <- hex_grid |>
  inner_join(
    hex_stats,
    by =
      "hex_id"
  ) |>
  left_join(
    dominant_persistence,
    by =
      "hex_id"
  )


# =============================================================================
# 28. ASSIGN DISPLAY COUNTY
# =============================================================================

hex_points <- st_point_on_surface(
  hex_public
)


hex_county <- st_join(
  
  hex_points,
  
  counties_proj,
  
  join =
    st_within,
  
  left =
    TRUE
  
) |>
  st_drop_geometry() |>
  select(
    hex_id,
    county
  )


hex_public <- hex_public |>
  left_join(
    hex_county,
    by =
      "hex_id"
  )


# =============================================================================
# 29. PRIVACY SUPPRESSION
# =============================================================================

MIN_PUBLIC_LOCATIONS <- 10


hex_public <- hex_public |>
  filter(
    n_locations >=
      MIN_PUBLIC_LOCATIONS
  )


message(
  "\nPublic hexagons after suppression: ",
  nrow(
    hex_public
  )
)


# =============================================================================
# 30. RUTHERFORD QA
# =============================================================================

rutherford_hexes <- hex_public |>
  st_drop_geometry() |>
  filter(
    county ==
      "Rutherford"
  )


stopifnot(
  nrow(
    rutherford_hexes
  ) > 0
)


stopifnot(
  any(
    !is.na(
      rutherford_hexes$quality_access_2023
    )
  )
)


stopifnot(
  any(
    !is.na(
      rutherford_hexes$quality_access_2026
    )
  )
)


message(
  "Public Rutherford hexagons: ",
  nrow(
    rutherford_hexes
  )
)

message(
  "Rutherford quality-weighted values successfully included."
)


# =============================================================================
# 31. WRITE GEOJSON
# =============================================================================

hex_public <- hex_public |>
  st_transform(
    4326
  )


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
  driver =
    "GeoJSON",
  quiet =
    TRUE
)


# =============================================================================
# 32. PUBLIC METADATA
# =============================================================================

metadata <- tibble(
  
  item = c(
    "analysis_version",
    "demand_surface_n",
    "original_demand_n",
    "rutherford_added_n",
    "rutherford_source",
    "quality_metric",
    "hex_cellsize_m",
    "minimum_locations_per_public_hex"
  ),
  
  value = c(
    "18-county Rutherford-repaired capacity and quality analysis",
    "41078",
    "37982",
    "3096",
    "Rutherford E911 address points, 2023-09-07",
    "Capacity-weighted E2SFCA multiplied by provider star rating / 5",
    "5000",
    as.character(
      MIN_PUBLIC_LOCATIONS
    )
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
# 33. FINAL QA
# =============================================================================

message(
  "\n============================================================"
)

message(
  "QUALITY RECOMPUTATION + PUBLIC DATA PREP COMPLETE"
)

message(
  "============================================================"
)

message(
  "Demand locations: 41,078"
)

message(
  "Rutherford repaired locations: 3,096"
)

message(
  "Public hexagons: ",
  nrow(
    hex_public
  )
)

message(
  "Rutherford public hexagons: ",
  nrow(
    rutherford_hexes
  )
)

message(
  "\nRegional mean quality-weighted access:"
)

message(
  "2023: ",
  round(
    quality_headline$mean_quality_weighted_capacity_2023,
    4
  )
)

message(
  "2026: ",
  round(
    quality_headline$mean_quality_weighted_capacity_2026,
    4
  )
)

message(
  "Percent improved: ",
  scales::percent(
    quality_headline$pct_locations_quality_weighted_improved,
    accuracy = 0.1
  )
)

message(
  "\nWritten:"
)

message(
  quality_child_file
)

message(
  file.path(
    public_dir,
    "child_access_hex.geojson"
  )
)

message(
  file.path(
    public_dir,
    "county_summary_public.csv"
  )
)

message(
  file.path(
    public_dir,
    "public_data_metadata.csv"
  )
)

message(
  "\nNo HERE routing/API calls were made."
)

message(
  "No exact residential locations were exported."
)

message(
  "============================================================\n"
)