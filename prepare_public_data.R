# Prepare PUBLIC-SAFE data for the portfolio app.
# This script DOES NOT rerun routing.

library(tidyverse)
library(sf)

project_dir <- "PATH/TO/WNC_Helene_ECRQ_Coauthor_Package_2026-08-19"
results_dir <- file.path(project_dir, "04_Results")
inputs_dir  <- file.path(project_dir, "05_Processed_Inputs")

a23 <- read_csv(file.path(results_dir, "child_access_2023_CORRECTED.csv"), show_col_types = FALSE)
a26 <- read_csv(file.path(results_dir, "child_access_2026_CORRECTED.csv"), show_col_types = FALSE)
chg <- read_csv(file.path(results_dir, "child_level_access_change_CORRECTED.csv"), show_col_types = FALSE)
qual <- read_csv(file.path(results_dir, "QUALITY_ACCESS_CHILD_LEVEL.csv"), show_col_types = FALSE)

children <- st_read(file.path(inputs_dir, "sampled_residences_wgs84.shp"), quiet = TRUE) |>
  mutate(child_row_id = row_number())

stopifnot(nrow(children) == 37982)

# child_row_id is used because the legacy UniquID is not geographically unique.
xy <- st_coordinates(children)

public <- tibble(
  child_row_id = children$child_row_id,
  # Generalize before release; review disclosure risk before publishing.
  longitude_public = round(xy[,1], 2),
  latitude_public  = round(xy[,2], 2)
) |>
  left_join(a23 |> select(child_row_id, access_capacity_2023 = access_capacity), by="child_row_id") |>
  left_join(a26 |> select(child_row_id, access_capacity_2026 = access_capacity), by="child_row_id") |>
  left_join(chg |> select(child_row_id, capacity_access_change), by="child_row_id") |>
  left_join(qual |> select(
    child_row_id,
    quality_access_2023 = quality_weighted_capacity_access_2023,
    quality_access_2026 = quality_weighted_capacity_access_2026,
    quality_access_change = quality_weighted_capacity_access_change
  ), by="child_row_id")

cut23 <- quantile(public$access_capacity_2023, .10, na.rm=TRUE)
cut26 <- quantile(public$access_capacity_2026, .10, na.rm=TRUE)

public <- public |>
  mutate(
    low_2023 = access_capacity_2023 <= cut23,
    low_2026 = access_capacity_2026 <= cut26,
    persistence = case_when(
      low_2023 & low_2026 ~ "Persistently lowest-access",
      low_2023 & !low_2026 ~ "Moved out of lowest-access decile",
      !low_2023 & low_2026 ~ "Moved into lowest-access decile",
      TRUE ~ "Not in lowest-access decile"
    )
  ) |>
  select(-low_2023, -low_2026)

# NEXT: attach county from a public county polygon layer.
# Do not write the CSV until county and disclosure-safe coordinates are verified.
# write_csv(public, "data/child_access_public.csv")
