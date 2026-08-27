# Mapping Child Care Access and Resilience in Western North Carolina

**R • Shiny • sf • Leaflet • E2SFCA • geospatial program evaluation**

This interactive portfolio project examines geographic access to licensed center-based child care across 18 Western North Carolina counties in 2023 and 2026.

## Live app

The public Shiny application will be linked here after deployment.

## Public-facing design

The application displays a **5-km hexagonal aggregation** of derived child-care-accessibility results. Exact residential demand locations are never exported to the public repository.

The app includes:

- capacity-weighted accessibility in 2023 and 2026;
- change in capacity-weighted accessibility;
- secondary quality-weighted accessibility;
- persistent low-access communities;
- county filtering;
- headline regional and county statistics;
- methods and interpretation guidance.

## Analysis

The primary accessibility measure is a capacity-weighted enhanced two-step floating catchment area (E2SFCA) score using:

- 0–10 minute travel band, weight 1.0;
- 10–20 minute travel band, weight 0.5;
- licensed provider capacity;
- geographically weighted competing demand;
- one fixed child-demand surface across both years.

The public app visualizes already-derived results and does not rerun routing.

## Reproduce the public app

1. Place this repository on your computer.
2. Edit `project_dir` in `prepare_public_data.R` if necessary.
3. Run:

```r
source("prepare_public_data.R")
```

4. Confirm these files were created:

```text
data/child_access_hex.geojson
data/county_summary_public.csv
```

5. Install app packages:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "leaflet",
  "dplyr", "readr", "sf", "scales", "htmltools"
))
```

6. Run:

```r
shiny::runApp()
```

7. When ready, deploy using `deploy_app.R`.

## Interpretation

The 2023–2026 comparisons are descriptive longitudinal analyses rather than causal estimates of Hurricane Helene. Quality-weighted comparisons are secondary because North Carolina's quality-rating framework changed after the 2023 baseline.

## Researcher

**Sarah F. Pedonti, Ph.D.**
