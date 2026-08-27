# Mapping Child Care Access and Resilience in Western North Carolina

**Geospatial research portfolio | R • sf • E2SFCA • Shiny • data visualization**

This project examines changes in geographic access to licensed center-based child care across **18 Western North Carolina counties between 2023 and 2026**, with particular attention to uneven recovery following Hurricane Helene.

> **Project status:** Analysis and reproducible mapping workflow are complete. A public-safe interactive Shiny version is being prepared from derived analytic outputs.

## Research questions

1. How did geographic access to licensed center-based child care change between 2023 and 2026?
2. Where did capacity-weighted access improve or decline?
3. Which communities remained persistently low-access?
4. How do patterns change when provider capacity and formal quality are incorporated?

## Analytic approach

The primary outcome is a **capacity-weighted enhanced two-step floating catchment area (E2SFCA)** accessibility measure.

The analysis:

- uses a fixed residential child-demand surface for longitudinal comparison;
- models access within **0–10 and 10–20 minute travel-time bands**;
- applies distance-decay weights of **1.0 and 0.5**;
- accounts for licensed center capacity and geographically weighted competing demand;
- compares 2023 and 2026 access using a shared demand surface;
- identifies communities that remained in the bottom decile of accessibility across both years.

A secondary quality-weighted measure incorporates provider star ratings. Because North Carolina's quality-rating framework changed after the 2023 baseline, those longitudinal quality comparisons are interpreted descriptively.

## Why this matters

Child care is essential social and economic infrastructure. In rural and disaster-affected communities, changes in center availability, licensed capacity, travel burden, and uneven recovery can substantially affect whether families can realistically reach care.

This project is designed not only to estimate geographic accessibility but also to make spatial inequities visible and actionable for policymakers, funders, and community partners.

## Reproducibility

The repository separates:

- **analysis logic and visualization code**;
- **derived public-facing data products**;
- **raw/restricted or potentially identifying source data**.

The public Shiny app does **not** rerun routing or expose raw residential locations, routing matrices, API credentials, or restricted administrative files.

## Repository contents

```text
app.R                     # Shiny application
prepare_public_data.R     # creates disclosure-safe app data from final results
R/helpers.R               # reusable display helpers
data/README.md             # public-data and disclosure rules
.gitignore                 # excludes raw/restricted data and secrets
LICENSE
```

## Interactive application

A public Shiny version is in development. Planned views include:

- 2023 capacity-weighted accessibility
- 2026 capacity-weighted accessibility
- change in accessibility
- quality-weighted accessibility
- persistent lowest-access communities
- county-level filtering and summary indicators

## Interpretation

The 2023–2026 comparisons are **descriptive longitudinal analyses**. They should not be interpreted as causal estimates of Hurricane Helene because provider-specific disaster exposure, funding, and timing are not fully observed.

## Author

**Sarah F. Pedonti, Ph.D.**

Applied developmental and education researcher working at the intersection of early childhood systems, family well-being, rural education, program evaluation, quantitative methods, and research-to-practice translation.
