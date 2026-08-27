# Fastest way to publish this repository

## Option A — GitHub website (fastest; no command line)

1. Go to GitHub and sign in.
2. Click **New repository**.
3. Repository name: `wnc-childcare-access`
4. Description:
   `Geospatial analysis of child care accessibility and resilience in Western North Carolina, 2023–2026`
5. Set visibility to **Public**.
6. Do **not** initialize with a README, .gitignore, or license.
7. Create the repository.
8. On the empty repository page, choose **uploading an existing file**.
9. Unzip the portfolio ZIP on your computer.
10. Drag the contents of the `wnc-childcare-access-portfolio` folder into GitHub.
11. Commit with:
    `Initial public portfolio release`

Your link will be:

`https://github.com/YOUR-USERNAME/wnc-childcare-access`

You can use that URL in your cover letter immediately.

## Option B — Terminal

After unzipping:

```bash
cd /path/to/wnc-childcare-access-portfolio
git init
git add .
git commit -m "Initial public portfolio release"
git branch -M main
git remote add origin https://github.com/YOUR-USERNAME/wnc-childcare-access.git
git push -u origin main
```

## Recommended GitHub About settings

**Description**
> Geospatial analysis of child care accessibility and resilience in Western North Carolina, 2023–2026

**Topics**
`r` `shiny` `geospatial` `early-childhood` `child-care` `e2sfca` `rural` `program-evaluation` `data-visualization`

## What to put in the cover letter today

Use either:

> Selected research and data products, including reproducible geospatial analyses and interactive visualization work, are available in my research portfolio: [GitHub link].

or, more specifically:

> My applied quantitative portfolio includes a reproducible geospatial analysis of child care accessibility and resilience in Western North Carolina: [GitHub link].

## Later

Once the public-safe data layer is finalized:
1. add `data/child_access_public.csv`;
2. test `app.R`;
3. deploy to shinyapps.io;
4. place the Shiny URL in the GitHub repository's **Website** field;
5. update the README's Interactive Application section with the live link.
