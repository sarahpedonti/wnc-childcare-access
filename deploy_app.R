# deploy_app.R
# Run only after app.R works locally.

install.packages("rsconnect")
library(rsconnect)

# First use only:
# rsconnect::setAccountInfo(
#   name   = "YOUR_SHINYAPPS_ACCOUNT",
#   token  = "YOUR_TOKEN",
#   secret = "YOUR_SECRET"
# )
#
# IMPORTANT: Do not put token/secret values into GitHub.

rsconnect::deployApp(
  appDir = ".",
  appName = "wnc-childcare-access",
  appTitle = "Mapping Child Care Access and Resilience in Western North Carolina"
)
