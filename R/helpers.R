library(dplyr)
library(scales)

fmt_access <- function(x) {
  ifelse(is.na(x), "NA", number(x, accuracy = 0.001))
}

persistence_levels <- c(
  "Not in lowest-access decile",
  "Moved out of lowest-access decile",
  "Moved into lowest-access decile",
  "Persistently lowest-access"
)
