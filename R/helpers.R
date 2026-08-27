fmt_access <- function(x) {
  if (length(x) == 0) {
    return("—")
  }
  
  out <- scales::number(x, accuracy = 0.001)
  
  out[is.na(x)] <- "—"
  
  out
}