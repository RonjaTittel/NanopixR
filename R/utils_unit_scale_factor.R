# Getting the factor according to the scale for unit conversion
.unit_scale_factor <- function(unit) {
  if (is.na(unit) || !nzchar(unit)) return(NA_real_)
  u <- tolower(unit)
  if (u == "nm")  return(1e-6)
  if (u %in% c("um", "\u00b5m")) return(1e-3)
  if (u == "mm")  return(1)
  return(NA_real_)
}
