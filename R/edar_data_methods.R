

#' @export print.edar_data
print.edar_data = function(obj, ...) {
  n = names(obj)
  cat("edar_data object   \n")
  cat("with components:\n")
  cat(n, "\n")
  cat(sprintf("y coordinates flipped: %s\n", obj$info$flipped))
  cat(sprintf("interpolation performed: %s\n", "smooth" %in% n))
  cat(sprintf("data centered and rescaled: %s\n", "cen" %in% n))
  cat(sprintf("luminance paths available: %s\n", "lumi" %in% names(obj$smooth)))
  cat(sprintf("gaze correction performed: %s\n", "corr" %in% n))
  cat(sprintf("background images available: %s\n", "img" %in% n))
  return(invisible())
}
