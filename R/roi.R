#' Region-of-interest (ROI)
#' Functions for generating and analysing ROIs.
#' 
#' ROI object
#' @export
roi = function(xy1, xy2) {
  rval = list(xleft=xy1[1], ybottom=xy1[2], xright=xy2[1], ytop=xy2[2])
  class(rval) = c(class(rval), "roi")
  rval
}

#' Register ROI to dataset
#' 
#' Function for adding a ROI to the dataset, which can then be used
#' for analysis. 
#' @import data.table
#' @describeIn roi adding ROI to dataset
#' @export
add_roi = function(obj, roi_obj) {
  stopifnot("roi" %in% class(roi_obj))
  a = quote(substitute(roi_obj))
  obj$eye_s[, newcol := 0]
  obj$eye_s[x > roi1[[1]] &  x < roi1[[3]] &  y > roi1[[2]] &  y < roi1[[4]], 
            newcol := 1]
  new_names = names(obj$eye_s)
  new_names[names(obj$eye_s) == "newcol"] <- as.character(substitute(roi_obj))
  setnames(obj$eye_s, new_names)
  obj
}
