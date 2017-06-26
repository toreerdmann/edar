## construct screen function
#' @export
construct_screen = function(imgi, xmax_screen = 1024, ymax_screen = 768) {
  stopifnot(class(imgi) == "matrix")
  border_x = (xmax_screen - ncol(imgi)) / 2
  border_y = (ymax_screen - nrow(imgi)) / 2
  rscreen = rbind(matrix(0, nrow = border_y, ncol = xmax_screen),
                  cbind(matrix(0, nrow = nrow(imgi), ncol = border_x),
                        imgi,
                        matrix(0, nrow = nrow(imgi), ncol = border_x)),
                  matrix(0, nrow = border_y, ncol = xmax_screen))
  rscreen
}
## ## plot data over background screen function
## #' @import ggplot2
## #' @importFrom EBImage display
## #' @export
## plot_on_screen = function(subdat, rscreen, use_ggplot = TRUE) {
##   xmax_screen = ncol(rscreen)
##   ymax_screen = nrow(rscreen)
##   if (use_ggplot)
##     ggplot(subdat, aes(x, y)) +
##     geom_rect(xmin = 0, xmax = xmax_screen,
##               ymin = 0, ymax = ymax_screen, fill = "white") + 
##     annotation_raster(rscreen, 0, xmax_screen, 0, ymax_screen) + 
##     geom_point() + 
##     xlim(c(0, xmax_screen)) + ylim(c(0, ymax_screen))
##   else
##     EBImage::display(EBImage::rotate(rscreen, -90), method = "raster")
## }


#' Function for transforming a point cloud.
#' 
#' Rescaling from some coordinate system, defined by argument 'from' to another
#' given by 'to'. If 'shift' is given, the rescaled cloud is moved into the center
#' of a rectangle defined by shift.
#' @import data.table
#' @export
transf = function(pointPath, from = list(bottomleft = c(-1, -1), topright = c(1, 1)),
                   to = list(bottomleft = c(0, 0), topright = c(1, 1)),
                   shift = c(0, 0), flip = FALSE) {
  ## shift and scale to [(0,0),(1,1)]
  shift1 = (abs(from[[1]] - from[[2]])) / 2
  pointPath[, `:=`(x = (x + shift1[1]) * shift1[1]/2,
                   y = (y + shift1[2]) * shift1[2]/2)]
  ## scale 
  scalefac = to[[2]] - to[[1]]
  pointPath[, `:=`(x = x * scalefac[1],
                   y = y * scalefac[2])]
  ## shift into center of screen with dimensions given by 'shift'
  if (! identical(shift, c(0, 0))) {
    xmax_screen = shift[1]
    ymax_screen = shift[2]
    border_x = (xmax_screen - scalefac[1]) / 2
    border_y = (ymax_screen - scalefac[2]) / 2
    pointPath[, `:=`(x = x + border_x,
                     y = y + border_y)]
    if (flip) {
      mid_y = ymax_screen / 2
      pointPath[, y := (-1 * (y - mid_y)) + mid_y]
    }
  }
  
  pointPath
}
