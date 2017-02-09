
#' Function to validate the blink and gaze interpolation
#' and other stuff to come.
#' @import data.table
#' @export
check_preprocessing = function(obj, what = c("gaze", "ps"), save_pdf = "") {
  checkframe = obj$raw[,  data.frame(s = sample(unique(subject), 10, replace = TRUE),
                                     t = sample(unique(trial), 10, replace = TRUE))]
  what = match.arg(what)
  
  if (save_pdf != "")
    pdf(save_pdf)
  
  for (i in 1:nrow(checkframe)) {
    print(checkframe[i,]); cat("\n")
    if (obj$smooth[.(checkframe[i, "s"], checkframe[i, "t"]), .N] <= 2)
      next
    
    if (what == "ps") {
      obj$smooth[.(checkframe[i, "s"], checkframe[i, "t"]),  
                plot(time_in_trial, ps, type = "l", 
                     main = sprintf("subject = %s,  trial = %s", 
                                    checkframe[i, "s"], checkframe[i, "t"]))]
      obj$raw[.(checkframe[i, "s"], checkframe[i, "t"]), 
              lines(time_in_trial, ps, col = 2, lty = 2)]
      
    } else if (what == "gaze") {
    obj$smooth[.(checkframe[i, "s"], checkframe[i, "t"]), 
              plot(x, y, type = "l",
                   main = sprintf("subject = %s,  trial = %s", 
                                  checkframe[i, "s"], checkframe[i, "t"]))]
      obj$raw[.(checkframe[i, "s"], checkframe[i, "t"]), 
              lines(x, y, col = 2, lty = 2)]
      obj$fix[.(checkframe[i, "s"], checkframe[i, "t"]),
                 points(x, y, col = 4, cex = 2)]
      obj$sacc[.(checkframe[i, "s"], checkframe[i, "t"]),
                  segments(x1, y1, x2, y2, col = 4, cex = 2)]
    }
    
    if (save_pdf == "")
      Sys.sleep(2)
  }
  print("done.")
}