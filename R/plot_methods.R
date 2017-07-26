#' Plotting functions
#' @import data.table
#' @import ggplot2
#' @export plot.edar_data
plot.edar_data = function(obj, what = c("gaze", "ps", "cen"), type = c("1", "2", "3", "4", "5"), 
                          subs = 1:.N, 
                          show_msgs = "", condition = NULL,
                          limit_screen = TRUE, raw = FALSE, ...) {
  
  if (show_msgs != "")
    if (obj$msg[eval(substitute(subs))][text %in% show_msgs, .N] == 0)
      stop(sprintf("Message: %s not found.\nAvailable messages are: \n%s", show_msgs,
                   dat$msg[eval(substitute(subs))][ , paste(unique(text), collapse = ", ")]))
  
  what = match.arg(what)
  type = match.arg(type)
  
  if (limit_screen) {
    yli = ggplot2::ylim(c( - 50, obj$info$resolution$y[1] + 50))
    xli = ggplot2::xlim(c( - 50, obj$info$resolution$x[1] + 50))
  } else {
    yli = obj$raw[eval(substitute(subs)),
                  ylim(c(min(c(0, min(y, na.rm = TRUE))),
                         max(c(obj$info$resolution$y[1] + 50, max(y, na.rm = TRUE)))))]
    xli = obj$raw[eval(substitute(subs)),
                  xlim(c(min(c(0, min(x, na.rm = TRUE))),
                         max(c(obj$info$resolution$x[1] + 50, max(x, na.rm = TRUE)))))]
  }
  
  if (what == "gaze") {
    ## plot gaze paths
    g = ggplot(data = obj$raw[eval(substitute(subs))],
               mapping = aes(x, y, col = factor(subject))) +
      geom_rect(xmin = 0, xmax = obj$info$resolution$x[1],
                ymin = 0, ymax = obj$info$resolution$y[1], fill = "lightgrey") +
      geom_path(alpha = 1, size = .1)  +
      xli + yli + theme_classic()
    
  } else if (what %in% c("ps", "cen")) {
    ## plot pupil size paths
    if (what == "cen")  
      subdat = obj$cen[eval(substitute(subs))]
    else if ("smooth" %in% names(obj) & raw == FALSE)
      subdat = obj$smooth[eval(substitute(subs))]
    else  
      subdat = obj$raw[eval(substitute(subs))]
    
    #   casted_subdat = dcast(subdat, time_in_trial ~ subject + trial, value.var = "ps")
    #   molten_subdat = melt(casted_subdat, id.vars = "time_in_trial")
    #   g = 
    #     ggplot(molten_subdat, aes(time_in_trial, value, col = variable)) +
    #     geom_line()  +
    #     geom_vline(xintercept = obj$msg[eval(substitute(subs))][text %in% show_msgs, time_in_trial]) +
    #     ylab("pupil size") +
    #     theme_classic() + 
    #     ylim(molten_subdat[, mean(value) + c(-1, 1) * 1000])
    ## define summary function to use below:
    f = function(dat) {
      m = mean(dat, na.rm = T); 
      stderr = sd(dat, na.rm = T)
      # returns mean +- standard deviation
      data.frame(ymin = m - stderr, y = m, ymax = m + stderr)
    }
    
    g <- switch(type,
                "1" = 
                  ggplot(subdat, aes(time_in_trial, ps, col = factor(subject), shape = factor(trial))) +
                  geom_line(),
                "2" = 
                  ggplot(subdat, aes(time_in_trial, ps, col = factor(subject))) + 
                  stat_summary(fun.y = "mean", geom = "line"), 
                "3" = 
                  ggplot(subdat, aes(time_in_trial, ps, col = factor(subject))) +
                  aes_string(linetype = condition) + 
                  stat_summary(fun.y = "mean", geom = "line"),
                "4" =  
                  ## pointwise mean functions +- standard deviation
                  ggplot(subdat, aes(time_in_trial, ps, col = factor(subject))) +
                  aes_string(linetype = condition) + 
                  stat_summary(fun.data = f),
                "5" =  
                  ## pointwise mean functions +- standard deviation
                  ggplot(subdat, aes(time_in_trial, ps, col = factor(subject))) +
                  aes_string(linetype = condition) + 
                  stat_summary(fun.data = "mean_cl_boot"),
                "6" = 
                  ggplot(subdat, aes(time_in_trial, ps,)) +
                  aes_string(linetype = condition) + 
                  stat_summary(fun.y = "mean", geom = "line"))
  }
  print(g)
  g
}

#' Quick plot
#' 
#' Convenience function
#' @export
qp = function(subdat, ...) {
  casted_subdat = dcast(subdat, time_in_trial ~ subject + trial, value.var = "ps")
  matplot(casted_subdat[,  1, with = FALSE],
          casted_subdat[, -1, with = FALSE], type = "b", 
          xlab = "time", ylab = "pupil size", ...)
}

# subdat = obj$raw[eval(substitute(subs))]
# casted_subdat = dcast(subdat, time_in_trial ~ subject + trial, value.var = "pups")
### old
## ## prepare subset
##  if (!is.null(subj))
##      subjs = subj
##  else
##      subjs = obj$raw[ , unique(subject)]
##  if (! is.null(trials)) {
##      stopifnot(all(trials %in% obj$raw[, unique(trial)]))
##      yli = obj$raw[.(subj),
##                    ylim(c(0 - 50, resolution$y + 50))]
##      xli = obj$raw[.(subj),
##                    xlim(c(0 - 50, resolution$x + 50))]
##  } else {
##      trials = obj$raw[ , unique(trial)]
##      yli = obj$raw[.(subj),
##                    ylim(c(min(c(0, min(y, na.rm = TRUE))),
##                           max(c(resolution$y + 50, max(y, na.rm = TRUE)))))]
##      xli = obj$raw[.(subj),
##                    xlim(c(min(c(0, min(x, na.rm = TRUE))),
##                           max(c(resolution$x + 50, max(x, na.rm = TRUE)))))]
##  }
## subs = expand.grid(subject = subjs, trial = trials)

#############
### tests ###
#############

## obj = data.table(expand.grid(subject = 1:2, trial = 1:3, t = 1:5), key = "subject")
## obj[, x:= rnorm(.N)]
## obj[, y:= rnorm(.N)]
## obj[subject %in% 1:2, plot(x, y, col = as.numeric(subject))]
## obj[subject %in% 1:2, plot(x, y, type = "l", col = as.numeric(subject))]
## obj[subject %in% 1:2, plot(x, y, type = "b", col = as.numeric(subject))]

## library(ggplot2)

## obj[subject %in% 1:2, plot(x, y, type = "l", col = as.numeric(subject))]
## ggplot(as.data.frame(obj[subject %in% 1:2 & trial == 1])) +
##     geom_point(aes(x, y, col = subject)) 

## ggplot(as.data.frame(obj[subject %in% 1:2 & trial == 1])) +
##     geom_path(aes(x, y, col = subject)) 

## ggplot(obj[subject %in% 1:2 & trial == 1]) +
##     geom_line(aes(x, y, col = subject)) 

## subdt = alldat$raw[.(c("03", "02"), 1),]
## ggplot(subdt) +
##     geom_path(aes(x, y, col = subject)) 

## ggplot(alldat$raw[.(c("03", "02"), 1),]) +
##     geom_path(aes(x, y, col = subject)) 

## alldat$raw[subject %in% "02" & trial == 1, plot(x=x, y=y)]
## alldat$raw[subject %in% "02" & trial == 1,
##            ggplot() + geom_line(aes(x=x, y=y, col = subject))]

## ## weird behavior of plot
## plot(obj[subject %in% 1:2, .(x,y)], col = obj$subject)
## plot(obj[subject %in% 1:2, .(x,y)], col = obj$subject, type = "l")
## plot(obj[subject %in% 1:2, .(x,y)], col = obj$subject, type = "b")
## plot(obj[subject %in% 1:2, .(x,y)], col = 1:2, type = "b")
## plot(obj[subject %in% 1:2, .(x,y)], col = 1:2, type = "l")


#' Plot subdat on image.
#'
#' @param subdat [data.table] with components x and y.
#' @param imgi   [cimg] image to use as background.
#' @import data.table
#' @import imager
#' @export
plot_gaze = function(subdat, imgi, ...) {
    imp = implot(imager::mirror(imgi, "y"),
                 subdat[, plot(x, y, ...)])
    plot(mirror(imp, "y"))
}
