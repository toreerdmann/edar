####################################
### functions for removing blinks,
### interpolation,
### and luminance extraction
####################################

#' Interpolate blinks per trial
#' 
#' First, data is thinned by taking every `thin`-th value.
#' Then blinks are detected by high values of the derivative and 
#' interpolated linearly.
#' @import data.table
#' @export
interpolate_blinks = function(obj, time_new, thin = 100, verbose = FALSE) {
    if (! "edar_data" %in% class(obj))
        stop(sprintf("argument obj %s has to be an object of class 'edar_data'.", obj))
    ## maybe use obj$blink_dt for this?
    obj$smooth = 
        obj$raw[
                1:.N %% thin == 1][,
                                   .( time_in_trial = time_new, 
                                     x = {
                                         if (verbose)
                                             cat(sprintf("subj %s, trial %d\n", subject, trial))
                                         idx <- ps != 0 & c(abs(diff(ps)) < 100, TRUE)
                                         if (sum(!is.na(x[idx])) < .5 * length(x))
                                             NA_real_
                                         else 
                                             approx(time_in_trial[idx], x[idx], xout = time_new)$y
                                     },
                                     y = {
                                         idx <- ps != 0 & c(abs(diff(ps)) < 100, TRUE)
                                         if (sum(!is.na(y[idx])) < .5 * length(y))
                                             NA_real_
                                         else 
                                             approx(time_in_trial[idx], y[idx], xout = time_new)$y
                                     },
                                     ps = {
                                         idx <- ps != 0 & c(abs(diff(ps)) < 100, TRUE)
                                         ## drop observations that are 0 (blink) or 
                                         ## high velocity (right around the blink)
                                         if (sum(!is.na(ps[idx])) < .5 * length(ps))
                                             NA_real_
                                         else 
                                             approx(time_in_trial[idx], ps[idx], xout = time_new)$y
                                     }), by = .(subject, trial)]
    ## add time to interpolated data, for correcting slow drift later 
    obj$smooth = merge(obj$smooth, obj$raw[, .(subject, trial, time_in_trial, time)], 
                       by = c("subject", "trial", "time_in_trial"))
    obj
}

## scale and center
#' @import data.table
#' @export
scale_center = function(obj, reftimes) {
    if (! "edar_data" %in% class(obj))
        stop(sprintf("argument obj %s has to be an object of class 'edar_data'.", obj))
    if (! "smooth" %in% names(obj)) {
        cat("Scaling and centering not interpolated data. It is recommended to first use the 
        'interpolate_blink' function.")
        obj$cen = obj$raw[, .(ps = (ps - ps[1]) / ps[1],
                              time_in_trial),
                          by = .(subject, trial)]
    } else {
        if (length(reftimes) == 1 & is.numeric(reftimes)) {
            
            if (all(abs(reftimes - obj$smooth[, unique(time_in_trial)]) > 5)) {
                stop("reftimes not within the time range of the interpolated data.")
            }
            
            obj$cen = 
                obj$smooth[, .SD[, .(ps = {
                    mps = median(ps[abs(time_in_trial - reftimes) < 5], na.rm = TRUE)
                    (ps - mps) / mps
                }, 
                time_in_trial)], by = .(subject, trial)]
            
        } else if (length(reftimes) == 2 & is.numeric(reftimes)) {
            if (all(abs(reftimes - obj$smooth[, unique(time_in_trial)]) > 5)) {
                stop("reftimes not within the time range of the interpolated data.")
            }
            obj$cen = 
                obj$smooth[, .SD[, .(ps = {
                    mps = median(ps[abs(time_in_trial - reftimes[1]) < reftimes[2]], na.rm = TRUE)
                    (ps - mps) / mps
                }, 
                time_in_trial)], by = .(subject, trial)]
            
        } else if (is.data.table(reftimes)) {
            obj$cen = 
                obj$smooth[,
                           .(ps = {
                               mps = 
                                   median(ps, na.rm = TRUE)[time_in_trial > reftimes[.(subject, trial), V1[1]] &
                                                            time_in_trial < reftimes[.(subject, trial), V1[2]]]
                               (ps - mps) / mps
                           },
                           time_in_trial), 
                           by = .(subject, trial)]
        } else {
            stop("reftimes has to be a numeric of length 1 or a data.table.")
        }
    }
    obj
}

#' @import data.table
#' @export
get_luminance = function(design, triali, img, rectdims) {
    stopifnot(is.list(rectdims) & length(rectdims) == 2)
    scalefac = rectdims$bottomright - rectdims$topleft
    ## get path
    pointPath = 
        design$design[trial == triali, point_path[[1]]][, .(x, y)] 
    ## flip y
    pointPath[, y := -1 * y]
    ## scale to [(0,0),(1,1)]
    pointPath[, `:=`(x = (x + 1) * 1/2,
                     y = (y + 1) * 1/2)]
    ## scale to rectdims
    pointPath[, `:=`(x = x * scalefac[1],
                     y = y * scalefac[2])]
    ## shift down and right
    pointPath[, `:=`(x = x + rectdims[[1]][1],
                     y = y + rectdims[[1]][2])]
    z = imageData(img)[,,1]
    luminancePath = z[pointPath[, cbind(round(x), round(y))]]
    oldpar = par(no.readonly = TRUE)
    par(mfrow=c(2,1))
    plot(luminancePath, ylab = "luminance", xlab = "t", type = "b")
    display(img, method = "raster")
    text(pointPath, pch = 19, col = 4)
    ## reset par
    par(oldpar)
    list(pointPath = pointPath, luminancePath = luminancePath)
}

##############################
## general utility functions
##############################
#' @export
grep_files <- function(pattern, path, ...) {
    grep(pattern, dir(path, full.names = TRUE), value = TRUE, ...)
}


## mirror y coordinates at middle of the screen
#' @import data.table
#' @export 
flip = function(subdata) {
    stopifnot("edar_data" %in% class(subdata))
    if (is.null(subdata$info$resolution))
        stop("Resolution is NULL. Set resolution prior to flipping.")
    mid_y = subdata$info$resolution$y / 2
    rval = lapply(subdata, function(subdat) {
        if (is.data.table(subdat)) {
            if ("y" %in% names(subdat)) {
                subdat[, y := (- 1 * (y - mid_y)) + mid_y]
            } else if (all(c("y1", "y2") %in% names(subdat))) {
                subdat[, y1 := (- 1 * (y1 - mid_y)) + mid_y]
                subdat[, y2 := (- 1 * (y2 - mid_y)) + mid_y]
            } 
        }
        subdat
    })
    if (! is.null(rval$info$flipped)) {
        if (rval$info$flipped)
            rval$info$flipped = FALSE
        else
            rval$info$flipped = TRUE
    }
    class(rval) = c(class(rval), "edar_data")
    rval
}

#' Adding luminance information
#' Extract and append luminance paths to the data.
#' @param obj [edar_data] obj, created by load_data or read_ascii.
#' @param files [list] of vectors of file-paths ordered by trial and 
#' with names corresponding to the subject IDs in 'obj'.
#' @import data.table
#' @importFrom EBImage resize readImage imageData medianFilter
#' @export
add_luminance = function(obj, files, imgdir = NULL, resize_img = NULL, 
                         smooth_img = FALSE, verbose = FALSE) {
    ntrials = obj$smooth[, length(unique(trial))] 
    subs = obj$smooth[, unique(subject)] 
    if (! inherits(files, "data.frame")) {
        files = obj$smooth[, .(image = files[trial]), by = .(subject, trial)]
        obj$img = files
    } else {
        files2 = files
        files2[ , image := basename(image)]
        setkey(files2, subject, trial)
        obj$img = files2
    }
    
    obj$smooth = merge(obj$smooth, files[, .(subject, trial, image  = basename(image))], by = c("subject", "trial"))

    obj$smooth[x > 0 & y > 0 &  
               x < obj$info$resolution$x & 
               y < obj$info$resolution$y, 
               lumi := {
                   if (verbose) {
                       if (trial == 1)
                           cat(sprintf("\n"))
                       cat(sprintf("\rprocessing subject: %s, trial: %d, image: %s\n", 
                                   subject, trial, image[1]))
                   }
                   if (is.null(imgdir))
                       filei = image[1]
                   else
                       filei = grep_files(image[1], imgdir)
                   if (length(filei) == 0)
                       stop("file not found.")
                   if (length(filei) > 1)
                       stop(sprintf("multiple matches for file: %s.", image[1]))
                   img = EBImage::readImage(filei)
                   if (smooth_img > 0)
                       img = EBImage::medianFilter(img, smooth_img)
                   img = EBImage::imageData(img)
                   if (length(dim(img)) == 3) {
                       img = img[,,1] + img[,,2] + img[,,3]
                       img = img / max(img)
                   }
                   cat(sprintf("bla"))
                   if (! is.null(resize_img))
                       img = t(as.matrix(EBImage::resize(img, h = resize_img)))
                   else
                       img = t(as.matrix(img))
                   rscreen = construct_screen(img, obj$info$resolution$x, obj$info$resolution$y)
                   rscreen[cbind(floor(y), floor(x))]
               }, by = .(subject, trial)]
    obj
}

#' @import data.table
#' @export
cast_data = function(obj, ntimepoints = NULL) {
    stopifnot(inherits(obj, "edar_data"))
    subs = unique(obj$smooth$subject)
    if (is.null(ntimepoints))
        ntimepoints = obj$smooth[trial == 1, length(unique(time_in_trial))]
    psmat = do.call(rbind, lapply(subs, function(subji)
        t((dcast(obj$smooth[.(subji)], time_in_trial ~ trial, 
                 value.var = "ps"))[,-1,with=FALSE])))
    xmat = do.call(rbind, lapply(subs, function(subji)
        t(as.matrix(dcast(obj$smooth[.(subji)], 
                          time_in_trial ~ trial, 
                          value.var = "x")[,-1,with=F]))))
    ymat = do.call(rbind, lapply(subs, function(subji)
        t(as.matrix(dcast(obj$smooth[.(subji)], 
                          time_in_trial ~ trial, 
                          value.var = "y")[,-1,with=F]))))
    lumimat = do.call(rbind, lapply(subs, function(subji)
        t(as.matrix(dcast(obj$smooth[.(subji)], 
                          time_in_trial ~ trial, 
                          value.var = "luminance")[,-1,with=F]))))
    vars = names(obj$smooth)[! names(obj$smooth) %in% c("subject", "trial", "time", "time_in_trial", "x", "y" ,"ps", "ps_z", "luminance")]
    obj$smooth[, ]
    form = as.formula(paste0("subject + trial + ", paste(vars, collapse = " + "), " ~ time_in_trial"))
    casted = dcast(dat$smooth, form, value.var = "ps")
    casted = casted[, 1:(2 + length(vars)), with = FALSE]
    casted[, subject := factor(subject)]
    designmat = append(list(pupilsize = I(psmat),
                     x = I(xmat),
                     y = I(ymat),
                     luminance = I(lumimat),
                     t = obj$smooth[, sort(unique(time_in_trial))]),
                     as.list(casted))
    designmat
}
