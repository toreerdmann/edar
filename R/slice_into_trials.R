
#' @export
slice_into_trials = function(obj, 
                             trial_seperators = list(), 
                             save_rdata = "", 
                             number_trials = NULL) {
  
  ## convert to dt
  is_eyelinker = FALSE
  if (! is.data.table(obj$msg)) {
    ## in this case, the data was loaded with read_ascii
    obj$msg = data.table(obj$msg)
    is_eyelinker = TRUE
  }
  if (!is_eyelinker) 
    setnames(obj$msg, c("text", "time"))
  
  if (is.null(number_trials)) {
    ## try to infer number of trials
    number_trials <- length(obj$msg[, text[text %in% trial_seperators$trial_start]])
  }
  
  ## starting times of all trials
  tstart_vec <- obj$msg[, time[text %in% trial_seperators$trial_start]]
  
  if (length(tstart_vec) == 0)
    stop(c("'trial_start' messages not found. Please make sure to input messages",
           " just as they are in the raw .asc file."))
  
  ## trial_end is a vector of numbers, use these as the length
  ## if they are character, regard them as messages
  if (is.numeric(trial_seperators$trial_end[1])) {
    if (! (length(trial_seperators$trial_end) == 1 ||
           length(trial_seperators$trial_end) == number_trials))
      stop("trial_seperators$trial_end has to have length 1 or equal to number of trials.")
    tend_vec <- tstart_vec + trial_seperators$trial_end
  } else {
    tend_vec <-
      obj$msg[, time[text %in% trial_seperators$trial_end]]
  }
  
  rval = list()
  if (is_eyelinker) {
    rval$raw = data.table(obj$raw[, c("time", "xp", "yp", "ps")])
    names(rval$raw) = c("time", "x", "y", "ps")
    rval$msg = obj$msg[, .(time, text)]
    rval$fix = data.table(obj$fix[, 1:6])
    names(rval$fix)[c(1:2, 4:6)] = c("t1", "t2", "x", "y", "ps")
    rval$sacc = data.table(obj$sacc[, 1:8])
    names(rval$sacc)[c(1:2, 4:7)] = c("t1", "t2", "x1", "y1", "x2", "y2")
    rval$blink = data.table(obj$blink[, 1:3])
    names(rval$blink)[1:2] = c("t1", "t2")
  } else {
    rval$raw = obj$raw[, .(time, x, y, ps = ps)]
    rval$msg = obj$msg[, .(time, text)]
    rval$fix = obj$fix[, .(t1, t2, dur = dur, x, y, ps = ps)]
    rval$sacc = obj$sacc[, .(t1, t2, dur = dur, x1, y1, x2, y2, ampl, velo)]
    rval$blink = obj$blink[, .(t1, t2, dur = dur)]
  }
    
  system.time(
    for(i in 1:number_trials) {
      tstart <- tstart_vec[i]
      tend <- tend_vec[i]
      
      ## at last trial, let everyting til the end be in that trial
      if (is.na(tend))
        tend <- tstart + rval$raw[ , max(time, na.rm = TRUE)]
      
      # set(rval$raw, rval$raw[, which(time > tstart & time < tend)], j = trial, value = i)
      # set(rval$msg, time > tstart & time < tend, j = trial, value = i)
      # set(rval$sacc, t1 > tstart & t2 < tend, j = trial, value = i)
      # set(rval$fix, t1 > tstart & t2 < tend, j = trial, value = i)
      # set(rval$blink, t1 > tstart & t2 < tend, j = trial, value = i)
      
      ## add trial index and time_in_trial: relative to start of current trial
      rval$raw[ time >= tstart & time < tend,
                `:=`(trial = i,
                     time_in_trial = time - tstart)]
      rval$msg[ time >= tstart & time <= tend,
                 `:=`(trial = i,
                      time_in_trial = time - tstart)]
      rval$sacc[ t1 >= tstart & t2 <= tend,
               `:=`(trial = i,
                    time_in_trial = t1 - tstart)]
      rval$fix[ t1 >= tstart & t2 <= tend,
              `:=`(trial = i,
                   time_in_trial = t1 - tstart)]
      rval$blink[ t1 >= tstart & t2 <= tend,
                `:=`(trial = i,
                     time_in_trial = t1 - tstart)]
    })
  
  ## format(object.size(rval), units = "auto")
  rval$raw_notrial = rval$raw[is.na(trial)]
  rval$raw = rval$raw[!is.na(trial)]
  
  ## set class
  class(rval) = c(class(rval), "edar_data")
  
  if (save_rdata != "") 
    saveRDS(rval, file = paste0(save_rdata, ".rds"))
  
  ## return everything
  rval
}
