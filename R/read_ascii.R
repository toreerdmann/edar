#' read_ascii 
#' Function for loading raw data from a single .asc file into R.
#' 
#' @param trial_seperators [list] with entries 'trial_start' and 'trial_end'. Either these entries contain 
#' a character vector of length 1 with the message delineating the trials, or a character with one message
#' per trial. For example, the whole time series of the experiment is cut between the first entries of 
#' `trial_start` and `trial_end` to obtain the first trial.
#' `trial_end` can also be [numeric] and indicating the length trials.
#' @export
read_ascii <- function(path_ascii, save_rdata = "", number_trials = NULL,
                       trial_seperators = NULL, ...) {

    ## skip is not important, as we are not using the first rows anyway, but may be different
    ## for other datasets
    system.time(
    asc <- read.table(path_ascii, sep = "\t", col.names = paste0("V", 1:9),
                      fill = TRUE, header = FALSE, stringsAsFactors = FALSE, ...)
    )
    asc <- data.table(asc)
    setnames(asc, 1:4, c("t", "x", "y", "pups"))

    if (nrow(asc) < 10000)
        return(list(raw = data.table(),
                    raw = data.table(),
                    msg = data.table(),
                    sacc = data.table(),
                    fix = data.table(),
                    blink = data.table()))
    
    ## cut off beginning and last message
    # input_idx <- asc[, (grep("^INPUT", t))]
    # input_l <- length(input_idx)
    # asc <- asc[input_idx[input_l -1]:(input_idx[input_l] - 1) ]

    ## gsub messages
    asc[t %in% grep("^[A-Z]", t, value = TRUE),
        msg := gsub(pattern = "*\\sR?\\s+\\d+\\s*", replacement = "", x = t)]
    
    ### this was the old way:
    ## idx <- which(asc$msg == "END")
    ## idx <- idx[length(idx)]
    ## asc <-
    ##     asc[ - ((idx-1):nrow(asc)) , ]
    #####################################
    ### make message dt
    #####################################
    ## only take first time value for MSG, ans SFIX/SSAC
    ## messagedf <-
    ##     asc[msg == "MSG", .(msg = gsub("^[0-9]+\ ", "", x),
    ##                         t = (gsub("[^0-9]?.?\d", "", x)))]
    if (!is.null(number_trials) && number_trials %in% c(144, 171, 192)) {
      print("getting trials via TRIALID msg... ")
      msg <- grep("TRIALID", asc$x, value = TRUE)
      t <- gsub("([^0-9]).*$", "", grep("MODE RECORD", asc$x, value = TRUE))
      messagedf <- data.table(msg = gsub(".+(TRIALID [0-9]+)$", "\\1", msg), t = as.numeric(t))
    } else {
      messagedf <-
        asc[msg == "MSG", .(msg = gsub("^[0-9]+\ ", "", x),
                            t = as.numeric(gsub("[^0-9]+(.[0-9]{1,2})?", "", x)))]
    }
    
    suppressWarnings({
      ## only need end of sacc to get all info
      sacc_dt <- asc[t %in% grep("^ESACC", t, value = TRUE),
                     .(t1 = as.numeric(gsub("[^0-9]", "", t)),
                       t2 = as.numeric(x),
                       dur = as.numeric(y),
                       x1 = as.numeric(pups),
                       y1 = as.numeric(V5),
                       x2 = as.numeric(V6),
                       y2 = as.numeric(V7),
                       ampl = as.numeric(V8),
                       velo = as.numeric(V9))]
      
      fix_dt <- asc[t %in% grep("^EFIX", t, value = TRUE),
                    .(t1 = as.numeric(gsub("[^0-9]", "", t)),
                      t2 = as.numeric(x),
                      dur = as.numeric(y),
                      x = as.numeric(pups),
                      y = as.numeric(V5),
                      ps = as.numeric(V6))]
      
      blink_dt <- asc[t %in% grep("^EBLINK", t, value = TRUE),
                      .(t1 = as.numeric(gsub("[^0-9]", "", t)),
                        t2 = as.numeric(x),
                        dur = as.numeric(y))]
    })
    
    
    ## clean up time values
    asc[ , time :=  gsub(pattern = "[^0-9]", replacement = "", x = t)]
    asc[time == "", time :=  gsub(pattern = "([^0-9]).*$", replacement = "", x = x)]
    
    ## remove rows with message data
    ## this step introduces as, as the gaze is coded as "." when missing
    suppressWarnings(
      asc <- asc[is.na(msg), .(x = as.numeric(x),
                               y = as.numeric(y),
                               ps = as.numeric(pups),
                               time = as.numeric(time))]
    )
    
    ## check that there are no large holes
    # if (any(asc[, which(diff(time) > 2)]))
    #   stop("There are jumps in the time vector. The data was not parsed correctly.")
    
    ## subtract time of intro ??
    ## asc[, time2 := as.numeric(time)]
    ## t1 <- messagedf[msg == "BASE", .(t)][[1]]
    ## asc[ , time2 := as.numeric(time) - t1]
    
    
    ## check if time points of msgs exists in data already
    ## this could make trouble otherwise!
    # stopifnot(messagedf[["time"]] %in% asc[["time"]])
    # stopifnot(sacc_dt[["t1"]] %in% asc[["time"]])
    # stopifnot(sacc_dt[["t2"]] %in% asc[["time"]])
    # stopifnot(fix_dt[["t1"]] %in% asc[["time"]])
    # stopifnot(fix_dt[["t2"]] %in% asc[["time"]])
    # stopifnot(blink_dt[["t1"]] %in% asc[["time"]])
    # stopifnot(blink_dt[["t2"]] %in% asc[["time"]])
    ## if yes, we can append it as another column
    
    ## TODO:
    ## remove events that are not within time frame of asc[["time"]]
    
    
    ## make a copy to recover from
    ## asc2 <- asc
    setkey(asc, time)
    
    ## if the data should not be sliced into trials,
    ## stop here.
    if (is.null(trial_seperators))
      return(list(raw = asc,
                  raw_notrial = data.table(),
                  msg = messagedf,
                  sacc = sacc_dt,
                  fix = fix_dt,
                  blink = blink_dt))
    
    #####################################
    ### slice trials
    #####################################
    
    ## from = "QUESTION"; until = "FIX3"
    if (is.null(number_trials)) {
      ## this is old, from the ToM analysis
      ## number_trials <- nrow(messagedf[msg == "QUESTION"])
      
      ## try to infer number of trials
      number_trials <- length(messagedf[, msg[msg %in% trial_seperators$trial_start]])
    }
    
    ## starting times of all trials
    tstart_vec <- messagedf[, t[msg %in% trial_seperators$trial_start]]
    
    if (length(tstart_vec) == 0)
      stop(c("'trial_start' messages not found. Please make sure to input messages",
             " just as they are in the raw .asc file."))
    
    ## trial_end is a vector of numbers, use these as the length
    ## if they are character, regard them as messages
    if (is.numeric(trial_seperators$trial_end[1])) {
      if (! (length(trial_seperators$trial_end) == 1 ||
             length(trial_seperators$trial_end) == number_trials))
        stop("trial_seperators$trial_end has to have length 1 or equal to number of trials.")
      tend <- tstart + trial_seperators$trial_end
    } else {
      tend_vec <-
        messagedf[, t[msg %in% trial_seperators$trial_end]]
    }
    
    ## assign trials
    system.time(
      for(i in 1:number_trials) {
        
        tstart <- tstart_vec[i]
        tend <- tend_vec[i]
        
        ## at last trial, let everyting til the end be in that trial
        if (is.na(tend))
          tend <- tstart + asc[ , max(time, na.rm = TRUE)]
        
        ## add trial index and time_in_trial: relative to start of current trial
        asc[ time >= tstart & time < tend,
             `:=`(trial = i,
                  time_in_trial = time - tstart)]
        messagedf[ t >= tstart & t <= tend,
                   `:=`(trial = i,
                        time_in_trial = t - tstart)]
        sacc_dt[ t1 >= tstart & t2 <= tend,
                 `:=`(trial = i,
                      time_in_trial = t1 - tstart)]
        fix_dt[ t1 >= tstart & t2 <= tend,
                `:=`(trial = i,
                     time_in_trial = t1 - tstart)]
        blink_dt[ t1 >= tstart & t2 <= tend,
                  `:=`(trial = i,
                       time_in_trial = t1 - tstart)]
      })
    
    ## format(object.size(asc), units = "auto")
    asc_notrial <- asc[is.na(trial)]
    asc <- asc[!is.na(trial)]
    
    if (save_rdata != "") 
      saveRDS(list(raw = asc, raw_notrial = asc_notrial,
                   msg = messagedf, sacc = sacc_dt,
                   fix = fix_dt, blink = blink_dt),
              file = paste0(save_rdata, ".rds"))
    
    ## return everything
    list(raw = asc, raw_notrial = asc_notrial,
         msg = messagedf, sacc = sacc_dt,
         fix = fix_dt, blink = blink_dt)
}
