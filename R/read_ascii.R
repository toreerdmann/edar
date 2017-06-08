#' Read and parse an ascii (.asc) file.
#'
#' @param path_ascii path to a .asc file
#' @import data.table
#' @export
read_ascii = function(path_ascii) {
    dat = readLines(path_ascii)
    id_lines_star = grep("^[*]{2}", dat)
    id_lines_input = grep("^[INPUT]", dat)

    ## remove last few lines (becuase of invalid file end)
    ## n = length(dat)
    ## dat = dat[-((n-100):n)]
    ## for now, I'll assume that every file has exactly 7 occurences of INPUT
    stopifnot(length(id_lines_input) %in% 6:7)
    ## not true generally...

    ## seperate header and data
    if (length(id_lines_input) == 6) {
        header = dat[1:id_lines_input[5]]
        dat = dat[(id_lines_input[5]+1):(id_lines_input[6]-1)]
    }
    if (length(id_lines_input) == 7) {
        header = dat[1:id_lines_input[6]]
        dat = dat[(id_lines_input[6]+1):(id_lines_input[7]-1)]
    }

    ## parse events
    events = dat[grep("^[0-9]+", dat, invert = TRUE)]
    ## reduce data to regular rows
    dat = dat[grep("^[0-9]+", dat)]

    ## read in raw data
    rawdat = data.table::fread(paste(dat, collapse = "\n"),
                               select = 1:4,
                               colClasses = list(integer = 1, numeric = 2:4),
                               na.strings = ".",
                               stringsAsFactors = FALSE,
                               col.names = c("time", "x", "y", "ps"),
                               nrows = length(dat))

    ## for now, assume that all rows have one millisecond
    ## ... or at least: are the same
    if (! length(unique(diff(rawdat$time))) == 1)
        warning("Jump in raw time vector.")

    ##=========================
    ## sort out events
    ##=========================

    msg = data.table::fread(paste(events[grep("^MSG", events)], collapse = "\n"),
                            colClasses = list(integer = 1, character = 2),
                            stringsAsFactors = FALSE,
                            col.names = c("time", "text"))
    msg[, time := as.integer(gsub("^([0-9]+).*", "\\1", text))]
    msg[, text := gsub("^([0-9]+ )(.*)", "\\2", text)]
    fix = data.table::fread(paste(events[grep("^EFIX", events)], collapse = "\n"),
                            select = 1:6,
                            colClasses = list(integer = 1:3, numeric = 4:6),
                            stringsAsFactors = FALSE,
                            col.names = c("t1", "t2", "dur", "x", "y", "ps"))
    fix[, t1 := as.numeric(gsub("^.* ([0-9]+)$", "\\1", t1))]
    sacc = data.table::fread(paste(events[grep("^ESACC", events)], collapse = "\n"),
                             colClasses = list(integer = 1:3, numeric = 4:7),
                             stringsAsFactors = FALSE,
                             col.names = c("t1", "t2", "dur", "x1", "y1", "x2", "y2", "ampl", "velo"))
    sacc[, t1 := as.numeric(gsub("^.* ([0-9]+)$", "\\1", t1))]
    blink = data.table::fread(paste(events[grep("^EBLINK", events)], collapse = "\n"),
                              colClasses = list(integer = 1:3),
                              stringsAsFactors = FALSE,
                              col.names = c("t1", "t2", "dur"))
    blink[, t1 := as.numeric(gsub("^.* ([0-9]+)$", "\\1", t1))]

    ##=========================
    ## warnings and checks
    ##=========================

    ## TODO: write more sanity checks
    if (sacc[, any(dur > 10000)]) {
        warning(sprintf("Removed saccade starting at %d and ending at time %d, hence with a length > 10000. \n", sacc[dur > 10000, t1], sacc[dur > 10000, t2]))
        ## remove saccades with duration > 10000
        sacc = sacc[dur < 10000]
    }
    if (blink[, any(dur > 10000)]) {
        warning(sprintf("There are blinks with durations > 10.000."))
    }

    ##=========================
    ## return
    ##=========================
    obj = list(raw = rawdat, msg = msg, fix = fix, sacc = sacc, blink = blink, header = header)
    class(obj) = c(class(obj), "edar_data")
    obj
}


#' Read and parse an ascii (.asc) file.
#'
#' @param obj [edar_data] object as returned from read_ascii function
#' @param trial_seperators [list] with entries 'trial_start' and 'trial_end'. Either these entries contain 
#' a character vector of length 1 with the message delineating the trials, or a character with one message
#' per trial. For example, the whole time series of the experiment is cut between the first entries of 
#' `trial_start` and `trial_end` to obtain the first trial.
#' `trial_end` can also be [numeric] and indicating the length trials.
#' @import data.table
#' @export
slice_into_trials = function(obj, trial_seperators, number_trials = NULL) {
    stopifnot(inherits(obj, "edar_data"))

    if (is.null(number_trials)) {
        ## try to infer number of trials
        if (! is.null(trial_seperators$grep) && trial_seperators$grep == TRUE) {
            number_trials <- length(obj$msg[grep(trial_seperators$trial_start, text), text])
        } else {
            number_trials <- length(obj$msg[, text[text %in% trial_seperators$trial_start]])
        }
    }

    if (! all(trial_seperators$trial_start %in% obj$msg[, text])) {
        found = trial_seperators$trial_start %in% obj$msg[, text]
        warning(sprintf("Could only find the following messages provided in 'trial_seperators$trial_start': \n%s\nMessages not found: \n%s\n",
                        paste(trial_seperators$trial_start[found], collapse = ", "),
                        paste(trial_seperators$trial_start[!found], collapse = ", ")))
    }
    
    ## starting times of all trials
    if (!is.null(trial_seperators$grep) && trial_seperators$grep == TRUE) {
        tstart_vec <- obj$msg[grep(trial_seperators$trial_start, text), time]
    } else {
        tstart_vec <- obj$msg[, time[text %in% trial_seperators$trial_start]]
    }
    
    if (length(tstart_vec) == 0)
        stop(c("'trial_start' messages not found. Please make sure to input messages",
               " identical to those in the raw .asc file."))
    
    ## trial_end is a vector of numbers, use these as the length
    ## if they are character, regard them as messages
    if (is.numeric(trial_seperators$trial_end[1])) {
        if (! (length(trial_seperators$trial_end) == 1 ||
               length(trial_seperators$trial_end) == number_trials))
            stop("trial_seperators$trial_end has to have length 1 or equal to number of trials.")
        tend_vec <- tstart_vec + trial_seperators$trial_end
    } else {
        if (!is.null(trial_seperators$grep) && trial_seperators$grep == TRUE) {
            tend_vec <- obj$msg[grep(trial_seperators$trial_end, text), time]
        } else {
            tend_vec <- obj$msg[, time[text %in% trial_seperators$trial_end]]
        }
    }
    
    system.time(
        for(i in 1:number_trials) {
            tstart <- tstart_vec[i]
            tend <- tend_vec[i]
            ## at last trial, let everyting til the end be in that trial
            if (is.na(tend))
                tend <- tstart + obj$raw[ , max(time, na.rm = TRUE)]
            ## add trial index and time_in_trial: relative to start of current trial
            obj$raw[ time >= tstart & time < tend,
                    `:=`(trial = i,
                         time_in_trial = time - tstart)]
            obj$msg[ time >= tstart & time <= tend,
                    `:=`(trial = i,
                         time_in_trial = time - tstart)]
            obj$fix[ t1 >= tstart & t2 <= tend,
                    `:=`(trial = i,
                         time_in_trial = t1 - tstart)]
            obj$sacc[ t1 >= tstart & t2 <= tend,
                     `:=`(trial = i,
                          time_in_trial = t1 - tstart)]
            obj$blink[ t1 >= tstart & t2 <= tend,
                      `:=`(trial = i,
                           time_in_trial = t1 - tstart)]
        })
    
    ## format(object.size(obj), units = "auto")
    obj$raw_notrial = obj$raw[is.na(trial)]
    obj$raw = obj$raw[!is.na(trial)]
    ## ## set class
    ## class(obj) = c(class(obj), "edar_data")
    obj
}
