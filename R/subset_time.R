
## functions which takes list of data.tables and subsets them for a relevant time window
##
## asc        [eyedata object] typical list of data.tables
##
## bounds     [character] vector of length two with start and end message of time window, or
##            [list] with time vectors 
#' @import data.table
#' @export
subset_time <- function(asc, bounds) {
    stopifnot(key(asc$eye) %in% c("subject", "trial", "time"))
    
    ## use closure 
    if(is.character(bounds)) {
        bounds <-
            asc$msg_dt[ , .(t[msg %in% bounds[1]],
                            t[msg %in% bounds[2]]),
                       by = .(subject, trial)]
    }
    
    time_window <- function(subject_i, trial_i) {
        bnds <- bounds[subject == subject_i & trial == trial_i]
        bnds[[3]]:bnds[[4]]
    }
    ## plot(asc$sacc_dt$t1)
    ## points(time_window("TBAA", 100))
    ## asc$sacc_dt <-
    ##     asc$sacc_dt[!is.na(trial) , .SD[trial == 1, time_window("TBAA", 1)],
    ##                 by = .(subject, trial)]
    ##     asc$sacc_dt[!is.na(trial) , .(t1, time_window(subject, trial)),
    ##                 by = .(subject, trial)]

    ## restrict data to the time between IMAGE and FIX2 during the trials
    ## make sure, keys are set for subject, trial and time!
    asc$eye <- asc$eye[ , .SD[time %in% time_window(subject, trial)], by = .(subject, trial)]
    asc$fix_dt <-
        asc$fix_dt[!is.na(trial) , .SD[t1 %in% time_window(subject, trial) &
                                       t2 %in% time_window(subject, trial), .SD],
                   by = .(subject, trial)]
    asc$sacc_dt <-
        asc$sacc_dt[!is.na(trial) , .SD[t1 %in% time_window(subject, trial) &
                                        t2 %in% time_window(subject, trial), .SD],
                    by = .(subject, trial)]
    asc$blink_dt <-
        asc$blink_dt[!is.na(trial) , .SD[t1 %in% time_window(subject, trial) &
                                         t2 %in% time_window(subject, trial), .SD],
                     by = .(subject, trial)]
    
    ## ## asc$eye <-
    ## ##     asc$eye[ ,
    ## ##             .(x = x[time %in% time_window(subject, trial)],
    ## ##               y = y[time %in% time_window(subject, trial)],
    ## ##               time = time[time %in% time_window(subject, trial)],
    ## ##               time_in_trial = time_in_trial[time %in% time_window(subject, trial)],
    ## ##               condition = condition[time %in% time_window(subject, trial)],
    ## ##               image = image[time %in% time_window(subject, trial)]),
    ## ##             by = .(subject, trial)]
    ## asc$fix_dt <- asc$fix_dt[ ,
    ##                          .(x = x[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                            y = y[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                            t1 = t1[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                            t2 = t2[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                            duration = duration[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                            time_in_trial = time_in_trial[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                             trial = trial,
    ##                            condition = condition, image = image), 
    ##                          by = .(subject, trial)]
    ##  asc$sacc_dt <- asc$sacc_dt[ ,
    ##                             .(x1 = x1[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               y1 = y1[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               x2 = x2[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               y2 = y2[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               t1 = t1[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               t2 = t2[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               duration = duration[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               angle = angle[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               velo = velo[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                               trial = trial, condition = condition, image = image),
    ##                             by = .(subject, trial)][-1]
    ##  asc$blink_dt <- asc$blink_dt[ ,
    ##                               .(t1 = t1[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                                 t2 = t2[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                                 duration = duration[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                                 time_in_trial = time_in_trial[t1 %in% time_window(subject, trial) & t2 %in% time_window(subject, trial)],
    ##                                 trial = trial, condition = condition, image = image),
    ##                               by = .(subject, trial)][-1]
    
    ## class(asc) <- c(class(asc), "eye")
    asc
}

## depends on bounds in the working directory
## time_window <- function(subject_i, trial_i) {
##     bnds <- bounds[subject == subject_i & trial == trial_i]
##     bnds[[3]]:bnds[[4]]
## }

## test
## time_window("TBAA", 2)
