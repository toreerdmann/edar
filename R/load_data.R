#' load_data
#' Function for loading data from multiple .asc files into R.
#' @param datadir [character vector] with the path to the folder where the data is located. If of length
#' greater than 1, it is assumed to be a vector of paths to files that are to be read in.
#' @import data.table
#' @import parallel
#' @importFrom BBmisc extractSubList
#' @export
load_data <- function(datadir, savedir = "", prefix = "", trial_seperators = NULL,
                      with_images = FALSE, discard_oot_data = FALSE, ncpus = 1, ...) {
    ## if (missing(prefix))
    ##     stop("Please provide prefix for naming the data files.")

  if (length(datadir) > 1 || file.exists(datadir)) {
    files = datadir 
    if (! all(file.exists(files)))
      stop(sprintf("Files %s could not be found.", 
                   paste(files[!file.exists(files)], collapse = ", ")))
    print("Reading in the following files: ")
    print(files)
    if (interactive())
      invisible(readline(prompt="Press [enter] to continue"))
  } else {
    ## get the ascii files
    files <- grep(".asc$", dir(datadir, full.names = TRUE), value = TRUE)
    
    ## filter with prefix
    files <-
      files[grep(paste0("^", prefix), basename(files))]
    print("Reading in the following files: ")
    print(files)
    print("You can restrict the selection by using the 'prefix' argument.")
    if (interactive())
      invisible(readline(prompt="Press [enter] to continue"))
  } 
  
  ## extract ids from file names
  ids <- sub("_[A-z]$", "", gsub("[.][[:alnum:]]+", "", basename(files)))
  
  if (with_images) {
    ## get questions and images
    images <- get_images(datadir)
  }
 
  ## load and sort out all files
  system.time(
    datalist <- parallel::mclapply(seq_along(files), function(i) {
      id_i <- ids[i]
      print(paste0("loading data for id: ", id_i, " ... "))
      
      if (id_i == "fMmW") {
        asc <- read_ascii(path_ascii = files[i], save_rdata = "", number_trials = 171,
                          trial_seperators = trial_seperators)
        ## add subject indicators
        asc$raw[ , subject := id_i]
        if (!is.null(asc$raw_notrial))
            asc$raw_notrial[ , subject := NULL]
        asc$msg[ , subject := id_i]
        asc$fix[ , subject := id_i]
        asc$sacc[ , subject := id_i]
        asc$blink[ , subject := id_i]
      } else {
        asc <- read_ascii(path_ascii = files[i])
        
        ## slice into trials
        if (! is.null(trial_seperators))
          asc <- slice_into_trials(asc, trial_seperators)
        
        ## add subject indicators
        asc$raw[ , subject := id_i]
        if (!is.null(asc$raw_notrial))
            asc$raw_notrial[ , subject := id_i]
        asc$msg[ , subject := id_i]
        asc$fix[ , subject := id_i]
        asc$sacc[ , subject := id_i]
        asc$blink[ , subject := id_i]
      }
      asc
    }, mc.cores = ncpus)
  )
  
  if (length(datalist) > 1)
    ## bind all eye data
    data <- list(raw = rbindlist(BBmisc::extractSubList(datalist, "raw", simplify = FALSE)),
                 raw_notrial = rbindlist(BBmisc::extractSubList(datalist, "raw_notrial", simplify = FALSE), fill = TRUE),
                 msg = rbindlist(BBmisc::extractSubList(datalist, "msg", simplify = FALSE)),
                 fix = rbindlist(BBmisc::extractSubList(datalist, "fix", simplify = FALSE)),
                 sacc = rbindlist(BBmisc::extractSubList(datalist, "sacc", simplify = FALSE)),
                 blink = rbindlist(BBmisc::extractSubList(datalist, "blink", simplify = FALSE)))
  else
    data = datalist[[1]]
  
  class(data) <- c(class(data), "edar_data")
  setkeyv(data$raw, c("subject", "trial"))
  setkeyv(data$raw_notrial, c("subject", "trial"))
  setkeyv(data$msg, c("subject", "trial"))
  setkeyv(data$fix, c("subject", "trial"))
  setkeyv(data$sacc, c("subject", "trial"))
  setkeyv(data$blink, c("subject", "trial"))
  
  ## add resolution to data
  f1 = read.table(files[1], nrows = 80, sep = "\t", 
                  col.names = paste0("V", 1:9),
                  fill = TRUE, header = FALSE, stringsAsFactors = FALSE)
  names(f1)[1:4] = c("t", "x", "y", "pups")
  ss = strsplit(f1[grep("DISPLAY_COORDS", f1[,2]), 2], " ")[[1]]
  coords = as.numeric(ss[(length(ss)-1):length(ss)])
  data$info$resolution = list(x = coords[1], y = coords[2])
  
  ## save data
  if (savedir != "") {
    saveRDS(data, file = file.path(savedir, paste0("data_", prefix, "_all.rds")))
  }
  data
}


