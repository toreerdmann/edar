## #' Bootstrap based confidence intervals
## #' 
## #' @importFrom purrr map_at
## #' @export
## bootstrap_pffr = function(formula, data, t, nboot = 40, ncpus = 1, seed = NULL) {
##   stopifnot(c("subject", "trial") %in% names(data))
##   stopifnot(is.factor(data$subject))
  
##   if (!is.null(seed))
##     set.seed(seed)
  
##   ntrials = max(data$trial)
  
##   ## draw sample before parallization
##   id_sample = replicate(nboot, setNames(lapply(levels(data$subject), function(subji) {
##       sample(data$trial[data$subject == subji], size = ntrials, TRUE) 
##   }), nm = levels(data$subject)), FALSE)
  
##   msg("starting sampling ...")
##   boot_out = parallel::mclapply(1:nboot, function(rep) {
##     ## take bootstrap sample
##     ## sample same number of trials for each person
##     data_boot = lapply(data, function(list_element_i) {
##       if (is.matrix(list_element_i)) {
##         do.call(rbind, lapply(names(id_sample[[rep]]), function(subji) {
##           list_element_i[data$subject == subji,][data$trial[data$subject == subji] %in% id_sample[[rep]][[subji]], ]
##         }))
##       } else {
##         do.call(c, lapply(names(id_sample[[rep]]), function(subji) {
##           list_element_i[data$subject == subji][data$trial[data$subject == subji] %in% id_sample[[rep]][[subji]]]
##         }))
##       }
##     })
      
##     ## convert to factor again
##     data_boot = purrr::map_at(data_boot, which(sapply(data, is.factor)), ~ factor(.x))
    
##     ## fit model
##     ## fit1 = pffr(formula = formula, yind=t, data=data_boot)
##     fit1 = pffr(formula = formula, data=data_boot)
##     suppressMessages(c1 <- coef(fit1, seWithMean = TRUE))
##     list(pterms = c1$pterms, smterms = BBmisc::extractSubList(c1$smterms, "coef"))
##   }, mc.cores = ncpus)
##   msg("done sampling ...")
  
##   ## names(boot_out[[1]][[2]])
##   ## boot_out[[1]][[2]][[1]]
  
##   peffects = lapply(boot_out, function(li) {
##     li$pterms
##   })
##   smeffects = setNames(lapply(names(boot_out[[1]]$smterms), function(effect_i) {
##     do.call(cbind, lapply(boot_out, function(li) {
##       li$smterms[[effect_i]]$value
##     }))
##   }), names(boot_out[[1]]$smterms))
  
##   ## return list of matrices (nboot x T) of nboot coefficient functions evaluated at time points 1,...,T
##   rval = list(formula = formula, peffects = peffects, smeffects = smeffects)
##   class(rval) = c(class(rval), "pffr_boot")
##   rval
## }

## #' @export
## print.pffr_boot = function(obj) {
##   cat("Bootstrap results \n")
##   cat("Formula: ", paste(obj$formula), "\n")
##   cat("Simple (time-constant) linear effects: ", names(obj$peffects), "\n")
##   cat("Smooth effects: ", names(obj$smeffects), "\n")
##   print(str(obj,0))
## }

## #' @export
## plot.pffr_boot = function(obj) {
##   neffects = length(obj[["smeffects"]])
  
##   oldpar = par(no.readonly = TRUE)
##   on.exit(par(oldpar))
##   if (neffects %% 2 == 0)
##     par(mfrow = c(neffects / 2, neffects / 2), cex.lab = 3, mar = c(5, 7, 4, 2) + 0.1)
##   else
##     par(mfrow = c((neffects / 2) + 1, neffects / 2))
  
##   hist(sapply(obj[["peffects"]], function(i) i[2, 1]), main = "", xlab = 
##          sub("^.*(c\\((\\w+)\\)).*$", "\\2", as.character(obj$formula)[3], ignore.case = T))
##   for (i in 1:neffects)
##     matplot(obj[["smeffects"]][[i]], type = "l", ylab = names(obj$smeffects)[i])
  
##   invisible(NULL)
## }

## msg = function(text) {
##     message(sprintf("%s | %s", format(Sys.time(), "%d.%b.%Y %X"), text))
## }
