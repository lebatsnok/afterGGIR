#' afterGGIR
#'
#' @param datadir Name of the raw data folder
#' @param sib Set TRUE to include a table of sustained inactivity periods from GGIR output
#' @param verbose set FALSE to suppress bloody messages
#'
#' @return
#' @export
afterGGIR <- function(datadir, sib=FALSE, verbose=TRUE){
  ggirrdirr <- paste0("results/output_", datadir, "/meta/basic")
  if(!file.exists(ggirrdirr)) stop("Folder ", ggirrdirr, " does not exist, check your working directory using getwd() if you think it should :-)")
  ggirrikaust <- paste0(ggirrdirr, "/")
  if(!file.exists("rda")) dir.create("rda")
  for(iii in 1:length(dir(ggirrdirr))) {
    #print(iii)
    foo <- try(decodeGGIR(iii, datadir, sib=sib, minimize=TRUE))
    if(verbose) cat(iii, "\tProcessing GGIR's output ... ")
    if(inherits(foo, "try-error")){
      fn <- foo[[1]]
      if(verbose) cat(fn, "\n")
    } else {
      save(foo, file=paste0("rda/", attr(foo, "filename"), ".rda"))
      fn <- attr(foo, "filename")
      if(verbose) cat(fn, " ... OK \n")      
    }
  }
}

#' decode GGIR
#' 
#' Lorem ipsum
#'
#' @param x Number or name of the file to be read in
#' @param folder Name of the raw data folder
#' @param minimize set TRUE to forget some of the data (mean light, temperature etc)
#' @param sib set TRUE to report sustained inactivity periods (attr(res, "sib"))
#'
#' @return
#' @export
decodeGGIR <- function(x, folder = NULL, minimize=FALSE, sib=TRUE){
  if(!is.null(folder)) foo <- loadg(x, folder) else foo <- x
  
  freq <- as.numeric(as.character(foo$ggir1$I$header["Measurement_Frequency",1]))
  wins <- (foo$ggir1$M$windowsizes)
  mika <- foo$ggir1$M$metalong
  aere <- foo$ggir2$IMP$metashort
  if(sib) inactivity_periods <- foo$ggir3$sib.cla.sum ## kestvad mitteaktiivsusperioodid
  slip <- foo$ggir4$nightsummary  ## uneajad päevade kaupa
  sleepOK <- !identical(slip, "missing")  ## kas GGIRRi uneanalüüs õnnestus??
  posify <- function(.) strptime(., "%Y-%m-%dT%H:%M:%S")
  starta <- posify(aere$timestamp[1])
  startm <- posify(mika$timestamp[1])
  aere$timestamp <- seq(starta, length.out = nrow(aere), by = wins[1])
  mika$timestamp <- seq(startm, length.out = nrow(mika), by = wins[2])
  mika$xmin <- as.character(cut(mika$timestamp, "10 mins"))
  aere$xmin <- as.character(cut(aere$timestamp, "10 mins"))
  aere <- cbind(aere[, c(1,4,2,3)], mika[match(aere$xmin, mika$xmin), c(2,3,4,5, 6, 7)])
  aere$asleep <- 0
  if(sleepOK){
    slip$onset <- strptime(with(slip, paste(calendar_date, sleeponset_ts)), "%d/%m/%Y %H:%M:%S")
    slip$end <- with(slip, onset + SptDuration*60*60)
    for(i in 1:nrow(slip)) aere$asleep[aere$timestamp>=slip$onset[i] & aere$timestamp<=slip$end[i]] <- 1
  } else {
    aere$asleep <- NA
  }
  
  if(!minimize) aere$day <- as.Date(trunc(aere$timestamp, "days"))
  if(minimize){
    aere$lightmean <- aere$lightpeak <- aere$xmin <- aere$EN <- aere$temperaturemean <- NULL
  }
  
  rownames(aere) <- NULL
  
  structure(aere, 
            filename = basename(foo$file),
            folder = foo$ggir1$filefoldername,
            freq = freq, 
            slip = slip, 
            wins = wins,
            sib = if(sib) inactivity_periods else NULL,
            class = c("afterGGIR", "data.frame"))
}

#' summarizeGGIR
#'
#' @param fn 
#' @param toexcel 
#'
#' @return
#' @export
#'
#' @examples
summarizeGGIR <- function(fn, toexcel = FALSE){
  filelist <- dir(fn, full.names = TRUE)
  res <- lapply(filelist, function(x) summarizeGGIR1(Load(x)))
  res <- do.call(rbind, res)
  if(toexcel){
    outname <- paste0("results-", fn, ".xlsx")
    openxlsx::write.xlsx(res, outname)
    cat("Results written to ", outname)
  }
  res
}


#' summarizeGGIR1
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
summarizeGGIR1 <- function(x){
  #browser()
  file <- attr(x, "filename")
  # browser()
  id <- strsplit(file, "_")[[1]][1]
  if(!"day" %in% names(x)) x$day <- as.Date(x$timestamp)
  stats <- function(.){
    day <- .$day[1]
    mean.enmo <- mean(.$ENMO)
    weartime <- table(.$nonwearscore)["0"] / 60
    awakeweartime <- sum(.$nonwearscore %in% "0" & !.$asleep %in% "1")/60
    sleeptime <- table(.$asleep)["1"]/60
    brx <- c(0,52, 192, 540, Inf)/1000
    timeinintensities <- hist(.$ENMO[.$nonwearscore==0 & !.$asleep %in% "1"], breaks=brx, plot=FALSE)$counts / 60
    tin <- as.data.frame(setNames(as.list(timeinintensities), c("sed", "lig", "mod", "vig")))
    #browser()
    res <- data.frame(day, wd = format(day, "%u"), 
                      mean.enmo, 
                      weartime = unname(weartime), 
                      awakeweartime = unname(awakeweartime),
                      sleeptime = unname(sleeptime),  
                      tin)
    res
  }
  
  
  xS <- split(x, x$day)
  res <- do.call(rbind, lapply(xS, stats))
  
  xslip <- attr(x, "slip")
  
  if(!identical(xslip, "missing") && is.data.frame(xslip) && nrow(xslip)>0){
    uni <- with(xslip, data.frame(date2 = as.Date(strptime(calendar_date, "%d/%m/%Y")), sleeponset = sleeponset, wakeup = wakeup, sleepduration = SptDuration,
                                          stringsAsFactors=FALSE))
    offset <- which.min(as.Date(res$day)-uni$date2[1])
    uni0 <- uni[1,]; uni0[] <- NA
    algusse <- do.call(rbind, replicate(offset-1, uni0, simplify = FALSE))
    uni <- rbind(algusse,uni)
    loppu <- do.call(rbind, replicate(NROW(res)-NROW(uni), uni0, simplify = FALSE))
    uni <- rbind(uni, loppu)
    names(uni)[1] <- "date_sleep"
  } else {
    naa <- rep(NA, NROW(res))
    uni <- data.frame(date_sleep=naa,	sleeponset=naa,	wakeup=naa,	sleepduration=naa)
  }
  res <- data.frame(id=id, file=file, res, uni, stringsAsFactors = FALSE)
  rownames(res) <- NULL
  res
}
