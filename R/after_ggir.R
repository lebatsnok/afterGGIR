#' afterGGIR
#'
#' @param x 
#' @param folder 
#' @param minimize 
#'
#' @return
#' @export
#'
#' @examples
afterGGIR <- function(x, folder = NULL, minimize=FALSE){
  if(!is.null(folder)) foo <- loadg(x, folder) else foo <- x
  
  freq <- as.numeric(as.character(foo$ggir1$I$header["Measurement_Frequency",1]))
  wins <- (foo$ggir1$M$windowsizes)
  mika <- foo$ggir1$M$metalong
  aere <- foo$ggir2$IMP$metashort
  #slip0 <- foo$ggir3$sib.cla.sum ## kestvad mitteaktiivsusperioodid
  slip <- foo$ggir4  ## uneajad päevade kaupa
  posify <- function(.) strptime(., "%Y-%m-%dT%H:%M:%S")
  starta <- posify(aere$timestamp[1])
  startm <- posify(mika$timestamp[1])
  aere$timestamp <- seq(starta, length.out = nrow(aere), by = wins[1])
  mika$timestamp <- seq(startm, length.out = nrow(mika), by = wins[2])
  mika$xmin <- as.character(cut(mika$timestamp, "10 mins"))
  aere$xmin <- as.character(cut(aere$timestamp, "10 mins"))
  aere <- cbind(aere[, c(1,4,2,3)], mika[match(aere$xmin, mika$xmin), c(2,3,4,5, 6, 7)])
  aere$asleep <- 0
  slip$onset <- strptime(with(slip, paste(calendar_date, sleeponset_ts)), "%d/%m/%Y %H:%M:%S")
  slip$end <- with(slip, onset + SptDuration*60*60)
  for(i in 1:nrow(slip)) aere$asleep[aere$timestamp>=slip$onset[i] & aere$timestamp<=slip$end[i]] <- 1
  if(!minimize) aere$day <- as.Date(trunc(aere$timestamp, "days"))
  if(minimize){
    aere$lightmean <- aere$lightpeak <- aere$xmin <- aere$EN <- aere$temperaturemean <- NULL
  }
  
  rownames(aere) <- NULL
  
  structure(aere, 
            filename = basename(foo$ggir1$filename_dir),
            folder = foo$ggir1$filefoldername,
            freq = freq, 
            slip = slip, 
            wins = wins)
}


#' summarizeGGIR
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
summarizeGGIR <- function(x){
  #browser()
  file <- attr(x, "filename")
  # browser()
  id <- strsplit(file, "_")[[1]][1]
  if(!"day" %in% names(x)) x$day <- as.Date(trunc(x$timestamp, "days"))
  xS <- split(x, x$day) 
  res <- do.call(rbind, lapply(xS, function(.){
    day <- .$day[1]
    mean.enmo <- mean(.$ENMO)
    weartime <- table(.$nonwearscore)["0"] / 60
    sleeptime <- table(.$asleep)["1"]/60
    brx <- c(0,52, 192, 540, Inf)/1000
    timeinintensities <- hist(.$ENMO[x$nonwearscore==0], breaks=brx, plot=FALSE)$counts / 60
    tin <- as.data.frame(setNames(as.list(timeinintensities), c("sed", "lig", "mod", "vig")))
    #browser()
    res <- data.frame(day, wd = format(day, "%u"), 
                      mean.enmo, 
                      weartime = unname(weartime), 
                      sleeptime = unname(sleeptime),  
                      tin)
    res
  }))
  
  uni <- with(attr(x, "slip"), data.frame(date2 = as.Date(strptime(calendar_date, "%d/%m/%Y")), sleeponset = sleeponset, wakeup = wakeup, sleepduration = SptDuration,
                                          stringsAsFactors=FALSE))
  offset <- which.min(as.Date(res$day)-uni$date2[1])
  uni0 <- uni[1,]; uni0[] <- NA
  algusse <- do.call(rbind, replicate(offset-1, uni0, simplify = FALSE))
  uni <- rbind(algusse,uni)
  loppu <- do.call(rbind, replicate(NROW(res)-NROW(uni), uni0, simplify = FALSE))
  uni <- rbind(uni, loppu)
  names(uni)[1] <- "date_sleep"
  res <- data.frame(id=id, file=file, res, uni, stringsAsFactors = FALSE)
  rownames(res) <- NULL
  res
}