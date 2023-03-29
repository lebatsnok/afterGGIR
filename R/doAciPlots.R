
#' Convert an afterGGIR object to accelerate::acc
#'
#' @param x 
#'
#' @return an accelerate::acc object
#' @export
toacc <- function(x){
  if(is.character(x) && file.exists(x)) x <- afterGGIR::Load(x)
  X <- with(x, data.frame(Activity1 = ENMO, ENMO = ENMO, asleep=asleep, nonwearscore = nonwearscore, clippingscore=clippingscore, anglez = anglez))
  X$Activity1[X$nonwearscore > 0 | X$asleep > 0] <- NA
  a <- attributes(x)
  HD <- list(
    File = a$filename,
    Serial = "", Model="Geneactiv", ActiLife = "GGIR", Firmware = "",
    Start = x$timestamp[1], 
    Axes = 1, 
    Epoch = 1,
    Down = as.POSIXct(NA),  Address = NA, Voltage = NA, Mode = NA, NH = NA,
    End = tail(x$timestamp, 1),
    ID = strsplit(a$filename, "_")[[1]][1]
  )
  structure(list(HD=HD, X=X), class="acc")
}

make.cutoffs <- function(Name, Epoch, ...) structure(list(c(Epoch=Epoch, ...)), names=Name)
cutoffs.ENMO <- make.cutoffs("ENMO", 1, Sedentary = 0, Light = 52/1000, Moderate = 192/1000, Vigorous = 540/1000)


#' MakeAciPlots
#'
#' @param x 
#'
#' @return nothing
#' @export
makeAciPlots <- function(x){
  if(is.character(x)) foo <- Load(x) else foo<-x
  # stringi::stri_locale_list()
  Sys.setlocale("LC_ALL", locale="et_EE.UTF-8")
  # foo$ENMO[foo$asleep==1] <- 0
  foo2 <- toacc(foo)
  foo3 <- summary(foo2, STATS=CNTSTATS, BY="60 mins", report.errors=FALSE, cutoffs=cutoffs.ENMO)
  foo4 <- summarizeGGIR1(foo)
  foo3$day <- as.Date(trunc(foo3$Period, "days"))
  days <- unique(foo3$day)
  #iii <- 2
  for(iii in 1:length(days)){
    toplot <- subset(foo3, day %in% days[iii])
    res <- try(aciPlot2(toplot, cutname="ENMO"), silent=TRUE)
    
    if(inherits(res, "try-error")) next
    legend(0,60, fill=heat.colors(4), legend=rev(c("Istuv", "Kerge", "Mõõdukas", "Tugev")), bty="n", bg="none")
    title(format(days[iii], "%A %d.%m.%Y"))
    st0 <- subset(foo4, date_sleep %in% (days[iii]-1))
    st1 <- subset(foo4, date_sleep %in% days[iii])
    s1 <- if(NROW(st0)) st0$wakeup-24 else NA
    s2 <- if(NROW(st1)) st1$sleeponset else NA
    #segments(0,0,48, 0, col="gray", lwd=10)
    #segments(0,0,s1*2, 0, col="green", lwd=10, lend=2)
    #segments(s2*2,0,48,0, col="green", lwd=10, lend=2)
  }
  foo4
}

#' aciPlot2
#'
#' @param D 
#' @param cutname 
#' @param cats 
#' @param plot 
#' @param select 
#' @param ... 
#'
#' @return nothing (a plot is drawn)
#' @export
aciPlot2 <- function(D, cutname = "Evenson", cats = c("Sedentary", "Light", "Moderate", "Vigorous"), plot=TRUE, select = Wkdy %in% 0:6, ...){
  if(is.acc(D)) {
    CO <- get(paste0("Cutoffs.", cutname), .GlobalEnv)
    D <- summary(D, BY="30 mins", cutoffs = CO)
  }
  nrm <- 360
  cutoffnames <- paste(cutname, cats, sep=".")
  rateoc <- with(D, difftime(Period[2], Period[1], units="mins"))
  nrowpd <- 1440 / as.numeric(rateoc)
  D$day <- as.character(trunc(D $ Period, "days"))
  dt <- table(D$day)
  wrongdays <- dt[dt!=nrowpd]
  throwaway <- which(D$day %in% names(wrongdays))
  if(length(throwaway)) D <- D[-throwaway,]
  
  if(nrow(D) == 0) stop("zero rows left after deleting incomplete days")
  
  hrs <- as.numeric(format(D$Period, "%H"))
  mns <- as.numeric(format(D$Period, "%M"))
  hrfs <- mns / 60
  taim <- hrs + hrfs
  
  splat <- lapply(split(D[,cutoffnames], D$day), as.matrix)
  
  splat <- splat[sapply(splat, sum)>=nrm] 
  if(!length(splat)) stop("zero days left after deleting <360min days")
  
  
  avgd <- Reduce("+", splat) / length(splat)
  rownames(avgd) <- 1:24
  cols <- rev(grDevices::heat.colors(ncol(avgd)))
  tavgd <- t(avgd)
  barplot(tavgd, col=cols, space=0)
}


#' Format dates
#'
#' @param x 
#' @param units 
#' @param style 
#' @param lz 
#'
#' @return a formatted date (string)
#' @export
fmth <- function(x, units="mins", style=";", lz = FALSE) {
  if(units=="mins") x <- x/60
  if(units=="secs") x <- x/3600
  end <- if(style %in% c("h", "t")) "m" else ""
  fmt <- if(lz) "%02i%s%02i%s" else "%2i%s%02i%s"
  sprintf(fmt, trunc(x), style, trunc(x%%1*60), end)
}



#' Ctaplot
#'
#' @param x 
#' @param FUN 
#' @param XLAB 
#' @param YLAB 
#' @param MAIN 
#'
#' @return nothing
#' @export
ctaplot <- function(x, FUN=mean, XLAB="Kellaaeg", YLAB="Aktiivsus", MAIN=NULL){
  if(is.null(MAIN)){
    op <- par(mar=abs(par()$mar-c(0,0,3.5, 1.5)))
    on.exit(par(op))
  }
  x$time <- format(x$timestamp, "%H:%M")
  x$wd <- format(x$timestamp, "%u")
  tp <- zoo::rollmean(with(subset(x, wd %in% 1:5 ), tapply(ENMO, time, FUN)), 60, na.pad=TRUE, align="center")
  nv <- zoo::rollmean(with(subset(x, !wd %in% 1:5 ), tapply(ENMO, time, FUN)), 60, na.pad=TRUE, align="center")
  df <- data.frame(time=names(tp), tp=tp, nv=nv, stringsAsFactors=FALSE)
  df$time2 <- as.POSIXct(paste0("1970-01-01", df$time))
  xmax <- max(c(df$tp, df$nv), na.rm=TRUE)
  xmax <- ceiling(xmax*20)/20
  with(df, plot(time2, tp, ylim=c(0, xmax), type="l", lwd=2, xlab="Kellaaeg", ylab="Aktiivsus (g)"))
  with(df, lines(time2, nv, ylim=c(0, xmax), type="l", lwd=2, col="green"))
}

#' Dorender (chk if needed?)
#'
#' @param fn 
#'
#' @return nothing
#' @export
dorender <- function(fn){
  if(!file.exists("ts")) dir.create("ts")
  save(fn, file="temp.rda")
  output_file <- sub("rda$", "html", basename(fn))
  rmarkdown::render("R/tagasiside.Rmd")
  file.rename(from="R/tagasiside.html", file.path("ts", output_file))
  #, output_dir="ts", output_file=output_file,
   #                 envir = list(filename=fn))
}

