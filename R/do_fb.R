#' Simple feedback
#'
#' @param DIR 
#' @param ij 
#' @param UNLINK 
#' @param UNLINK.ALL 
#'
#' @return nothing
#' @export
do_fb <- function(DIR = "rda", ij=TRUE, ver = 1, fun = NULL, UNLINK=TRUE, UNLINK.ALL=FALSE){
  tsfn <- ver
  usesweave <- ver %in% 1 || identical(fun, utils::Sweave)
  template <- switch(ver, "1" = "ts1.Rnw", "2"="ts2.Rmd")
  if(is.null(tsfn)) tsfn <- ver
  if(!file.exists(template)) file.copy(system.file(template, package="afterGGIR"), template)
  TSF <- sub("rda", "ts", DIR)
  if(!file.exists(TSF)) dir.create(TSF)
  fns <- dir(DIR, full.names = TRUE)

  for(iii in fns[ij]){
    writeLines(iii, ".filename")
    nfn <- paste0(TSF, "/", strsplit(basename(iii),"_")[[1]][1], ".pdf")
    if(usesweave) {
      # if using Sweave
      try(utils::Sweave(template))
      try({
      tools::texi2pdf("ts1.tex", clean=TRUE)
      file.rename("ts1.pdf", nfn)
      })} else {
        # alternatively, using Markdown
        try(rmarkdown::render(template, output_file = nfn))
      }
  }
  if(usesweave && UNLINK){
    unlink(c("ts1-003.pdf", "ts1-concordance.tex", "ts1.aux", 
                        "ts1.log", "ts1.tex"))
  }
  if(UNLINK.ALL) unlink(template)
}

#' Plot raw data
#'
#' @param x 
#' @param DAYS 
#' @param PDF 
#'
#' @return
#' @export
#'
#' @examples
big_fb <- function(x, DAYS=TRUE, PDF=FALSE){
  brx <- c(0, 52, 192, 540, Inf)/1000
  if(is.character(x)){
    if(file.exists(x)) x <- Load(x) else stop("File does not exist")
  }
  id <- strsplit(attr(x, "filename"), "_")[[1]][1]
  if(PDF) {
    pdf(paste0(id, ".pdf"))
    on.exit(dev.off())
  }
  x$day <- unclass(factor(trunc(x$timestamp, "days")))
  x$date <- trunc(x$timestamp, "days")
  for(iii in unique(x$day)){
    # iii <- 1
    foo <- subset(x, day %in% iii)
    fooz <- tozoo(foo)
    plot(fooz, ylim = 0:1, ylab="ENMO (g)", col="white", xlab="Kellaaeg")
    segments(index(fooz), 0, index(fooz), foo$ENMO, col=ifelse(foo$nonwearscore > 0, "gray", "black"))
    points(foo$timestamp, ifelse(foo$asleep > 0, -0.02, -1), col=ifelse(foo$asleep==0, NA, "red"), pch=22, cex=0.5)
    points(foo$timestamp, ifelse(foo$nonwearscore > 0, -0.01, -1), col=foo$nonwearscore, pch=22, cex=0.5)
    
    abline(h=brx[-1], col="green")
    #browser()
    title(paste(id, foo$date[[1]]))
  }
}

do_fb2 <- function(fn, fbdir = "ts/"){
  if(!file.exists(fbdir)) dir.create(fbdir)
  writeLines(fn, ".filename")
  rmarkdown::render("Rnw/tagasiside.Rmd", output_file = paste0("../", fbdir, sub("\\.rda$", ".pdf", basename(fn))))
}
