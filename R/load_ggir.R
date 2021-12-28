#' Load
#'
#' @param file 
#' @param to.list 
#' @param Unlist 
#' @param spray 
#' @param delete.parent 
#' @param Attach 
#'
#' @return
#' @export
#'
#' @examples
Load <- function (file, to.list = TRUE, Unlist = TRUE, spray = FALSE, 
                  delete.parent = FALSE, Attach = FALSE) 
{
  attachYN <- (is.logical(Attach) & isTRUE(Attach)) | is.character(Attach)
  if (attachYN) {
    attachNAME <- if (is.logical(Attach)) 
      date()
    else Attach[1]
  }
  NE <- if (spray) 
    .GlobalEnv
  else if (attachYN) 
    attach(NULL, name = attachNAME)
  else new.env()
  load(file, NE)
  if (attachYN) 
    return(invisible(NULL))
  if (to.list) {
    NE <- as.list(NE)
    if (Unlist & length(NE) == 1) 
      NE <- NE[[1]]
  }
  if (delete.parent) 
    parent.env(NE) <- emptyenv()
  NE
}

loadg <- function(FN, OD="", browse = FALSE){
  DIR <- paste0("results/output_", OD)
  if(is.character(FN)){
    FN1 <- paste0("meta_", FN)
  } else {
    FN1 <- dir(file.path(DIR, "meta/basic"))[[FN]]
    FN1 <- sub("\\.RData$", "", sub("^meta_", "", FN1)) 
  }
  P1 <- file.path(DIR, "meta")
  PS <- sapply(c("basic", "ms2.out", "ms3.out", "ms4.out", "ms5.out"), function(x) file.path(P1,x))
  FNS <- rep(FN1, 5)
  FNS[1] <- paste0("meta_", FNS[1])
  FNS <- paste0(paste(PS, FNS, sep ="/"), ".RData")
  if(!all(sapply(FNS, file.exists))) {
    whim <- paste(names(which(!sapply(FNS, file.exists))), collapse="|")
    stop("Some .RData files missing from GGIR output:  ", whim, " ", call.=FALSE)
    }
  RES <- sapply(FNS, Load)
  ggid <- names(RES); names(RES) <- paste0("ggir", 1:5)
  RES$file <- FN1
  RES$fileinfo <- strsplit(FN1, "_|\\.")[[1]]
  
  if(browse) browser()
  RES
}