#' Resave
#' 
#' Re-saves a text file (which can be a *.bin file from GeneActiv) line by line.
#' This will (in many cases!) get rid of code table conflicts
#'
#' @param IN 
#' @param OUT 
#'
#' @return a value
#' @export
resave <-   function(IN, OUT) {
  CIN <- file(IN, "r")
  COUT <- file(OUT, "w")
  I <- 0
  while ( TRUE ) {
    L <-  readLines(CIN, n = 1)
    if ( length(L) == 0 ) break
    I <- I + 1
    if(I %% 1000 == 0) cat(I, " ... ")
    if(I %% 10000 == 0) cat("\n")
    # browser()
    writeLines(L, COUT)
  }
  close(CIN)
  close(COUT)
}
