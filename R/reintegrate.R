#' "Reintegrate i.e. 
#'
#' @param x an afterGGIR object
#' @param TO the desired resolution ("epoch") in seconds
#' @param ... currently ignored
#'
#' @return
#' @export
reintegrate2 <- function (x, TO = 60,  ...) 
{
  X <- x$ENMO
  Epoch <- attr(x, "wins")[1]
  FACTOR <- TO/Epoch
  foldMeans <- function(x, F = FACTOR, na.rm = FALSE) {
    saba <- length(x)%%F
    if (saba != 0) 
      x <- c(x, rep(NA, F - saba))
    colMeans(matrix(x, nrow = F), na.rm = na.rm)
  }
  foldMax <- function(x, F = FACTOR, na.rm = FALSE) {
    saba <- length(x)%%F
    if (saba != 0) 
      x <- c(x, rep(NA, F - saba))
    apply(matrix(x, nrow = F), 2, max, na.rm = na.rm)
  }
  foldFirst <- function(x, F = FACTOR) {
    LEN <- length(x) %/% F
    x[seq(1, length.out = LEN, by = FACTOR)]
  }
  
  tims <- foldFirst(x$timestamp)
  #  ax <- attributes(x)[c("filename", "folder", "freq", "slip", "wins", "class")]
  res <- data.frame(timestamp = NA, anglez = foldMeans(x$anglez), 
                    ENMO = foldMeans(x$ENMO), nonwearscore = foldMax(x$nonwearscore),
                    clippingscore=foldMax(x$clippingscore), asleep = foldMax(x$asleep))
  res$timestamp[1:length(tims)] <- tims
  structure(res, filename = attr(x, "filename"), folder = attr(x, "folder"),
            freq = attr(x, "freq"), slip = attr(x, "slip"), wins = c(TO, attr(x, "wins")[2:3]))
}
