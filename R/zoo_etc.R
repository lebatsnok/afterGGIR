
#' tozoo
#'
#' @param x 
#' @param ... 
#'
#' @return a zoo object
#' @export
tozoo <- function(x, ...) {
    #wins <- attr(x, "wins")
    #first <- x$timestamp[1]
    res <- zoo::zoo(x$ENMO, x$timestamp) 
    # system.time(res2 <- zoo(x$ENMO, seq(first, by = wins[1], length.out = NROW(x))))
    res
}
