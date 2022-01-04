#' Find_windows
#' 
#' Given a folder of preprocessed (with afterGGIR) files and a data table with events,
#' find average and SD of activity (g) preceding each event and time in intensity categories.
#' For example, E may include a number of experience sampling data points, with variable called 
#' "time" indicating the time of the "beep". With default values, the activity from 35 to 5 minutes
#' before the "beep" is analyzed, and mean activity, as well as time in intensity zones (defined by "cuts")
#' is reported.
#'
#' @param E data frame with events data
#' @param A activity data folder (the result of afterGGIR, defaults to "rda") 
#' @param mins width of the activity window (in minutes) i.e. how much time before the event to consider
#' @param codevar name of the ID variable in the data frame E (unique identifier of each person)
#' @param timevar name of the `time` variable in the data frame E (must be of POSIXt type or converted before analysis)
#' @param extramins extra minutes between the end of activity window and the event (a negative number might be used if activity AFTER rather than BEFORE the event is of interest)
#' @param cuts cutoffs for activity categories: a named vector of lower thresholds
#'
#'
#' @return A data frame (E) with extra variables added
#' @export
#'
#' @examples
find_windows <- function(E, A = "rda", mins = 30, codevar="id", timevar = "time", extramins = 5 ,
                         cuts = c(sed=0, lig = 52, mod = 192, vig = 540)){
  require(zoo)
  # Esimene argument (A) -- aktiivsusmonitori andmete kaust 
  # Teine argument (K) -- kogemuse väljavõtte andmetabel
  # Kolmas argument (mins) -- mitu minutit enne signaali algust arvestada  
  # Neljas argument (K.kood) -- kooditunnuse nimi andmetabelis K
  # Viies argument (K.time) -- ajatunnuse nimi andmetabelis K
  # -- eeldame, et ajatunnus on "õigel" kujul (POSIXt)
  # extramins = 5 # selle arvestame maha KV küsimustiku täitmisaja lõpust
  aeg <- E[[timevar]]
  id <- E[[codevar]]
  akna_l6pp <- aeg - extramins*60
  akna_algus <- akna_l6pp - mins*60
  res <- list()
  end <- length(id)
  A <- lapply(dir(A, full.names=TRUE), function(x){
    res <- Load(x)
    id2 <- strsplit(attr(res, "filename"), "_")[[1]][1]
    structure(tozoo(res), id=id2)
  })
  names(A) <- sapply(A, function(x) attr(x, "id")) 
  for(iii in 1:end){
    # `try` on igaks juhuks: kui mõni signaal jääb aktiivsusmonitori kandmise ajast välja,
    # ... siis ei tea, mis tulemuse window annab --> kirjutame tulemuseks NaN
    # -- selda lisakontrolli võib-olla ei ole vaja
    w <- try(window(A[[id[iii]]], start= akna_algus[iii], end = akna_l6pp[iii]))
    if(inherits(w, "try-error")) {
      res[[as.character(iii)]] <- NaN 
    }
    else {
      Nc <- names(cuts)
      Cuts = c(cuts, Inf)/1000 
      res[[as.character(iii)]] <- c(mean_g = mean(w), 
                                    table(cut(w, Cuts, labels=Nc))
      )
    }
  }
  # lisame need tunnused KV andmestikule
  len <- sapply(res, length)
  if(any(len %in% 0)){
    maks <- res[which.max(len)]
    maks[] <- NA
    res[len %in% 0] <- maks
  }
  res <- as.data.frame(do.call(rbind, res))
  E$win_start <- akna_algus
  E$win_end <- akna_l6pp
  E <- cbind(E, res)
  # tagastame KV andmestiku koos lisatunnustega
  E
}