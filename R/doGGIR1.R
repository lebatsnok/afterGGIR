
#' doGGIR one file at a time
#'
#' @param indir Folder name (*.bin files)
#' @param III Integer vector - rank number[s] of files to be processed. Can be NULL (default) - in that case, all files in the folder will included
#'
#' @return system time
#' @export
doGGIR1 <- function(indir, III = NULL, SINK = TRUE) {
  system.time({
    if(!file.exists("results")) dir.create("results")
    FILES <- dir(indir)
    if(is.null(III)) {
      MIN <- 1
      MAX <- length(dir(indir))
    } else {
      MIN <- min(III)
      MAX <- max(III)
    }
    for(JJJ in MIN:MAX){
      cat("---\nProcessing file no. ", JJJ, ": ", FILES[JJJ], "\n")
      if(SINK) {
        sink(paste0("log-", indir, ".txt"), append = TRUE)
        cat("---\nProcessing file no. ", JJJ, ": ", FILES[JJJ], "\n")
      }
      res <- try(GGIR(
        f0 = JJJ, f1 = JJJ,
        # part 1
        #This function will pre-process accelerometer raw data to summarize and clean the signal
        # Rowlands et al http://dx.doi.org/10.1249/MSS.0000000000000978 (on GGIR with Actigraph vs genectiv)
        datadir= indir,
        outputdir= "results",  # see kaust peab juba olemas olema!
        # f0=1,f1=1,   # mitmendast failist alustab ja mitmendaga l6petab
        windowsizes = c(1,600,3600), 
        # w1: for acceleration and angle metrics; 
        # w2 non-wear and signal clipping; 
        # w3 is the window length of data used for non-wear detection and by default 3600 seconds.
        # siit esimest komponenti võiks muuta-- praegu 1 sekund, aga võiks panna 5, 10 või 15, et läheks kiiremini!
        desiredtz = "Europe/Tallinn",
        chunksize=1,  # from 0 to 1, fraction of 24h
        studyname='AUU',
        do.enmo = TRUE, 
        do.bfen = FALSE, # band pass filtering followed by EN
        # see also https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0061691 on metrics
        do.mad = FALSE,
        # see https://www.nature.com/articles/s41598-020-67983-7 on mad
        do.anglex = FALSE,
        do.angley = FALSE,
        do.anglez=TRUE, # used in sleep analysis, (<5deg == sleeping?)
        do.cal = TRUE,
        # calibration method van Hees et al 2015 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4187052/
        dynrange=6,
        # myfun = something # see ExternalFunction.pdf vignette
        # results: OD/output_data/meta/basic/meta__FN....RData
        # components: $M$metashort, $M$metalong; $I - headerinfo; $C - calibration data
        # $metashort components: timestamp, enmo, anglez (missing: x,y,z, )
        
        # part2 -----------------------------------------------------------------
        # This function will provide some physical activity descriptors
        # Incorporation(or not) of detected nonwear times + descriptive summary
        
        do.imp=TRUE,  # impute - using g.impute
        strategy = 1,   #  = 1 from hrs.del.start to hrs.del.end + maxdur [days]. = 2 1st midnight to last midnight. 
        # = 3 most active X [ndayswindow] days;  = 4 to only use the data after the first midnight. See also g.impute 
        hrs.del.start = 0, hrs.del.end = 0, 
        maxdur = 0, # unknown
        includedaycrit = 16,
        ilevels = seq(0, 1000, by = 50),
        qwindow=c(0,24), qlevels = c(960/1440,   #M1/3 (8h)  # quantiles
                                     1320/1440,  #M120
                                     1380/1440,  #M60
                                     1410/1440,  #M30
                                     1438/1440), #M2,
        mvpathreshold = c(100),  # this is in milli-g i.e ENMO*1000 units
        boutcriter = 0.8,
        mvpadur = c(1,5,10),
        bout.metric = 4,
        iglevels = c(seq(0,4000,by=25),8000),
        # epochvalues2csv = TRUE,  # exports epox to csv
        
        # results: results/outputdata/results/par2_daysummary; part2_summary.csv
        
        # g.part3 -----------------------------------------------------------------
        #Sleep detection
        
        anglethreshold = 5, timethreshold = 5,
        do.part3.pdf=TRUE,
        
        # g.part4 -----------------------------------------------------------------
        #Sleep period time detection
        
        do.visual = TRUE,
        def.noc.sleep=1,
        
        # g.part5 -----------------------------------------------------------------
        #Merge physical activity and sleep data
        # Migueles et al 2019 https://www.nature.com/articles/s41598-019-54267-y --> need different cutpoints
        # for dominant and nondominant wrist!!!!
        # activitycounts https://cran.r-project.org/web/packages/activityCounts/index.html
        # Migueles et al Sci reports 2019 cutpoints for dominant wrist: 
        #       sedentary time (<50 mg), light PA (50–110 mg), moderate PA (110–440 mg) and vigorous PA (≥440 mg).
        # Migueles original cut points validated in non-dominant wrist (viitab 8, 11 allpool): 45/100/430
        # seega: 
        #                non-
        #            dominant   dominant
        # -------------------------------
        # Light            45         50
        # Moderate        100        110
        # Vigorous        430        440
        # ------------------------------
        # 8: Hildebrand et al - age group comparability https://pubmed.ncbi.nlm.nih.gov/24887173/
        #       (wrist output > hip, high agreement btw GA and AG but higher for adults than for children)
        # 11: Hildebrand et al 2017 https://pubmed.ncbi.nlm.nih.gov/24887173/  
        timewindow = c("MM"), # MM- midnight2midnight, WW - waking time 2 waking time, c("MM", "WW")
        threshold.lig = c(40), threshold.mod = c(100), threshold.vig = c(400), 
        boutdur.in = c(10,20,30), boutdur.lig = c(1,5,10), boutdur.mvpa = c(1,5,10), 
        boutcriter.in = 0.9, boutcriter.lig = 0.8, boutcriter.mvpa = 0.8,
      ))
      if(inherits(res, "try-error")) cat("\n---", "error with ", FILES[JJJ], "---\n")
      if(SINK)  {
        sink(NULL)
        if(inherits(res, "try-error")) cat("\nerror with ", FILES[JJJ], ":\n", res[1], "\n")
      }
    } # END FOR
  })  # CLOSING BRACKETS FOR SYSTEM:TIME
}
