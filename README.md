# afterGGIR

Functions to simplify working with GGIR. Install this package using:

```
remotes::install_github("lebatsnok/afterGGIR")
# you need "remotes" package to install from github
```

1) `doGGIR` -- runs g.shell.GGIR with "reasonable options" :) 
2) `afterGGIR` -- extracts some important data from the GGIR results folder and makes a single compact *.rmd files for each source file
3) `summarizeGGIR` -- computes some summary statistics and (if you want) saves the result to an excel table

Suppose you have your raw GeneActiv files in a folder called "precious". The analysis will then go as follows:

```
library(afterGGIR)
doGGIR("precious")     # this step takes the most time
                       # GGIR's results will be found in results/output_precious/.........
afterGGIR("precious")  # this crates a folder named "rda" wit one file for each raw file
                       # these rda files contain ENMO aggregated by 1 second plus sleep data from GGIR
summarizeGGIR("rda", toexcel = TRUE)   # a simple day-level summary for all raw files
                                       # if toexcel=TRUE the result is saved as "results-precious.xlsx"
```

You can look at the 1-s data individually using the function Load, e.g.,

```
x <- Load("precious/firstfile.rda")
head(x)
```


... more to follow
