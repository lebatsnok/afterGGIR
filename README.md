# afterGGIR

Functions to simplify working with GGIR. Install this package using:

```
remotes::install_github("lebatsnok/afterGGIR")
# you need "remotes" package to install from github
```

### Simple workflow

1) `doGGIR` -- runs g.shell.GGIR with reasonable options (check the code for details:) 
2) `afterGGIR` -- extracts some important data from the GGIR results folder and makes a single compact *.rmd file for each source file
3) `summarizeGGIR` -- computes some summary statistics and (if you want) saves the result to an excel table

Suppose you have your raw GeneActiv files in a folder called "precious". The analysis will then go as follows:

```
library(afterGGIR)
doGGIR("precious")     # this step takes the most time
                       # GGIR's results will be found in results/output_precious/.........
afterGGIR("precious")  # this creates a folder named "rda" with one file for each raw file
                       # these rda files contain ENMO aggregated by 1 second plus sleep data from GGIR
                       # it's just the ENMO right now - more might be added as needed
summarizeGGIR("rda", toexcel = TRUE)   # a simple day-level summary for all raw files
                                       # if toexcel=TRUE the result is saved as "results-precious.xlsx"
```

### Working with 1-s data sets

You can look at the 1-s data individually using the function Load, e.g.,

```
x <- Load("precious/firstfile.rda")
head(x) # a few values from the beginnig
tozoo(x)  # make a zoo (time series:) object out of x
```

And you can merge it with experience sampling data using find_windows:

```
find_windows(E=expsamp, A = "rda", timevar = "time", idvar="id")
# Expects a data frame `expsamp` where variable called 'time' is the time of the "beep" and 'id' is respondent's unique # code - and a folder called 'rda' with activity data (result of afterGGIR)
# .. see more with ?find_windows
```


### Resave files with encoding problems

Resaving with `resave()` might save some of the files with encoding problems. For example, the error messages from GGIR with one file were:

```
# Errors and warnings for <FILENAME>$message
# [1] "NA/NaN/Inf in 'x'"
```

When trying to view the same file with `readLines`, I got the following message:

```
readLines("<FILENAME>", 150) # show just 150 initial lines to save some time (the file can be huge)
# Warning messages
# 1: In readLines("<FILENAME>", 150):
# line 26 appears to contain an embedded nul
``` 

.. and similar messages for several other lines. This is a sign of "wrong" encoding which might be repaired by downloading the file again with the same computer you are using to analyze the data -- but that is often not feasible or possible. Some of the problems can be solved by resaving the file to make it less unappetizing for GGIR:

```
resave("folder/oldfilename.bin", "folder/newfilename.bin")
```

Now try GGIR with the resaved file. (And keep the old file at least for a while, until you have made sure that resaving solved the problem.)


### ... more to follow
