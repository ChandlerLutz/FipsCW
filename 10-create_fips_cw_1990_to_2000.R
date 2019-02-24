## c:/Dropbox/PublicData/FipsCW/10-create_fips_cw_1990_to_2000.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-02-24

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())

##Set working directory
wd <- "c:/Dropbox/PublicData/FipsCW/"
if (dir.exists(wd)) {
  setwd(wd)
} else {
  ##wd has the wrong path, Assume only the first folder (e.g. c) is wrong
  temp.wd <- getwd()
  base.folder <- substr(temp.wd, 1, 1)
  wd <- paste0(base.folder, substring(wd, 2))
  setwd(wd)
  rm(temp.wd, base.folder)
}
rm(wd)

suppressMessages({library(CLmisc); })

## --

DT.1990.geog <- fread("data-raw-census/county_pop_1990.csv") %>%
  .[2:.N] %>%
  .[FIPCO != ""]
DT.2000.geog <- fread("data-raw-census/county_pop_1990_with_2000_geographies.csv") %>%
  .[2:.N] %>%
  .[FIPCO != ""]

DT.
