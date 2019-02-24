## c:/Dropbox/PublicData/FipsCW/10-download_census_data.R

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

suppressMessages({library(CLmisc); library(tidycensus)})

census_api_key("bb27140497791e254cd9b9c71efa2757b17649ae")

##Total persons
cnty.pop1990 <- get_decennial(geography = "county", variables = "P0010001", year = 1990) %>%
  setDT %>%
  setnames("value", "pop1990") %>%
  .[, GEOID1990 := GEOID] %>%
  .[, variable := NULL]

cnty.pop2000 <- get_decennial(geography = "county", variables = "P001001", year = 2000) %>%
  setDT %>%
  setnames("value", "pop2000") %>%
  .[, GEOID2000 := GEOID] %>%
  .[, variable := NULL]

cnty.pop2010 <- get_decennial(geography = "county", variables = "P001001", year = 2010) %>%
  setDT %>%
  setnames("value", "pop2010") %>%
  setnames("GEOID", "GEOID2010") %>%
  .[, variable := NULL]


saveRDS(cnty.pop1990, "RdsFiles/10-census_cnty_pop_1990.rds")
saveRDS(cnty.pop2000, "RdsFiles/10-census_cnty_pop_2000.rds")
saveRDS(cnty.pop2010, "RdsFiles/10-census_cnty_pop_2010.rds")


