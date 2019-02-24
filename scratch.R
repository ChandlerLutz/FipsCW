## c:/Dropbox/PublicData/FipsCW/scratch.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-02-23

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

suppressMessages({library(CLmisc); library(sf); library(recogeo)})

sf.cnty.1990 <- readRDS("data-raw-shp/20-county1990_shapefile.rds") %>%
  setDT %>%
  .[, state.fips := substr(GEOID, 1, 2)]
sf.cnty.2000 <- readRDS("data-raw-shp/20-county2000_shapefile.rds") %>%
  setDT %>%
  .[, state.fips := substr(GEOID, 1, 2)]
sf.cnty.2010 <- readRDS("data-raw-shp/20-county2010_shapefile.rds") %>%
  setDT %>%
  .[, state.fips := substr(GEOID, 1, 2)]


setdiff(sf.cnty.1990$GEOID, sf.cnty.2000$GEOID)

setdiff(sf.cnty.2000$GEOID, sf.cnty.1990$GEOID)


DT.1990.2000 <- merge(sf.cnty.1990[, .(state.fips, GEOID)],
                      sf.cnty.2000[, .(state.fips, GEOID)],
                      by = c("state.fips", "GEOID"),
                      all = TRUE, allow.cartesian = TRUE)

reconcileGeographies(sf.cnty.1990, sf.cnty.2000, dist_buffer = 10000)


DT.ipums1990.2010 <- fread("data-raw-ipums/nhgis_blk2000_blk2010_ge/nhgis_blk2000_blk2010_ge.csv")

DT <- fread("data-raw-census/county_pop1990.csv") %>%
  .[2:.N]


library(tidycensus)

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

##In the 1990 population, but not in 2000
setdiff(cnty.pop1990$GEOID, cnty.pop2000$GEOID)
cnty.pop1990[cnty.pop1990$GEOID %chin% c(setdiff(cnty.pop1990$GEOID, cnty.pop2000$GEOID))]

setdiff(cnty.pop2000$GEOID, cnty.pop1990$GEOID)


convert_fips_codes_from_1990_to_2000 <- function(fips.codes.1990) {

  ##Dade County renamed to Miami-Dade county: "12025" to "12086"
  fips.codes.1990 <- ifelse(fips.codes.1990 == "12025", "12086", fips.codes.1990)

  ##02231 split to create 02232 and 02282

  ##delete 30113 -- yellowstone

  ##add 51780 to 51083 -- 51083 subsumes 51780

}

cw.fips.1990.2000 <- copy(cnty.pop1990) %>%
  .[, GEOID2000 := GEOID1990] %>%
  setnames("NAME", "NAME1990") %>%
  ##Dade County renamed to Miami-Dade county: "12025" to "12086"
  .[, GEOID2000 := ifelse(GEOID2000 == "12025", "12086", GEOID2000)] %>%
  ##Alaska: 02231 split to create 02232 and 02282
  rbind(., .[GEOID1990 == "02231"]) %>%
  .[GEOID1990 == "02231", GEOID2000 := c("02232", "02282")] %>%
  ##population from
  ##https://www2.census.gov/programs-surveys/popest/tables/1990-2000/estimates-and-change-1990-2000/2000c8_02.txt
  .[GEOID2000 == "02232", pop1990 := 3679] %>%
  .[GEOID2000 == "02282", pop1990 := 725] %>%
  ##Alaska: Add county 02068 from 02290 and 02240
  rbind(., .[GEOID1990 %chin% c("02290", "02240")]) %>%
  .[(GEOID1990 %chin% c("02290", "02240")) & duplicated(GEOID1990), GEOID2000 := "02068"] %>%
  ##population from
  ##https://www2.census.gov/programs-surveys/popest/tables/1990-2000/estimates-and-change-1990-2000/2000c8_02.txt
  .[GEOID1990 == "02240" & GEOID2000 == "02240", pop1990 := 5757] %>%
  .[GEOID1990 == "02290" & GEOID2000 == "02290", pop1990 := 6714] %>%
  ##for the new 02068 county
  .[GEOID1990 == "02240" & GEOID2000 == "02068", pop1990 := 156] %>%
  .[GEOID1990 == "02290" & GEOID2000 == "02068", pop1990 := 1764] %>%
  ##merge yellowstone, 30113, to 30031
  .[GEOID1990 == "30113", GEOID2000 := "30031"] %>%
  ##Add south boston, virginia (51780) to Halifax county (51083) %>%
  .[GEOID1990 == "51780", GEOID2000 := "51083"] %>%
  merge(cnty.pop2000[, .(GEOID2000, NAME2000 = NAME)], by = "GEOID2000", all = TRUE) %>%
  .[, GEOID := NULL] %>%
  setcolorder(c("GEOID1990", "NAME1990", "GEOID2000", "NAME2000", "pop1990"))


setdiff(cnty.pop2010$GEOID, cnty.pop2000$GEOID) %>%
  .[substr(., 1, 2) != "72"]
setdiff(cnty.pop2000$GEOID2000, cnty.pop2010$GEOID2010)

cw.fips.2000.2010 <- copy(cnty.pop2000) %>%
  .[, GEOID2010 := GEOID2000] %>%
  setnames("NAME", "NAME2000") %>%
  ##I honestly couldn't figure
  ##out the new Alaska counties
  ##set them to NA
  .[GEOID2000 %chin% c("02201", "02232", "02280"), GEOID2010 := NA_character_] %>%
  ##the colorado counties  -- merge Adams, Boulder, Jefferson, Weld (2000) --> Broomfield
  .[GEOID2000 %chin% c("08001", "08013", "08059", "08123"), GEOID2010 := "08014"] %>%
  ##Change clifton forge Virginia to be a part of Alleghany County
  .[GEOID2000 == "51560", GEOID2010 := "51005"]



