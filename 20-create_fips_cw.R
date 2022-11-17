## ./20-create_fips_cw.R


##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())


##Set wd using the here package
setwd(here::here("./"))

suppressPackageStartupMessages({library(CLmisc); })

##The population data
cnty.pop1990 <- readRDS("RdsFiles/10-census_cnty_pop_1990.rds")
cnty.pop2000 <- readRDS("RdsFiles/10-census_cnty_pop_2000.rds")
cnty.pop2010 <- readRDS("RdsFiles/10-census_cnty_pop_2010.rds")

##Crosswalks based on
##https://www.census.gov/geo/reference/county-changes.html
## Now see https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html

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
  .[GEOID2000 == "51560", GEOID2010 := "51005"] %>%
  .[, GEOID := NULL]



saveRDS(cw.fips.1990.2000, "RdsFiles/20-cw_fips_1990_2000.rds")
saveRDS(cw.fips.2000.2010, "RdsFiles/20-cw_fips_2000_2010.rds")
