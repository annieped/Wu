# Author: Kaitlin Swinnerton
# Title: HRS memory and dementia imputations 
# Date: 02/23/2022
# Last edited: 07/20/2022
################################################################################
# This code implements the methods described in Wu et. al. (2013) to create 
# measures for dementia probability and cognition in HRS.
#
# To create these measures, we need variables from the HRS CORE data files, 
# the tracker file, and the RAND longitudinal file. Load and filter them all
# in the order listed and merge together.
#
# This script uses helper functions defined in 
# "scripts/helper_functions/hrs_helper_functions.R" to load in raw data, rename,
# columns, and merge into long dataframes.
#
# Outline
#  1. Define columns to pull in and rename from RAND fat files 
#     (also works with HRS core files)
#  2. Run functions from `helper_functions.R` to read in individual files, 
#     rename columns, and merge into one long dataframe
#  3. Load and filter tracker file
#  4. Load and filter RAND longitudinal file
#  5. Merge all together
################################################################################
# Preamble :  Load libraries, helper functions
################################################################################
# load libraries
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}
library(pacman)
p_load(
  # tidyverse packages
  "plyr", "tidyverse", "broom", "glue",
  # project management
  "here", "readr", "haven"
)
# load helper functions
# source(here::here("scripts", "helper_functions", "hrs_helper_functions.R"))
source(here::here("scripts", "helper_functions", "hrs_helper_functions.R"))
################################################################################
# Step 1 :  Load and filter data files - using CORE files
################################################################################

# if you wanted to use the HRS CORE data files instead of the RAND fat files:
#  * Save all needed core data files to: here::here("data", "01_raw_data")
#  * Uncomment this chunk below and comment out the next section "fat files"
#  * Comment out line 247 and uncomment line 250

# list out directories and get list of files needed to read and merge
# da_fps <- list.files(
#   path = here::here("data", "01_raw_data", "CORE_data"), 
#   pattern = "[a-zA-Z]\\d\\d.+.da",
#   full.names = T, ignore.case = T
# )
# dct_fps <- list.files(
#   path = here::here("data", "01_raw_data", , "CORE_data"), 
#   pattern = "[a-zA-Z]\\d\\d.+.dct",
#   full.names = T, ignore.case = T
# )

# fat files
dta_fps <- list.files(
  path = here::here("data", "01_raw_data", "RAND_fat_files"),
  pattern = ".+.dta",
  full.names = TRUE, ignore.case = TRUE, recursive = TRUE
)

# make dictionary of column names ------
# ID needed cols for creating Wu measures
# This named list is structured such that for every column we want to pull out, 
# we give it a new, more meaningful name, and then list all of the columns that
# refer to that variable for each wave. For 2002 and on, the column names stay 
# same, excpet for the letter at the beginning of the column that references the
# wave. Therefore, we can use a wildcard character that we will later use to 
# match to the appropriate wave letter. For 2000, 1998, 1996, and 1995, 
# the variables change, so you must list the variable names for each year.
# The lists read in reverse chronological order from left to right (yes, it 
# would have been better to do it the other way, but I realized that too late,
# so here we are).
col_dict <- list(
  ### Cognition (Section D: 02-present, Sections C and PC for 98-00 )
  # Word recall - tr20
  # pre 2002, have to pull each word individually - pull them out for all years
  IR_01 = c(".D182M1", "G1666M1", "F1491M1",   "E1174M1" , "D1174M1" ), 
  IR_02 = c(".D182M2", "G1666M2", "F1491M2",   "E1174M2" , "D1174M2" ),
  IR_03 = c(".D182M3", "G1666M3", "F1491M3",   "E1174M3" , "D1174M3" ),
  IR_04 = c(".D182M4", "G1666M4", "F1491M4",   "E1174M4" , "D1174M4" ),
  IR_05 = c(".D182M5", "G1666M5", "F1491M5",   "E1174M5" , "D1174M5" ),
  IR_06 = c(".D182M6", "G1666M6", "F1491M6",   "E1174M6" , "D1174M6" ),
  IR_07 = c(".D182M7", "G1666M7", "F1491M7",   "E1174M7" , "D1174M7" ),
  IR_08 = c(".D182M8", "G1666M8", "F1491M8",   "E1174M8" , "D1174M8" ),
  IR_09 = c(".D182M9", "G1666M9", "F1491M9",   "E1174M9" , "D1174M9" ),
  IR_10 = c(".D182M10", "G1666M10", "F1491M10", "E1174M10", "D1174M10"),
  IR_11 = c("G1666M11", "F1491M11", "E1174M11", "D1174M11"),
  # dealyed recall
  DR_01 = c(".D183M1", "G1815M1", "F1640M1",   "E1314M1" , "D1314M1" ),
  DR_02 = c(".D183M2", "G1815M2", "F1640M2",   "E1314M2" , "D1314M2" ),
  DR_03 = c(".D183M3", "G1815M3", "F1640M3",   "E1314M3" , "D1314M3" ),
  DR_04 = c(".D183M4", "G1815M4", "F1640M4",   "E1314M4" , "D1314M4" ),
  DR_05 = c(".D183M5", "G1815M5", "F1640M5",   "E1314M5" , "D1314M5" ),
  DR_06 = c(".D183M6", "G1815M6", "F1640M6",   "E1314M6" , "D1314M6" ),
  DR_07 = c(".D183M7", "G1815M7", "F1640M7",   "E1314M7" , "D1314M7" ),
  DR_08 = c(".D183M8", "G1815M8", "F1640M8",   "E1314M8" , "D1314M8" ),
  DR_09 = c(".D183M9", "G1815M9", "F1640M9",   "E1314M9" , "D1314M9" ),
  DR_10 = c(".D183M10", "G1815M10", "F1640M10", "E1314M10", "D1314M10"),
  DR_11 = c("G1815M11", "F1640M11", "E1314M11", "D1314M11"),
  # 2002 and beyond - can pull total recalled
  tr20_1  = c(".D174"), # QD174 NUMBER GOOD - IMMEDIATE
  tr20_2  = c(".D184"), # QD184 NUMBER GOOD - DELAYED
  # Serial seven subtraction - ser7 
  ser7_1  = c(".D142", "G1806",  "F1631", "E1305", "D1305"), # SERIES MINUS 7- 1
  ser7_2  = c(".D143", "G1807",  "F1632", "E1306", "D1306"), # SERIES MINUS 7- 2
  ser7_3  = c(".D144", "G1808",  "F1633", "E1307", "D1307"), # SERIES MINUS 7- 3
  ser7_4  = c(".D145", "G1809",  "F1634", "E1308", "D1308"), # SERIES MINUS 7- 4
  ser7_5  = c(".D146", "G1810",  "F1635", "E1309", "D1309"), # SERIES MINUS 7- 5
  ## TICS variables
  # backwards counting from 20: (first and second try)
  bwc20_1 = c(".D124", "G1710", "F1535", "E1205", "D1205"), 
  bwc20_2 = c(".D129", "G1733", "F1558", "E1228", "D1228"), 
  # date orientation
  # vars in order:
  #   * TODAYS DATE- MONTH
  #   * TODAYS DATE- DAY
  #   * TODAYS DATE- YEAR
  #   * TODAYS DATE- DAY OF WEEK
  orient_m   = c(".D151", "G1820", "F1645", "E1179", "D1179"), 
  orient_d   = c(".D152", "G1821", "F1646", "E1180", "D1180"), 
  orient_y   = c(".D153", "G1822", "F1647", "E1181", "D1181"), 
  orient_dow = c(".D154", "G1823", "F1648", "E1182", "D1182"), 
  # naming
  # vars in order:
  #   * TOOL USED TO CUT PAPER
  #   * NAME OF PRICKLY DESERT PLANT
  #   * WHO IS THE PRESIDENT OF US
  #   * WHO IS THE VICE-PRESIDENT OF US
  name_scissors  = c(".D155", "G1824", "F1649", "E1301", "D1301"), 
  name_cactus    = c(".D156", "G1825", "F1650", "E1302", "D1302"), 
  name_president = c(".D157", "G1826", "F1651", "E1303", "D1303"), 
  name_vp        = c(".D158", "G1827", "F1652", "E1304", "D1304"), 
  # proxy memory ratings (section PC for earlier years)
  pxy_memory = c(".D501", "G1527", "F1373", "E1056", "D1056"), # RATE MEMORY- PC
  # IQCODE variables
  # iqcode_primary_01-iqcode_primary_16
  #   * RATE R AT REMEMBERING THINGS- PC
  #   * RATE R AT REMEMBERING RECENT EVENTS- PC
  #   * RATE R AT CONVERSATION RECALL- PC
  #   * RATE REMEMBERING OWN PHONE NUM- PC
  #   * REMEMBERING OWN PHONE NUM IMPROVE- PC
  #   * RATE REMEMBERING WHERE THINGS KEPT- PC
  #   * RATE FINDING THINGS IN DIFF PLACES- PC
  #   * RATE WORKING WITH FAMILIAR MACHINES- PC
  #   * RATE LEARNING NEW MACHINES- PC
  #   * RATE LEARNING NEW THINGS IN GENERAL- PC
  #   * RATE ABILITY TO FOLLOW STORY- PC
  #   * RATE MAKING DECISIONS- PC
  #   * RATE HANDLING SHOPPING MONEY- PC
  #   * RATE HANDLING FINANCES- PC
  #   * RATE HANDLING DAILY ARITHMETIC PROBS- PC
  #   * RATE REASONING- PC
  iqcode_primary_01 = c(".D506", "G1543", "F1389", ".1072"), 
  iqcode_primary_02 = c(".D509", "G1548", "F1394", ".1077"),
  iqcode_primary_03 = c(".D512", "G1533", "F1399", ".1082"),
  iqcode_primary_04 = c(".D515", "G1558", "F1404", ".1087"),
  iqcode_primary_05 = c(".D518", "G1563", "F1409", ".1092"),
  iqcode_primary_06 = c(".D521", "G1568", "F1414", ".1097"),
  iqcode_primary_07 = c(".D524", "G1573", "F1419", ".1102"),
  iqcode_primary_08 = c(".D527", "G5178", "F1424", ".1107"),
  iqcode_primary_09 = c(".D530", "G1583", "F1429", ".1112"),
  iqcode_primary_10 = c(".D533", "G1588", "F1434", ".1117"),
  iqcode_primary_11 = c(".D536", "G1593", "F1439", ".1122"),
  iqcode_primary_12 = c(".D539", "G1598", "F1444", ".1127"),
  iqcode_primary_13 = c(".D542", "G1602", "F1448", ".1132"),
  iqcode_primary_14 = c(".D545", "G1605", "F1451", ".1135"),
  iqcode_primary_15 = c(".D548", "G1608", "F1454", ".1138"),
  iqcode_primary_16 = c(".D551", "G1611", "F1457", ".1141"),
  # iqcode_improve_01-iqcode_improve_16
  #   * ORGANIZATION IMPROVED- PC
  #   * REMEMBERING RECENT EVENTS IMPROVED- PC
  #   * REMEMBERING RECENT EVENTS IMPROVED- PC
  #   * REMEMBERING OWN PHONE NUM IMPROVE- PC
  #   * REMEMBERING CURRENT DY/MO IMPROVE- PC
  #   * WHERE THINGS ARE KEPT IMPROVED- PC
  #   * FINDING THINGS IMPROVED- PC
  #   * WORKING WITH FAMILIAR MACHINES IMPR- PC
  #   * LEARNING NEW MACHINES IMPROVED- PC
  #   * LEARNING ABILITY IMPROVE- PC
  #   * ABILITY TO FOLLOW STORY IMPROVE- PC
  #   * MAKE DECISIONS IMPROVE- PC
  #   * HANDLING SHOPPING MONEY IMPROVE- PC
  #   * HANDLING FINANCES IMPROVE- PC
  #   * HANDLING ARITHMETIC PROBLEMS IMPROVE- PC
  #   * REASONING IMPROVE- PC
  iqcode_improve_01 = c(".D507", "G1544", "F1390", ".1073"),
  iqcode_improve_02 = c(".D510", "G1549", "F1395", ".1078"),
  iqcode_improve_03 = c(".D513", "G1554", "F1400", ".1083"),
  iqcode_improve_04 = c(".D516", "G1559", "F1405", ".1088"),
  iqcode_improve_05 = c(".D519", "G1564", "F1410", ".1093"),
  iqcode_improve_06 = c(".D522", "G1569", "F1415", ".1098"),
  iqcode_improve_07 = c(".D525", "G1574", "F1420", ".1103"),
  iqcode_improve_08 = c(".D528", "G1579", "F1425", ".1108"),
  iqcode_improve_09 = c(".D531", "G1584", "F1430", ".1113"),
  iqcode_improve_10 = c(".D534", "G1589", "F1435", ".1118"),
  iqcode_improve_11 = c(".D537", "G1594", "F1440", ".1123"),
  iqcode_improve_12 = c(".D540", "G1599", "1445", ".1128"),
  iqcode_improve_13 = c(".D543", "G1603", "F1449", ".1133"),
  iqcode_improve_14 = c(".D546", "G1606", "F1452", ".1136"),
  iqcode_improve_15 = c(".D549", "G1609", "1455", ".1139"),
  iqcode_improve_16 = c(".D552", "G1612", "F1458", ".1142"),
  # iqcode_decline_01-iqcode_decline_16
  #   * ORGANIZATION WORSE- PC
  #   * REMEMBERING RECENT EVENTS WORSE- PC
  #   * CONVERSATION RECALL WORSE- PC
  #   * REMEMBERING OWN PHONE NUM WORSE- PC
  #   * REMEMBERING CURRENT DY/MO WORSE- PC
  #   * WHERE THINGS ARE KEPT WORSE- PC
  #   * FINDING THINGS WORSE- PC
  #   * WORKING WITH FAMILIAR MACHINES WORSE- PC
  #   * LEARNING NEW MACHINES WORSE- PC
  #   * LEARNING ABILITY WORSE- PC
  #   * ABILITY TO FOLLOW STORY WORSE- PC
  #   * MAKE DECISIONS WORSE- PC
  #   * HANDLING SHOPPING MONEY WORSE- PC
  #   * HANDLING FINANCES WORSE- PC
  #   * HANDLING ARITHMETIC PROBLEMS WORSE- PC
  #   * REASONING WORSE- PC
  iqcode_decline_01 = c(".D508", "G1545", "F1391", ".1074"),
  iqcode_decline_02 = c(".D511", "G1550", "F1396", ".1079"),
  iqcode_decline_03 = c(".D514", "G1555", "F1401", ".1084"),
  iqcode_decline_04 = c(".D517", "G1560", "F1406", ".1089"),
  iqcode_decline_05 = c(".D520", "G1565", "F1411", ".1094"),
  iqcode_decline_06 = c(".D523", "G1570", "F1416", ".1099"),
  iqcode_decline_07 = c(".D526", "G1575", "F1421", ".1104"),
  iqcode_decline_08 = c(".D529", "G1580", "F1426", ".1109"),
  iqcode_decline_09 = c(".D532", "G1585", "F1431", ".1114"),
  iqcode_decline_10 = c(".D535", "G1590", "F1436", ".1119"),
  iqcode_decline_11 = c(".D538", "G1595", "F1441", ".1124"),
  iqcode_decline_12 = c(".D541", "G1600", "F1446", ".1129"),
  iqcode_decline_13 = c(".D544", "G1604", "F1450", ".1134"),
  iqcode_decline_14 = c(".D547", "G1607", "F1453", ".1137"),
  iqcode_decline_15 = c(".D550", "G1610", "F1456", ".1140"),
  iqcode_decline_16 = c(".D553", "G1613", "F1459", ".1143") 
)

# comment out if using core files instead of fat files
d <- build_df(col_dict, dta_fps)

# uncomment below if using core data files instead of RAND fat files
# d <- build_df(col_dict, da_fps, dct_fps)

################################################################################
# Step 2 :  Load and filter tracker file
################################################################################

d_trkr <- read_dta(here::here("data", "01_raw_data", "trk2018tr_r.dta"))

# define columns to keep
time_constant <- c(
  "firstiw", "stratum", "secu",  "birthmo", "birthyr", "study", 
  "wtcohort", "exdeathmo", "exdeathyr", "knowndeceasedmo" 
  )
  
time_varying <- c("iwwave", "iwyear", "iwmonth", "proxy")

d_trkr <- d_trkr %>%
  select(
    hhid, pn, all_of(time_constant), ends_with(time_varying)) %>% 
  rename_with(toupper)

# rename wave specific cols from format (wave.letter)(colname) to colname_year
d_trkr <- d_trkr %>%
  rename_with( ~ rename_wave_cols(.x), ends_with(time_varying))

# pivot longer for merge with d
d_trkr <- d_trkr %>% 
  pivot_longer(
    cols = matches("(.+)(\\d\\d\\d\\d)"),
    names_to = c(".value", "year"),
    # names_pattern = "(.+)(\\d\\d\\d\\d)",
    names_sep = "__",
    values_drop_na = T
  ) %>% 
  select(HHID, PN, year, everything())

################################################################################
# Step 2 :  Load and filter RAND longitudinal file
################################################################################
d_rand <- read_dta(here::here("data", "01_raw_data", "randhrs1992_2018v1.dta"))

# respondant level cols to pull out from file
r_cols_to_pull <- c(
  # covariates, interview status, PN weights
  "agey_m", "proxy", "wtresp", 
  # cognitive task data
  "tr20", "ser7", "bwc20",
  # other
  "agem_e", "agey_e", "iwendy" 
)

# time constant covariates
other_cols <- c(
  "raracem", "rahispan", "raedyrs" , "ragender", "inw" 
  )

# use filter_rand function to pull out desired cols and pivot to long format
d_rlong <- filter_rand(
  d_rand, 
  r_wave_cols = r_cols_to_pull,
  time_invariant_cols = other_cols
)

################################################################################

# merge
d_merged <- d_trkr %>%
  right_join(d, by = c("HHID", "PN", "year")) 

# RAND groups all 1995 AHEAD data into wave 3 
# Correct `year` variable for 1995 and 1996 by merging with the tracker file on 
# IWYEAR and iwendy (interview end yr) variables. Then bind those rows back 
# to the rest of the rand data. Then we can merge with the other dataframes
# on the `year` variable.
# Can't just merge all data on IWYEAR/iwendy because some interviews from the 
# 2016 wave happened in 2018, so those rows would show as part of the 2018 wave
d_rlong <- d_merged %>% 
  filter(year <= 1996) %>% 
  select(HHID, PN, year, IWYEAR) %>% 
  left_join(
    d_rlong %>% 
      filter(year <= 1996) %>% 
      select(-year),
    by = c("HHID", "PN", "IWYEAR" = "iwendy")
  ) %>% 
  select(HHID, PN, hhidpn, year, iwendy = IWYEAR, everything()) %>%
  rbind(
    d_rlong %>% 
      filter(year > 1996) %>% 
      select(HHID, PN, hhidpn, year, iwendy, everything())
  ) %>% 
  arrange(HHID, PN, year) 

# merge with rest of data
d_merged <- d_merged %>% 
  left_join(d_rlong, by = c("HHID", "PN", "year"))

# save
saveRDS(d_merged, here::here("data", "02_merged_data.RDS"))
rm(list = ls())

