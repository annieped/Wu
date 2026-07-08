# Author: Kaitlin Swinnerton
# Title: HRS memory and dementia imputations - Step 2 - clean data
# Date: 03/08/2022
# Last edited: 
################################################################################
# This code implements the methods described in Wu et. al. (2013) to create 
# measures for dementia probability and cognition in HRS.
#
# Outline
#  1. recode variables
#     sex_male, race (0. white, 1. black, 2. other), proxy_ind, ageyrs_cent70
# variables needed:
#  * proxy_indicator
#  * word_i
#  * word_d
#  * tics_1p
#  * iqcode
#  * prxmem
#  * sex (sex male)
#  * ageyrs_c70
#  * black

# these are the formulas:
# dementia probability
# 	logodds_demprob = 4.608 + 1.889*proxy + 0.933*word_i - 
#       0.266*word_i_sq - 0.797*word_d - 1.075*tics_1p +
#       0.043*tics_1p_sq + 2.22*iqcode + 1.096*prxmem - 
#       0.854*sex + 0.095*ageyrs_cent70 - 0.695*black + 0.543*word_dXsex +
#       1.551*iqcodeXsex
#
# memory score
# 	ms = 0.422 - 1.388*proxy + 0.116*word_i + 0.024*word_d - 
#        0.068*iqcode - 0.21*prxmem - 0.132*sex - 
#        0.067*ageyrs_cent70 - 0.398*black + 0.01*word_dXagecent70 - 
#        0.446*iqcodeXsex + 0.039*proxyXage

################################################################################
# Preamble :  Load libraries, helper functions, data
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
  "here"
)
# load helper functions
source(
  here::here("scripts", "helper_functions", "hrs_helper_functions.R")
  )

# load file created in step 1
d <-readRDS(here::here("data", "02_merged_data.RDS"))
################################################################################
# Step 1 :  Create summary cognition variables 
#
# Variables to create:
#  * word_i & word_i_sq - sum of immediate word recall 
#  * word_d - sum of delayed word recall
#  * tics_1p & tics_1p_sq - summary of scores for all TICS tasks
#    * serial 7s subtraction
#    * backwards counting from 20
#  * iqcode (iqcode_cent5)  
#  * prxmem (prxmem_cent5)  
#  * sex (where male == 1, female == 0)  
#  * ageyrs_cent70  
#  * black 
################################################################################
# make word_i and word_d variables ---------------------------------------------
d <- d %>% 
  mutate(
    across(
      all_of(starts_with("IR_")), 
      ~ as.integer(between(.x, 1, 40))
    ),
    across(
      all_of(starts_with("DR_")), 
      ~ as.integer(between(.x, 1, 40))
    )
  ) %>% 
  mutate(
    # word_i = ifelse(
    #   between(year, 2002, 2016), tr20_1, 
    #   rowSums(across(all_of(starts_with("IR_"))), na.rm = TRUE)
    #   ), 
    word_i = rowSums(across(all_of(starts_with("IR_"))), na.rm = TRUE), 
    word_i_sq = word_i**2,
    # word_d = ifelse(
    #   between(year, 2002, 2016), tr20_2,
    #   rowSums(across(all_of(starts_with("DR_"))), na.rm = TRUE)),
    word_d = rowSums(across(all_of(starts_with("DR_"))), na.rm = TRUE), 
    .before = IR_01
    ) 

# make tics_1p variable --------------------------------------------------------
tics_names_dates <- d %>% 
  select(starts_with("orient_"), starts_with("name_"), -name_scissors) %>% 
  colnames()  

# make TICS summary variables  
d <- d %>% 
  # create serial 7s total score
  mutate(
    # create indicators for correct answers, then sum to make ser7 var
    #   each successive var is the # the respondent said for each subtraction
    #   ser7_1c is correct if respondent said "93"
    #   each successive var is correct if it is 7 less than the previous # said
    ser7_1c = as.integer(ser7_1 == 93),
    ser7_2c = as.integer(ser7_1 - ser7_2 == 7),
    ser7_3c = as.integer(ser7_2 - ser7_3 == 7),
    ser7_4c = as.integer(ser7_3 - ser7_4 == 7),
    ser7_5c = as.integer(ser7_4 - ser7_5 == 7)
  ) %>%
  mutate(
    ser7_total = rowSums(select(., matches("ser7_.c")), na.rm = T),
    ser7_total_m = ifelse(is.na(ser7_1), 5, 0),
    ser7_total_r = ifelse(ser7_1 == 999, 5, 0)
  ) %>% 
  # create indicator variable for correct backwards counting
  # mutate(bwc20 = ifelse(between(year, 2002, 2016), bwc20, as.integer(bwc20_1 == 1))) %>%
  mutate(
    bwc20 = as.integer(bwc20_1 == 1),
    bwc20_m = as.integer(is.na(bwc20_1)),
    bwc20_r = as.integer(bwc20_1 == 9)
    ) %>% 
  # create total scores for naming and orientation questions
  mutate(
    names_dates_total   = rowSums(select(., all_of(tics_names_dates)) == 1),
    names_dates_total_m = rowSums(is.na(select(., all_of(tics_names_dates)))),
    names_dates_total_r = rowSums(select(., all_of(tics_names_dates)) == 9)
  ) %>% 
  mutate(
    tics_1p = rowSums(
      select(., ser7_total, bwc20, names_dates_total), 
      na.rm = TRUE
    ),
    tics_total_m = rowSums(
      select(., ser7_total_m, names_dates_total_m), na.rm = TRUE
      ) + is.na(bwc20),
    tics_total_r = rowSums(
      select(., ser7_total_r, names_dates_total_r), na.rm = TRUE
      ) +
      as.integer(bwc20_1 == 9)
  ) %>% 
  # rescale tics based on missingness - drop if missing > 3 or refused >4
  mutate(
    tics_1p = case_when(
      tics_total_m <= 3 & (tics_total_r <= 4 | is.na(tics_total_r)) ~ 
        tics_1p*13 / (13 - tics_total_m)
    )
  ) 

# make iqcode variable ---------------------------------------------------------
d_proxy <- d %>% filter(proxy == 1) 

d <- d_proxy %>% 
  select(hhidpn, year, proxy, pxy_memory, starts_with("iqcode")) %>% 
  mutate(prxmem_cent5 = ifelse(pxy_memory >= 8, NA, pxy_memory - 5)) %>% 
  pivot_longer(
    cols = starts_with("iqcode"),
    names_to = c(".value", "n"),
    names_pattern = "(.+)_(\\d\\d)"
  ) %>% 
  mutate(
    # indicator variables for missing iqcode_primary variables
    primary_miss = as.integer(iqcode_primary %in% c(7, 8, 9) | is.na(iqcode_primary)),
    iqcode_improve = ifelse(iqcode_improve %in% c(7, 8, 9), 2, iqcode_improve),
    iqcode_decline = ifelse(iqcode_decline %in% c(7, 8, 9), 4, iqcode_decline),
    # code iqcode to take a value of 1-5 based on answer to iqcode_primary
    iqcode = case_when(
      iqcode_primary == 1 ~ iqcode_improve,
      iqcode_primary == 3 ~ iqcode_decline,
      iqcode_primary == 2 ~ 3
    ),
  ) %>% 
  group_by(hhidpn, year) %>% 
  mutate(
    iqcode_score = sum(iqcode, na.rm = TRUE),
    primary_miss_total = sum(primary_miss, na.rm = TRUE),
    # rescale based on missingness and center at 5
    iqcode_cent5 = case_when(
      primary_miss_total <= 3 & iqcode_score != 0 ~ 
        (iqcode_score/(16-primary_miss_total))-5,
    )
  ) %>% 
  ungroup() %>% 
  distinct(hhidpn, year, prxmem_cent5, iqcode_cent5) %>% 
  ungroup() %>% 
  rename(
    iqcode = iqcode_cent5,
    prxmem = prxmem_cent5
    ) %>% 
  right_join(d)

# recode demographic variables -------------------------------------------------
# indicators for sex = male and race = black
# age centered at 70
d <- d %>% 
  mutate(
    sex_male = as.integer(ragender == 1),
    race_black = as.integer(raracem == 2),
    ageyrs_c70 = agey_e - 70,
    # create exact age (years + months) variable to align with SAS code
    # in Aayush's code, he uses ageyrs_c70 as the direct age term in the model,
    # but uses this variable in the age interaction terms 
    age_c70 = (IWYEAR - BIRTHYR) + (IWMONTH - BIRTHMO)/12 - 70
  )

# Final data cleaning steps to align varibles with those used by Wu-------------
# 
# In Wu et. al. (2013), the authors use a missing indicator method to ensure 
# that all proxy and non-proxy responses are available when estimating their 
# regression model. The authors write: 
#   
#   "To retain participants regardless of whether they participated directly or 
#    via proxy in HRS core interviews, we adopted the missing-indicator method 
#    (22). For individuals who completed direct cognitive assessments, the proxy 
#    variables were set to zero. For individuals with proxy assessments, their 
#    direct assessments scores were set to zero."
# 
# Below, we create new versions of these cognitive variables where we recode 
# proxy/non-proxy responses as 0 depending on whether the variable pertains to 
# direct responses/proxy responses. We do so because we need to create a linear 
# combination of these variables to estimate the dementia probability score and 
# the cognition score. Keeping them as missing would not allow the evaluation of
# the linear combination for any observation.

d <- d %>% 
  mutate(
    across(
      c(word_i, word_d, tics_1p),
      ~ ifelse(proxy == 1, 0, .x)
    ),
    across(
      c(iqcode, prxmem),
      ~ ifelse(proxy == 0, 0, .x)
    )
  )

# create quadratic and interaction terms 
d <- d %>% 
  mutate(
    word_i_sq = word_i**2,
    tics_1p_sq = tics_1p**2,
    word_dXsex = word_d * sex_male,
    word_dXage_c70 = word_d * age_c70,
    iqcodeXsex = iqcode * sex_male,
    proxyXage = proxy * age_c70
  )

# keep only variables needed for calculating cognitive measures
d <- d %>%
  select(
    hhidpn, year, proxy,
    sex_male, race_black, age_c70, ageyrs_c70,
    word_i, word_i_sq, word_d, tics_1p, tics_1p_sq,
    iqcode, prxmem,
    word_dXsex, word_dXage_c70, iqcodeXsex, proxyXage
  ) %>%
  distinct() %>% 
  ungroup() %>% 
  arrange(hhidpn, year)

# # save output
d %>% saveRDS(here::here("data", "03_analytic_data.RDS"))
# rm(list = ls())

