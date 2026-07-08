# Author: Kaitlin Swinnerton
# Title: HRS memory and dementia imputations - Step 3 - create Wu measures
# Date: 2022-07-14
# Last edited: 
################################################################################
# This code implements the methods described in Wu et. al. (2013) to create 
# measures for dementia probability and cognition in HRS.
#
# This script takes a cleaned dataset and creates the dementia probability and 
# memory scores.
# 
# These are the formulas:
# 
# dementia probability
# 	logodds_demprob = 4.608 + 1.889*proxy + 
#       0.933*word_i - 0.266*word_i_sq - 0.797*word_d - 
#       1.075*tics_1p + 0.043*tics_1p_sq + 
#       2.22*iqcode + 1.096*prxmem - 
#       0.854*sex_male + 0.095*age_c70 - 0.695*race_black + 
#       0.543*word_dXsex + 1.551*iqcodeXsex
#
# memory score
# 	ms = 0.422 - 1.388*proxy + 
#        0.116*word_i + 0.024*word_d - 
#        0.068*iqcode - 0.21*prxmem - 
#        0.132*sex_male - 0.067*age_c70 - 0.398*black + 
#        0.01*word_dXage_c70 - 0.446*iqcodeXsex + 0.039*proxyXage

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
  "plyr", "tidyverse", "broom", "glue", "readr",
  # project management
  "here"
)

# load file created in step 1
d <-readRDS(here::here("data", "03_analytic_data.RDS"))
################################################################################
# Create dementia probability and memory scores---------------------------------

d <- d %>% 
  mutate(
    	logodds_demprob = 4.608 + 1.889*proxy +
          0.933*word_i - 0.266*word_i_sq - 0.797*word_d -
          1.075*tics_1p + 0.043*tics_1p_sq +
          2.22*iqcode + 1.096*prxmem -
    	  0.854*sex_male + 0.095*ageyrs_c70 - 0.695*race_black +
          0.543*word_dXsex + 1.551*iqcodeXsex,
    	demprob = exp(logodds_demprob) / (1 + exp(logodds_demprob)),
    	ms = 0.422 - 1.388*proxy + 
    	  0.116*word_i + 0.024*word_d -
    	  0.068*iqcode - 0.21*prxmem -
    	  0.132*sex_male - 0.067*ageyrs_c70 - 0.398*race_black +
        0.01*word_dXage_c70 - 0.446*iqcodeXsex + 0.039*proxyXage
  )

# save output
if(!dir.exists("output")){
  dir.create("output")
}

# save output as .RDS and .csv
d %>% saveRDS(here::here("output", "Wu2013-cognitive-measures_1996-2018.RDS"))
d %>% write_csv(here::here("output", "Wu2013-cognitive-measures_1996-2018.csv"))

