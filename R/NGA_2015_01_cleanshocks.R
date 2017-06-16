# setup --------------------------------------------------------------------

svy_dir = '~/LSMS/NGA_2015_LSMS/'
base_dir = '~/GitHub/badsurprises/'

country = 'NGA'
svy_year = 2016
svy_id = paste0(country, svy_year)

library(tidyverse) # version 1.1.1
library(haven) # version 1.0.0
library(lubridate) # version 1.6.0
library(stringr) # version 1.2.0
library(readxl) # version 1.0.0

source('R/factorize.R')




# import household svy information ---------------------------------------
hh_file = 'secta_harvestw3.dta'

hh_raw = read_dta(paste0(svy_dir, hh_file))

# pull out relevant vars; rename and de-label them.
hh = hh_raw %>% 
  # select raw variables
  select(
    # sampling
    hhid, ea, wt_w1_w2_w3, wt_wave3,
    # geographic info
    zone, state, lga, sector, 
    # interview
    saq11, saq12, contains('saq13'), contains('saq16'), contains('saq19'),
    
    # panel
    tracked_obs
  ) %>% 
  
  mutate(
    # convert dates
    idate1 = dmy(saq13),
    iday1 = saq13d,
    imonth1 = saq13m,
    iyear1 = saq13y,
    
    idate2 = dmy(saq16),
    iday2 = saq16d,
    imonth2 = saq16m,
    iyear2 = saq16y,
    
    idate2 = dmy(saq19),
    iday2 = saq19d,
    imonth2 = saq19m,
    iyear2 = saq19y,
    
    # rename
    enumerator = saq11, supervisor = saq12,
    
    # convert urban/rural to binary
    rural = ifelse(sector == 2, 1, ifelse(sector == 1, 0, NA)),
    
    # !!!! MAKE SURE IS CORRECT
    # flip panel
    ptrack = ifelse(tracked_obs == 0, 1, 0)
  ) %>% 
  
  # convert labelled values to strings
  factorize(hh_raw, 'zone', 'admin1') %>% 
  factorize(hh_raw, 'state', 'admin2') %>% 
  factorize(hh_raw, 'lga', 'admin3') %>% 
  # strip out number codes from region names
  mutate(admin1 = str_replace_all(admin1, '[0-9]', ''),
         admin2 = str_replace_all(admin2, '[0-9]', ''),
         admin3 = str_replace_all(admin3, '[0-9]', ''),
         admin1 = str_to_title(str_replace_all(admin1,'\\. ', '')),
         admin2 = str_to_title(str_replace_all(admin2,'\\. ', '')),
         admin3 = str_to_title(str_replace_all(admin3,'\\. ', '')))

# remove the copies of the unmutated vars
hh = hh %>% select(hhid, ea, wt_w1_w2_w3, wt_wave3, 
                   contains('i'), enumerator, supervisor, ptrack,
                   zone, state, lga, sector,
                   contains('admin'), rural)


# import geographic info --------------------------------------------------

geo_file = 'NGA_HouseholdGeovars_Y3.dta'

geo = read_dta(paste0(svy_dir, geo_file))

# import FTF ZOI
# spatial join to FTF

# import shock classification tables --------------------------------------
shk_dict = read_excel(paste0(base_dir, 'processeddata/shock_codes.xlsx')) %>% 
  filter(survey_id == svy_id)


# import shock info -------------------------------------------------------
shk_raw = read_dta(paste0(svy_dir, 'sect15a_harvestw3.dta'))


shk = shk_raw %>% 
  mutate(svy_id = svy_id,
         country = country,
         svy_year = svy_year) %>% 
  select(hhid, ea, 
         # geo
         zone, state, lga, sector,
         # shocks
         shock_cd, shock_desc,
         s15aq1, s15aq2, contains('s15aq3'), s15aq4, s15aq5a
         ) %>% 
  mutate(shk_id = shock_cd,
         shk_desc = shock_desc,
         # Did shock happen in last 2 y (since 2014)?
         shocked = ifelse(s15aq1 == 1, 1, ifelse(s15aq1 == 2, 0, NA)),
         shk_freq = s15aq2,
         # Did the shock happen in year X?
         `2014` = ifelse(s15aq3a == 'X', 1, 0),
         `2015` = ifelse(s15aq3b == 'X', 1, 0),
         `2016` = ifelse(s15aq3c == 'X', 1, 0),
         shk_signif = s15aq4,
         cope_id = s15aq5a
  )

shk = shk 
x=x%>% gather(shk_year, shk_year_val, `2014`)

# remove the copies of the unmutated vars
shk = shk %>% select(hhid, ea,  
                   zone, state, lga, sector,
                   shk_id, shk_desc,
                   shocked, shk_freq, 
                   shk_year, shk_signif,
                   cope_id)

# NOTE: shock_other contains the description of the shocks that are labeled "ohter"
# n = 44; ignored for now, but some could be re-classified into existing categories, e.g. "death of child"

# check that data labels match lookup info

# merge data together -----------------------------------------------------


