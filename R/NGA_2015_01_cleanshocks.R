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
library(data.table)
library(tidytext)

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
  filter(survey_id == svy_id) %>% 
  select(-variable, -description_full) %>% 
  mutate(shk_id = as.numeric(shk_id),
         description = str_replace_all(description, "[!#$%()*,.:;<=>@^_`|~.{}]", '')) # remove special characters.


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
         shk_desc = str_trim(str_to_lower(shock_desc)),
         shk_desc = str_replace_all(shk_desc, "[!#$%()*,.:;<=>@^_`|~.{}]", ''),
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

shk14 = shk %>% 
  mutate(shk_year = 2014) %>%
  rename(shk_year_val = `2014`) %>% 
  select(-`2015`, -`2016`)

shk15 = shk %>% 
  mutate(shk_year = 2015) %>%
  rename(shk_year_val = `2015`) %>% 
  select(-`2014`, -`2016`)

shk16 = shk %>% 
  mutate(shk_year = 2016) %>%
  rename(shk_year_val = `2016`) %>% 
  select(-`2015`, -`2014`)


shk = bind_rows(shk14, shk15, shk16)

# remove the copies of the unmutated vars
shk = shk %>% select(hhid, ea,  
                     zone, state, lga, sector,
                     shk_id, shk_desc,
                     shocked, shk_freq, 
                     shk_year, shk_year_val, shk_signif,
                     cope_id)

# NOTE: shock_other contains the description of the shocks that are labeled "ohter"
# n = 44; ignored for now, but some could be re-classified into existing categories, e.g. "death of child"


# classify shocks ---------------------------------------------------------
shk = left_join(shk, shk_dict, by = 'shk_id')

shk = shk %>% 
  rowwise() %>%
  mutate(desc_match = description %like% shk_desc)

shk = shk %>% ungroup()

shk %>% filter(desc_match == FALSE) %>% count(shk_desc, description)


if(nrow(shk %>% filter(desc_match == FALSE)) > 0) {
  warning('shock codes seem to have changed.  Investigate!')
}
# check that data labels match lookup info
# NOTE: if you don't replace the special characters, %like% will fail, even if the content is the same.

View(
shk %>% 
  filter(shk_year == 2014) %>% 
  # collapse down to shock category level: how many times did it occur for each hh?
  group_by(sector, hhid, cause_cat) %>% 
  summarise(tot = sum(shocked, na.rm = TRUE)) %>% 
  # count times each shock category occurred
  ungroup() %>% count(sector, cause_cat, tot > 0) %>% 
  ungroup() %>% group_by(sector, cause_cat) %>% 
  # calculate percent of observations
  mutate(pct = n/sum(n)) %>% filter(`tot > 0` == TRUE) %>% arrange(cause_cat,desc(n)) 
)
# merge data together -----------------------------------------------------


