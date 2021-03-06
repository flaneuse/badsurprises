# NGA_2013_01_cleanhh.R ---------------------------------------------------
# Module to import and clean the household-level data from the  
# 2013 World Bank Living Standards Measurement Study (LSMS)
# Main purpose is to bring in household-level variables for a 
# characterization of shocks across time and geography.
# Household base information includes interview dates, sampling weights,
# basic description of the household head
#
# LSMS data pulled from the World Bank data sets in mid-June 2017
# http://microdata.worldbank.org/index.php/catalog/lsms
#
# Laura Hughes, USAID | GeoCenter, lhughes@usaid.gov, 22 June 2017


# setup -------------------------------------------------------------------
setwd('~/GitHub/badsurprises/R/')
source('NGA_2013_00_setup.R')


# import household svy information ---------------------------------------
# base file; contains interview dates, etc.
hh_file1 = 'secta_harvestw2.dta'

# individual roster; Nigeria doesn't have household head info, so pulling from the indiv panel
hh_file2 = 'sect1_harvestw2.dta'

hh_base = read_dta(paste0(svy_dir, hh_file1))

indiv_raw = read_dta(paste0(svy_dir, hh_file2))


# clean hh cover sheet ----------------------------------------------------
!! Admin 3 screwed up
# pull out relevant vars; rename and de-label them.
hh = hh_base %>% 
  # select raw variables
  select(
    # sampling
    hhid, ea, contains('wt'),
    # geographic info
    zone, state, lga, sector, 
    # interview
    saq11, saq12, contains('saq13'), contains('saq17'), contains('saq21'),
    
    # 0 = interviewed in main survey; 1 = tracked and interviewed
    # assuming moved or split since initial interview.
    tracked_obs
  ) %>% 
  
  mutate(
    # convert dates
    idate1 = dmy(paste(saq13d, saq13m, saq13y, sep = '-')),
    iday1 = saq13d,
    imonth1 = saq13m,
    iyear1 = saq13y,
    
    idate2 = dmy(paste(saq17d, saq17m, saq17y, sep = '-')),
    iday2 = saq17d,
    imonth2 = saq17m,
    iyear2 = saq17y,
    
    idate3 = dmy(paste(saq21d, saq21m, saq21y, sep = '-')),
    iday3 = saq21d,
    imonth3 = saq21m,
    iyear3 = saq21y,
    
    # rename
    enumerator = saq11, supervisor = saq12,
    
    # convert urban/rural to binary
    rural = ifelse(sector == 2, 1, ifelse(sector == 1, 0, NA))
  ) %>% 
  
  # convert labelled values to strings
  factorize(hh_base, 'zone', 'admin1') %>% 
  factorize(hh_base, 'state', 'admin2') %>% 
  factorize(hh_base, 'lga', 'admin3') %>% 
  # strip out number codes from region names
  mutate(admin1 = str_replace_all(admin1, '[0-9]', ''),
         admin2 = str_replace_all(admin2, '[0-9]', ''),
         # admin3 = str_replace_all(admin3, '[0-9]', ''),
         admin1 = str_to_title(str_replace_all(admin1,'\\. ', '')),
         admin2 = str_to_title(str_replace_all(admin2,'\\. ', '')),
         admin3 = str_to_title(admin3))

# remove the copies of the unmutated vars
hh = hh %>% select(hhid, ea, wt_w1_w2_w3, wt_wave3, 
                   contains('i'), enumerator, supervisor, tracked_obs,
                   zone, state, lga, sector,
                   contains('admin'), rural)


# clean individual data ---------------------------------------------------
# Compared to 2015 data: no data on language spoken, mobile, internet availablity

indiv = indiv_raw %>% 

  select(
    # hh id
    hhid, ea,
    # geography
     zone, state, lga, sector,
    # demographics
    s1q3, # relationship to head
    s1q2, # sex
    s1q4, # age
    s1q8 # polygamous
    # s1q12a, # primary language spoken; not available in 2013 data
    
    # connectivity
    # s1q12b, # mobile; not available in 2013 data
    # s1q12c # internet; not available in 2013 data
    # s1q17, # religion; interesting, but only ~ 2.% response rate. At best 20% of hh
    
    # jobs
    # s1q35, # has a job; only ~ 100 obs.
    # s1q36b, # 8 obs (!)
  ) %>% 
  # factorize(indiv_raw, 's1q12a', 'language') %>% 
  mutate(
    sex = s1q2,
    age = s1q4,
    polygamous = ifelse(s1q8 == 2, 0, s1q8)
    # ownsMobile = ifelse(s1q12b == 2, 0, s1q12b),
    # internet = ifelse(s1q12c == 2, 0, s1q12c)
  ) 
# %>% 
#   mutate(langhead = str_replace_all(str_to_title(language), '[0-9][0-9].', ''),
#          langhead = str_replace_all(str_to_title(language), '[0-9].', ''))

head = indiv %>% 
  filter(s1q3 == 1) %>% # select only the head
  mutate(
    femhead = ifelse(sex == 2, 1, ifelse(sex == 1, 0, NA))) %>% 
  select(hhid, ea, zone, state, lga, sector, femhead, 
         agehead = age, polygamous)

# create household-level summaries ----------------------------------------
# household-level summaries
# total size
# hh owns phone
# num males
# num females
# wanted to calculate num of dependents: e.g. < 5, > 65; good number are missing.

hh_sum =
  indiv %>% group_by(hhid) %>% 
  summarise(hhsize = n(),
            hh_males = sum(sex == 1),
            hh_females = sum(sex == 2),
            hh_under5 = sum(age <= 5),
            hh_under15 = sum(age <= 15),
            hh_over65 = sum(age >= 65))