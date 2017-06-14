
# BGD_2015_01_cleanshocks.R -----------------------------------------------

# Code to clean up the shock module from the Bangladesh Integrated Household Survey, 2015
# Data downloaded 14 June 2017 from Harvard's Dataverse: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BXSYEL
# Laura Hughes, lhughes@usaid.gov, 14 June 2017, USAID | GeoCenter


# setup --------------------------------------------------------------------

base_dir = '~/LSMS/BGD_2015_PBS/'

library(tidyverse) # version 1.1.1
library(haven) # versoin 1.0.0



# import data -------------------------------------------------------------

df = read_dta(paste0(base_dir, '050_r2_mod_t1_male.dta'))
