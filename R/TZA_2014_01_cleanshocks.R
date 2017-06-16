# setup --------------------------------------------------------------------

base_dir = '~/LSMS/TZA_2012_LSMS/'

library(tidyverse) # version 1.1.1
library(haven) # versoin 1.0.0

tz = read_dta(paste0(base_dir, 'HH_SEC_R.dta'))
tz %>% filter(hh_r01 == 1) %>% count(shockid) %>% arrange(desc(n))
