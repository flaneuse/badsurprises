# setup --------------------------------------------------------------------

svy_dir = '~/LSMS/NGA_2012_LSMS/Post Harvest Wave 2/Household/'
base_dir = '~/GitHub/badsurprises/'

country = 'NGA'
svy_year = 2013
svy_id = paste0(country, svy_year)

library(tidyverse) # version 1.1.1
library(haven) # version 1.0.0
library(lubridate) # version 1.6.0
library(stringr) # version 1.2.0
library(readxl) # version 1.0.0
library(data.table)
library(tidytext)

source('factorize.R')