rm(list=ls())

## First step to load packages and file locations etc.
project.folder = paste0(print(here::here()),'/')

library(dplyr)
library(foreign)
library(readr)

format_mort_2003_2004 <- function(year, file_name) {

file.name = here("data1999-2022",file_name)

start.positions.1 =   c( 19,20,21,23,28,
                         29,35,38,43,44,
                         45,47,51,52,53,
                         55,61,63,65,69,
                         70,83,84,85,102,
                         106,107,109,144,145,
                         146,163,
                         165,166,167,172,173,
                         174,179,180,181,186,
                         187,188,193,194,195,
                         200,201,202,207,208,
                         209,214,215,216,221,
                         222,223,228,229,230,
                         235,236,237,242,243,
                         244,249,250,251,256,
                         257,258,263,264,265,
                         270,271,272,277,278,
                         279,284,285,286,291,
                         292,293,298,299,300,
                         341,344,349,354,359,
                         364,369,374,379,384,
                         389,394,399,404,409,
                         414,419,424,429,434,
                         439
)

start.positions.2 =   c(445,484,488)

end.positions.1 	=   c(19,20,22,25,28,
                        30,37,42,43,44,
                        46,50,51,52,54,
                        56,62,63,66,69,
                        73,83,84,85,105,
                        106,107,109,144,145,
                        149,164,
                        165,166,171,172,173,
                        178,179,180,185,186,
                        187,192,193,194,199,
                        200,201,206,207,208,
                        213,214,215,220,221,
                        222,227,228,229,234,
                        235,236,241,242,243,
                        248,249,250,255,256,
                        257,262,263,264,269,
                        270,271,276,277,278,
                        283,284,285,290,291,
                        292,297,298,299,304,
                        342,348,353,358,363,
                        368,373,378,383,388,
                        393,398,403,408,413,
                        418,423,428,433,438,
                        443
)

end.positions.2 	=   c(446,486,488)

names.1 	= c('rectype','restatus','stateoc_fips','countyoc_fips','popsizoc',
             'stateres_fips','countyrs_fips','cityrs_fips','popsize_cityrs','metro',
             'exstares','pmsares_fips','popsize_countyrs','popmsa','cmsares',
             'statbth','educ','educr','monthdth','sex',
             'age','placdth','marstat','weekday','year',
             'injwork','mandeath','autopsy','activity','injury',
             'ucod','eanum',
             'econdp_1','econds_1','enicon_1','econdp_2','econds_2',
             'enicon_2','econdp_3','econds_3','enicon_3','econdp_4',
             'econds_4','enicon_4','econdp_5','econds_5','enicon_5',
             'econdp_6','econds_6','enicon_6','econdp_7','econds_7',
             'enicon_7','econdp_8','econds_8','enicon_8','econdp_9',
             'econds_9','enicon_9','econdp_10','econds_10','enicon_10',
             'econdp_11','econds_11','enicon_11','econdp_12','econds_12',
             'enicon_12','econdp_13','econds_13','enicon_13','econdp_14',
             'econds_14','enicon_14','econdp_15','econds_15','enicon_15',
             'econdp_16','econds_16','enicon_16','econdp_17','econds_17',
             'enicon_17','econdp_18','econds_18','enicon_18','econdp_19',
             'econds_19','enicon_19','econdp_20','econds_20','enicon_20',
             'ranum','record_1','record_2','record_3','record_4',
             'record_5','record_6','record_7','record_8','record_9',
             'record_10','record_11','record_12','record_13','record_14',
             'record_15','record_16','record_17','record_18','record_19',
             'record_20'
)

names.2		= c('race','hispanic','hispanicr')

# load file part 1
dat.1	 	= 	readr::read_fwf(file.name, fwf_positions(start.positions.1,end.positions.1,names.1), progress=interactive())

# load file part 2
dat.2	 	= 	readr::read_fwf(file.name, fwf_positions(start.positions.2,end.positions.2,names.2), progress=interactive())

# bind two parts of file
dat = cbind(dat.1,dat.2)

dat_clean <- dat %>%
  filter(!if_all(everything(), ~ is.na(.) | . == ""))

write.csv(dat_clean,here("data", paste0('mort', year, '1.csv')))
}

format_mort_2003_2004(2003, "MULT2003.USAllCnty.txt")
format_mort_2003_2004(2004, "MULT2004.USAllCnty.txt")
data_2004 <- read.csv("data/mort2004.csv", fileEncoding="latin1")
data_20041 <- read.csv("data/mort20041.csv", fileEncoding="latin1")
sum <- data_2004 %>%
  group_by(stateoc) %>%
  summarize(count = n())

sum1 <- data_20041 %>%
  group_by(stateoc_fips) %>%
  summarize(count = n())


