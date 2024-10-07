## First step to load packages and file locations etc.
project.folder = paste0(print(here::here()),'/')
setwd(project.folder)
library(here)
library(dplyr)
library(foreign)
library(readr)

format_mort_1999_2002 <- function(year, file_name) {

  file.name <- here("data1999-2022", file_name)

start.positions.1 =   c( 19,20,21,23,
                         31,33,36,39,40,
                         44,46,49,50,51,
                         52,54,55,59,60,
                         64,75,77,78,80,
                         82,83,97,
                         115,119,121,
                         124,126,129,134,136,
                         139,140,141,142,160,
                         162,163,164,169,170,
                         171,176,177,178,183,
                         184,185,190,191,192,
                         197,198,199,204,205,
                         206,211,212,213,218,
                         219,220,225,226,227,
                         232,233,234,239,240,
                         241,246,247,248,253,
                         254,255,260,261,262,
                         267,268,269,274,275,
                         276,281,282,283,288,
                         289,290,295,296,297,
                         338,341,346,351,356,
                         361,366,371,376,381,
                         386,391,396,401,406,
                         411,416,421,426,431,
                         436
)


end.positions.1 	=   c(19,20,22,25,
                       32,35,38,39,40,
                       45,48,49,50,51,
                       53,54,56,59,61,
                       66,75,77,79,81,
                       82,83,101,
                       118,120,123,
                       125,128,132,135,136,
                       139,140,141,145,161,
                       162,163,168,169,170,
                       175,176,177,182,183,
                       184,189,190,191,196,
                       197,198,203,204,205,
                       210,211,212,217,218,
                       219,224,225,226,231,
                       232,233,238,239,240,
                       245,246,247,252,253,
                       254,259,260,261,266,
                       267,268,273,274,275,
                       280,281,282,287,288,
                       289,294,295,296,301,
                       339,345,350,355,360,
                       365,370,375,380,385,
                       390,395,400,405,410,
                       415,420,425,430,435,
                       440
)


names.1 	= c('rectype','restatus','stateoc','countyoc',
             'stateres','countyrs','cityrs','popsize_cityrs','metro',
             'exstares','pmsares','popsizoc','popsize_countyrs','popmsa',
             'educ','educr','monthdth','sex','race',
             'age','placdth','marstat','statbth','hispanic',
             'hispanicr','weekday','cityrs_fips',
             'year','stateoc_fips','countyoc_fips',
             'stateres_fips','countyrs_fips','pmsares_fips','cmsares','injwork',
             'mandeath','activity','injury','ucod','eanum',
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




dat.1	 	= 	readr::read_fwf(file.name, fwf_positions(start.positions.1,end.positions.1,names.1), progress=interactive())

dat.1_clean <- dat.1 %>%
  filter(!if_all(everything(), ~ is.na(.) | . == ""))

write.csv(dat.1_clean,here("data", paste0('mort', year, '1.csv')))
}

format_mort_1999_2002(1999, "MULT1999.USAllCnty.txt")
format_mort_1999_2002(2000, "MULT2000.USAllCnty.txt")
format_mort_1999_2002(2001, "MULT2001.USAllCnty.txt")
format_mort_1999_2002(2002, "MULT2002.USAllCnty.txt")

data_2001 <- read.csv("data/mort2001.csv", fileEncoding="latin1")
data_20011 <- read.csv("data/mort20011.csv", fileEncoding="latin1")
sum <- data_2001 %>%
  group_by(sex) %>%
  summarize(count = n())

sum1 <- data_20011 %>%
  group_by(sex) %>%
  summarize(count = n())

