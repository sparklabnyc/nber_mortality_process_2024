rm(list=ls())

## First step to load packages and file locations etc.
project.folder = paste0(print(here::here()),'/')

library(dplyr)
library(foreign)
library(readr)

# arguments from Rscript
args = commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year = as.numeric(args[1])
file.type = as.character(args[2])

# file name
file.name = here(paste0(raw_mortality_data,year),paste0('MULT',year,'.',file.type))


start.positions.1 =   c(	19,20,21,23,28,
                         29,35,38,43,44,
                         45,
                         55,61,63,65,69,
                         70,83,84,85,102,
                         106,107,109,144,145,
                         #445,484,488,
                         146,163,
                         165,167,172,174,179,
                         181,186,188,193,195,
                         200,202,207,209,214,
                         216,221,223,228,230,
                         235,237,242,244,249,
                         251,256,258,263,265,
                         270,272,277,279,284,
                         286,291,293,298,300,
                         341,344,349,354,359,
                         364,369,374,379,384,
                         389,394,399,404,409,
                         414,419,424,429,434,
                         439
)

start.positions.2 =   c(445,484,488)

end.positions.1 	=   c(	19,20,22,25,28,
                        30,37,42,43,44,
                        46,
                        56,62,63,66,69,
                        73,83,84,85,105,
                        106,107,109,144,145,
                        #446,486,488,
                        149,164,
                        166,171,173,178,180,
                        185,187,192,194,199,
                        201,206,208,213,215,
                        220,222,227,229,234,
                        236,241,243,248,250,
                        255,257,262,264,269,
                        271,276,278,283,285,
                        290,292,297,299,304,
                        342,348,353,358,363,
                        368,373,378,383,388,
                        393,398,403,408,413,
                        418,423,428,433,438,
                        443
)

end.positions.2 	=   c(446,486,488)

names.1 	= c('rectype','resident','stateocc_fips','countyocc_fips','pop_countyocc',
             'stateres_fips','countyres_fips','cityres_fips','pop_cityres','metro',
             'ex_stateres',
             'statebirth','educ','educ_recode','monthdth','sex',
             'age_detail','placedeath','marital','day_week','year',
             'injurywork','mannerdth','autopsy','activcode','place_injury',
             #'race_detail','origin','hispanic_recode',
             'cause','num_entity',
             'seqn_ent1','cause_ent1','seqn_ent2','cause_ent2','seqn_ent3',
             'cause_ent3','seqn_ent4','cause_ent4','seqn_ent5','cause_ent5',
             'seqn_ent6','cause_ent6','seqn_ent7','cause_ent7','seqn_ent8',
             'cause_ent8','seqn_ent9','cause_ent9','seqn_ent10','cause_ent10',
             'seqn_ent11','cause_ent11','seqn_ent12','cause_ent12','seqn_ent13',
             'cause_ent13','seqn_ent14','cause_ent14','seqn_ent15','cause_ent15',
             'seqn_ent16','cause_ent16','seqn_ent17','cause_ent17','seqn_ent18',
             'cause_ent18','seqn_ent19','cause_ent19','seqn_ent20','cause_ent20',
             'num_record','cause_rec1','cause_rec2','cause_rec3','cause_rec4',
             'cause_rec5','cause_rec6','cause_rec7','cause_rec8','cause_rec9',
             'cause_rec10','cause_rec11','cause_rec12','cause_rec13','cause_rec14',
             'cause_rec15','cause_rec16','cause_rec17','cause_rec18','cause_rec19',
             'cause_rec20'
)

names.2		= c('race_detail','origin','hispanic_recode')

# load file part 1
dat.1	 	= 	readr::read_fwf(file.name, fwf_positions(start.positions.1,end.positions.1,names.1), progress=interactive())

# establish any problems in data extraction
problems.1 = problems(dat.1)
problem.rows = sort(unique(problems.1$row))
dat.problem  = dat.1[problem.rows,]
write.csv(dat.problem, here(paste0(raw_mortality_data,year),paste0('MULT',year,'.',file.type,'.problems.csv')))

# load file part 2
dat.2	 	= 	readr::read_fwf(file.name, fwf_positions(start.positions.2,end.positions.2,names.2), progress=interactive())

# bind two parts of file
dat = cbind(dat.1,dat.2)

# reorder as with previous processing
dat = dat[,c(1:30,94:96,31:93)]

write.dta(dat,here(paste0(raw_mortality_data,year),paste0('MULT',year,'.',file.type,'.processed.dta')))
