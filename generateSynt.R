library(simPop) ## this scripts expect simPop>=2.0.0 (currently only on github)
library(data.table)


library(ipumsr)

ddi <- read_ipums_ddi("data/usa_00002.xml")
#dat <- read_ipums_micro(ddi,n_max  = 1000)
dat <- read_ipums_micro_chunked(ddi,
                                IpumsDataFrameCallback$new(function(x, pos) {
                                  x[x$STATEFIP%in%c(17,39), ]
                                }), chunk_size = 1e4)
setDT(dat)
dat[,hhid:=paste(SERIAL,YEAR)]
dat[,HHWT:=NULL]

dat$STRATA <- as.factor(dat$STRATA)
dat <- dat[GQ==1&YEAR>=2005]
dat[,PERWTbyy:=PERWT/length(unique(YEAR))]
inp <- specifyInput(dat, weight = "PERWTbyy",
                    strata = "STRATA",
                    hhid = "hhid")
synth <- simStructure(inp, method="direct",
                      basicHHvars = c("SEX","AGE","RELATE","RACE","METRO"))
synth <- simCategorical(synth, additional = c("MARST","CITIZEN"), method ="ranger", nr_cpus = 1)
synth <- simCategorical(synth, additional = c("EDUC","SPEAKENG"), method ="v", nr_cpus = 1)
synth <- simCategorical(synth, additional = c("HISPAN","SCHOOL"), method ="ranger", nr_cpus = 1)
synth <- simCategorical(synth, additional = c("EMPSTAT"), method ="ranger", nr_cpus = 1)
synth <- simRelation(synth, additional = c("HHINCOME","POVERTY"), method ="ranger", nr_cpus = 1,
                     relation = "RELATE", head = "Head/Householder", method ="ranger")

synth <- simContinuous(synth, additional = c("HHINCOME"), method ="ranger", nr_cpus = 1)



