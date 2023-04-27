library(tidyverse)
jobs<-readRDS('jobs_clean.rds')
View(jobs)
setwd("G:/My Drive/Adelaide uni/Stats7022/Assignment 2")

## reclean location column
jobs[jobs$location=='Full Time Employee',]$job_type='Full Time Employee'
jobs[jobs$location=='Full Time Employee',]$location=NA
jobs[jobs$organization=='Full Time Employee'& 
       !is.na(jobs$organization),]$organization=NA


unique_location=unique(jobs$location)
unique_location=unique_location[!is.na(unique_location)]
unique_organzation=unique(jobs$organization)
unique_organzation=unique_organzation[!is.na(unique_organzation)]

jobs[jobs$organization %in% unique_location, c("organization", "location")] <- 
  jobs[jobs$organization %in% unique_location, c( "location",'organization')] 

jobs[str_length(jobs$location)>100 & !is.na(jobs$location),]$location=NA

jobs$location<-str_replace_all(jobs$location, "[:digit:]", "")

new <- sub('.*,\\s*','\\1', jobs$location)

new=trimws(new)
new[str_length(new)>2 & !is.na(new)]=NA

jobs$state=new
View(jobs)


jobs1=jobs %>%
  select(salary)%>%
  mutate(salary=str_replace_all(salary,',',''))%>%
  mutate(pay_range = map(str_extract_all(salary, '\\d+([.,]\\d+)?'), as.numeric))
pay_range=jobs1$pay_range
idx <- (sapply(pay_range, length))
View(pay_range)
pay_range[idx==0] <- NA
length(x)
min_hourly_wage=rep(NA,nrow(jobs))
max_hourly_wage=rep(NA,nrow(jobs))
min_yearly_salary=rep(NA,nrow(jobs))
max_yearly_salary=rep(NA,nrow(jobs))

min_hourly_wage[str_detect(jobs$salary,'hour') & !is.na(pay_range)]=
  unlist(map(pay_range[str_detect(jobs$salary,'hour') & !is.na(pay_range)],1))



max_hourly_wage[str_detect(jobs$salary,'hour') & !is.na(pay_range)& sapply(pay_range,length)>1]=
  unlist(map(pay_range[str_detect(jobs$salary,'hour') & !is.na(pay_range)& sapply(pay_range,length)>1],2))
hourly_wage=bind_cols(min_hourly_wage,max_hourly_wage)

min_yearly_salary[str_detect(jobs$salary,'year') & !is.na(pay_range)]=
  unlist(map(pay_range[str_detect(jobs$salary,'year') & !is.na(pay_range)],1))

max_yearly_salary[str_detect(jobs$salary,'year') & !is.na(pay_range)& sapply(pay_range,length)>1]=
  unlist(map(pay_range[str_detect(jobs$salary,'year') & !is.na(pay_range)& sapply(pay_range,length)>1],2))

yearly_salary=bind_cols(min_yearly_salary,max_yearly_salary)
Yearly_wage=(min_yearly_salary+max_yearly_salary)/2

Salary=data.frame(min_hourly_wage=min_hourly_wage,
                  max_hourly_wage=max_hourly_wage,
                  min_yearly_salary=min_yearly_salary,
                  max_yearly_salary=max_yearly_salary)
Salary$yearly_wage=rowMeans(Salary[,c("min_yearly_salary","max_yearly_salary")],
                            na.rm = T)
Salary$hourly_salary=rowMeans(Salary[,c('min_hourly_wage','max_hourly_wage')],
                              na.rm=T)
Salary <- Salary %>% mutate_all(~ifelse(is.nan(.), NA, .))
Salary$yearly_wage[is.na(Salary$yearly_wage)]=Salary$hourly_salary[is.na(Salary$yearly_wage)]*1800     
jobs$salary=Salary$yearly_wage
sector=jobs$sector
fct_lump(sector,20)
jobs$sector=fct_lump(jobs$sector,20)
length(unique(jobs$sector))

?fct_lump

