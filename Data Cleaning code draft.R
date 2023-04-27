### 1 Look at the data
8*2/9-4/18+log(0.5,base=10)
log(0.3)
setwd("G:/My Drive/Adelaide uni/Stats7022/Assignment 1")
## THere are severals empty boxes in the csv file, which
# I will recognised as missing values (NAs) when I import
#the data

### 2 Loading data and libraries

View(jobs%>%
  select(sector,job_description)%>%
  filter(str_length(sector)>150))
pacman::p_load(tidyverse,skimr,forcats,purrr)
jobs<-read.csv('23-02-13_jobs.csv',na.strings = '')
skim_without_charts(jobs)
View(jobs)
View(as.data.frame(unique(jobs$sector)))
### 3 Break data into pieces
class(jobs)
jobs[1,]

# data already data frame format, only need to convert it into tibble
# (though not really necessary for this)

jobs$job_description[1]


### 5  looking at columns

# Country, country_code, has_expired and job_board columns only has 1 value, 
#so they can be discarded or ignored


# date_added columns has 21878 missing values so it is pretty much unusable

#job description describes each job, so is job titles, so both are neither 
#categorical nor quantitative variable for analysis purpose



View(jobs)
## Job type will be classed as either full time or part time
#or other, the missing values will be kept+ some job type are mistyped
#into location and organization columns and must be moved back
View(jobs%>%
       select(job_type,location,organization)%>%
       filter(organization=='Full Time Employee'|location=='Full Time Employee'))
jobs[jobs$location=='Full Time Employee',]$job_type='Full Time Employee'
jobs[jobs$location=='Full Time Employee',]$location=NA
jobs[jobs$organization=='Full Time Employee'& 
           !is.na(jobs$organization),]$organization=NA





jobs<-jobs%>%
  mutate(job_type=case_when(str_detect(job_type,'Full Time')~'Full Time',
                             str_detect(job_type,'Part Time')~'Part Time',
                             is.na(job_type)~ NA_character_,
                             T~ 'Other'
         ))

unique(jobs$job_type)




unique_location=unique(jobs$location)

unique_location=unique_location[!is.na(unique_location)]


unique_organzation=unique(jobs$organization)
unique_organzation=unique_organzation[!is.na(unique_organzation)]
View(as.tibble(jobs$location[jobs$location %in% unique_organzation]))
View(as.tibble(jobs$organization[jobs$organization %in% unique_location]))
nrow(as.tibble(jobs$location[jobs$location %in% unique_organzation]))
nrow(as.tibble(jobs$organization[jobs$organization %in% unique_location]))
View((jobs%>%
       select(location,organization)%>%
       filter(organization %in% unique_location)))
View(jobs)
## Location and organization has value that seems to be misplaced between
##check if there's any value in location that is also in organization, then
#switch those value

## however, if there's any unique location (location that appears in the dataset
#only once) that is misplaced, it won't be detected and switched
n_unique(jobs$location)

jobs[jobs$organization %in% unique_location, c("organization", "location")] <- 
  jobs[jobs$organization %in% unique_location, c( "location",'organization')] 

unique_location=unique(jobs$location)
unique_location=unique_location[!is.na(unique_location)]
unique_organzation=unique(jobs$organization)
unique_organzation=unique_organzation[!is.na(unique_organzation)]
View(as.tibble(jobs$location[jobs$location %in% unique_organzation]))
View(as.tibble(jobs$organization[jobs$organization %in% unique_location]))

skim_without_charts(jobs)


jobs[str_length(jobs$location)>100 & !is.na(jobs$location),]$location=NA

jobs[str_length(jobs$organization)>150 & !is.na(jobs$organization),]$location=NA


jobs$location<-str_replace_all(jobs$location, "[:digit:]", "")

### salary

View(head(jobs%>%
       select(salary)%>%
       filter(!is.na(salary))))

jobs1=jobs %>% 
       select(salary)%>%
       mutate(salary=str_replace_all(salary,',',''))%>%
  mutate(pay_range = map(str_extract_all(salary, '\\d+([.,]\\d+)?'), as.numeric))


View(jobs1)




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



min_yearly_salary[str_detect(jobs$salary,'year') & !is.na(pay_range)]=
  unlist(map(pay_range[str_detect(jobs$salary,'year') & !is.na(pay_range)],1))

max_yearly_salary[str_detect(jobs$salary,'year') & !is.na(pay_range)& sapply(pay_range,length)>1]=
  unlist(map(pay_range[str_detect(jobs$salary,'year') & !is.na(pay_range)& sapply(pay_range,length)>1],2))
View(jobs)
jobs$min_hourly_wage=min_hourly_wage
jobs$max_hourly_wage=max_hourly_wage
jobs$min_yearly_salary=min_yearly_salary
jobs$max_yearly_salary=max_yearly_salary

skim_without_charts(jobs)
# No particular format, some yearly salary and some hourly salary, most are undiclosed
# displayed as either NA or words

getwd()
filenamepath <- glue::glue("{lubridate::today()}-jobs.csv")
filenamepath
write.csv(jobs,filenamepath)
