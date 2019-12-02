rm(list =ls())

require("tictoc")
tictoc::tic()

# Housekeeping ------------------------------------------------------------
par(mar=c(1,1,1,1))        #1 plot per plot window
options(scipen = 999)      #Eliminate auto rounding
options(width = 80)        #Code width
options(max.print = 200)   #Limited for quicker run time
#memory.size(max=TRUE)      #Devote maximum RAM amount to Rs -- Windows functions
Sys.setenv(TZ = "GMT")     #
TZ <- "GMT"                #
set.seed(1776)             #



# Packages Enablement -----------------------------------------------------

require("checkpoint")
#checkpoint("2019-11-01")

pkgs<-rev(c("dplyr", "stringr", "tidyr", "lubridate",
            "data.table",
            "readtext",
            "tictoc",
            "caret", ## for modeling ML
            "ggplot2",
            "readxl", 
            "openxlsx",
            "parallel",
            "devtools",
            "beepr")) #Reversed for 1st package priority

as.data.frame(lapply(pkgs, require, character.only=TRUE))
#as.data.frame(lapply(pkgs, install.packages))

sessionInfo()


# File Name ---------------------------------------------------------------
########################################################################!
####

#File.Name
File.Name<-"Simulated Timecard Data.R"
#Date Version
Version<-"2019-11-28" #RECALL: use ctrl+f to replace this across the script

#Working DIrectory
WD<-file.path("Projects", "Simulated Data", "Simulated-Timecard-Data")

# dir.create() ------------------------------------------------------
#Sim.Data - Folder Generation
dir.create(file.path(WD, "Sim.Data"), showWarnings = F)
WD.Sim.Data<-file.path(WD, "Sim.Data")

####
####


# Custom Functions ------------------------------------------------------------------

if("Custom Functions.R" %in% list.files(WD) ){
  source(file.path(WD, "Custom Functions.R"))
}else{
  download.file(
  url="https://raw.githubusercontent.com/PGundy/Useful-Custom-R-Functions/master/Custom%20Functions.R",
  destfile=file.path(WD, "Custom Functions.R") )
}


# **************** --------------------------------------------------------------------------
# Start Time ---------------------------------------------------------------------------------------

Number.of.Days<-3650+60 ## (3650 days of days represents 10yrs of data PLUS 60 days extra)

Start.Hours.1<-rnorm(Number.of.Days, mean=10, sd=1)
  summary(Start.Hours.1)
Start.Hours.2<-rnorm(Number.of.Days, mean=18, sd=1)
  summary(Start.Hours.2)
Start.Hours<-append(Start.Hours.1, Start.Hours.2)
summary(Start.Hours)
Start.Hours.hm<-paste(Start.Hours%/%1, floor((Start.Hours%%1)*60), sep=":")

Start.Hours.dt<-ymd_hm(paste("2010-01-01", Start.Hours.hm))


df<-data.frame(Start.Hours, Start.Hours.hm, Start.Hours.dt) %>% 
  arrange(Start.Hours.dt) %>% 
  mutate(Shift.Type=ifelse(Start.Hours<13, "AM.Shift", "PM.Shift"),
         Employee.ID=ifelse(Shift.Type=="AM.Shift", "_000001", "UNKNOWN"),
         Employee.ID=ifelse(Shift.Type=="PM.Shift", "_000002", Employee.ID),
         Employee.Name=NA,
         Employee.Name=ifelse(Shift.Type=="AM.Shift", "Early Start Employee", Employee.Name),
         Employee.Name=ifelse(Shift.Type=="PM.Shift", "Late Start Employee", Employee.Name)
         ) %>% 
  group_by(Shift.Type) %>% 
  mutate(Row.Number=row_number(),
         Random.Number.Filter=(rbinom(n(), size=10, .4720)+1),  ##avg of 2.1 days off per week
                  ## ~114 days off per year which is 104 days for weekends + 10 other days off
         Day.Off=(Random.Number.Filter>6)
         ) %>% 
  ungroup()

#### Proof of the days off calculations
    table(df$Day.Off)
    prop.table(table(df$Day.Off))
    prop.table(table(df$Day.Off))*7 ## This implies that 2.1 of 7 every 7 days is a day off!
    prop.table(table(df$Day.Off))*365 ## Days off in the year counting weekends
    
    

# * Instance Lengths --------------------------------------------------------------------------------
    
    ##TODO: Create a new code section that has a lower mean for Instance length 2 and Instance.3
    
df<-df %>% 
  filter(Day.Off!=TRUE) %>% 
  mutate(Date.ORG=date(Start.Hours.dt),
         Date=Date.ORG+days(Row.Number),
         
         Start.Time.dt=ymd_hm(paste(Date, Start.Hours.hm)),
         
        ##Instance 1
         ## Wide distribution for the first Instance
         Instance.Length.1=rnorm(n(), mean=5, sd=2),
         Instance.Length.1=(round(Instance.Length.1*60)/60),
         
         ## All negative Instance lengths are changed to be 1hr
         Instance.Length.1=ifelse(Instance.Length.1<0, (60/60), Instance.Length.1),
         Instance.Length.1.MINS=as.integer(Instance.Length.1*60),
         
        
        ##Instance 2
         ## Less wide second Instance concentrated around a mean of 2.5 hrs IF
         ### Instance.1 is... (1) less than 6hrs OR 
         ###                 (2) has a minute value divisible by 16min or 17min
         Instance.Length.2=ifelse(round(Instance.Length.1)<8 | 
                                   round(Instance.Length.1*60)%%16==0 |
                                   round(Instance.Length.1*60)%%17==0,
                                 rnorm(n(), mean=3, sd=2),
                                 0),
         Instance.Length.2=(round(Instance.Length.2*60)/60),
         ## All negative Instance lengths are changed to be 1hr
         Instance.Length.2=ifelse(Instance.Length.2<0, (60/60), Instance.Length.2),
         Instance.Length.2.MINS=as.integer(Instance.Length.2*60),
         
        
        ##Instance 3
         ## Less wide second Instance concentrated around a mean of 3.0 hrs IF
         ### Instance.1 & 2 are... (1) less than 6hrs OR 
         ###                      (2) has a minute value divisible by 18min or 19min
         Instance.Length.3=ifelse(round(Instance.Length.1+Instance.Length.2)>8,
                                 rnorm(n(), mean=0, sd=2),
                                 0),
         Instance.Length.3=(round(Instance.Length.3*60)/60),
         Instance.Length.3=ifelse(Instance.Length.3<0, (0/60), Instance.Length.3),
         Instance.Length.3.MINS=as.integer(Instance.Length.3*60),
        
        
         Shift.Length.Total=(Instance.Length.1+Instance.Length.2+Instance.Length.3),
         Shift.Length.Total.MINS=(Instance.Length.1.MINS+Instance.Length.2.MINS+Instance.Length.3.MINS))
    

# * Break Lengths ----------------------------------------------------------------------------------

df<-df %>% 
      mutate(Meal.Break.1.MINS=ifelse(Instance.Length.1==0,
                                 0,
                                 rnorm(n(), mean=31, sd=3)),
             Meal.Break.1.MINS=round(Meal.Break.1.MINS),
             Meal.Break.2.MINS=ifelse(Instance.Length.2==0,
                                 0,
                                 rnorm(n(), mean=29, sd=3)),
             Meal.Break.2.MINS=ceiling(Meal.Break.2.MINS) )
  

summary(df$Date.ORG)
summary(df$Date) #Data spans from 2010-01-02 to 2020-03-01

glimpse(df)
summary(df$Instance.Length.1)
summary(df$Instance.Length.2)
summary(df$Instance.Length.3)
qplot(round(df$Shift.Length.Total, 1), bins=uniqueN(round(df$Shift.Length.Total, 1)))
summary(df$Shift.Length.Total)


summary(df$Meal.Break.1.MINS[df$Meal.Break.1.MINS!=0])
summary(df$Meal.Break.2.MINS[df$Meal.Break.2.MINS!=0])
#qplot(df$Meal.Break.1.MINS[df$Meal.Break.1.MINS!=0], bins=uniqueN(df$Meal.Break.1.MINS[df$Meal.Break.1.MINS!=0]))
#qplot(df$Meal.Break.2.MINS[df$Meal.Break.2.MINS!=0], bins=uniqueN(df$Meal.Break.2.MINS[df$Meal.Break.2.MINS!=0]))




# * Instance Builder --------------------------------------------------------------------------------
df<-df %>% select(-contains("dt"), everything(), Start.Time.dt, contains("dt"))

df<-df %>%
  mutate(In.Actual.dt1=Start.Time.dt,
         Out.Actual.dt1=In.Actual.dt1+ minutes(as.integer(Instance.Length.1.MINS)),
         
         In.Actual.dt2=Out.Actual.dt1+minutes(as.integer(df$Meal.Break.1.MINS)),
         Out.Actual.dt2=In.Actual.dt2+ minutes(as.integer(Instance.Length.2.MINS)),
         
         In.Actual.dt3=Out.Actual.dt2+minutes(as.integer(df$Meal.Break.2.MINS)),
         Out.Actual.dt3=In.Actual.dt3+ minutes(as.integer(Instance.Length.3.MINS)) 
         ) %>% 
  select(Employee.ID, 
         contains("dt1"), Instance.Length.1.MINS, Meal.Break.1.MINS,
         contains("dt2"), Instance.Length.2.MINS, Meal.Break.2.MINS,
         contains("dt3"), Instance.Length.3.MINS,
         Shift.Length.Total,
         everything()
         )






# *** ----------------------------------------------------------------------------------------------
# Timecard Analysis --------------------------------------------------------------------------------

tc.TEMP1<-df %>% 
  select(Employee.ID, contains("dt1")) %>% 
  rename(In.Actual.dt=In.Actual.dt1,
         Out.Actual.dt=Out.Actual.dt1)

tc.TEMP2<-df %>% 
  select(Employee.ID, contains("dt2")) %>% 
  rename(In.Actual.dt=In.Actual.dt2,
         Out.Actual.dt=Out.Actual.dt2)

tc.TEMP3<-df %>% 
  select(Employee.ID, contains("dt3")) %>% 
  rename(In.Actual.dt=In.Actual.dt3,
         Out.Actual.dt=Out.Actual.dt3)

tc.TEMP<-full_join(tc.TEMP1, tc.TEMP2)
tc.TEMP<-full_join(tc.TEMP, tc.TEMP3)

tc<-tc.TEMP %>% 
  arrange(Employee.ID, In.Actual.dt) %>% 
  group_by(Employee.ID) %>% 
  mutate(Gap.Next=as.numeric(difftime(lead(In.Actual.dt), Out.Actual.dt, units="hours")),
         Gap.Next=ifelse(is.na(Gap.Next), 123456, Gap.Next),
         Gap.Last=as.numeric(difftime(In.Actual.dt, lag(Out.Actual.dt), units="hours")) ,
         Gap.Last=ifelse(is.na(Gap.Last), 123456, Gap.Last))

ResetLength<-4

tc<-tc %>% 
  group_by(Employee.ID) %>% 
  mutate(Instance.Length=as.numeric(difftime(Out.Actual.dt, In.Actual.dt, units="hours"))) %>% 
  mutate(Shift.Start=ifelseC(Gap.Next<ResetLength & Gap.Last>ResetLength, In.Actual.dt, NA),
         Shift.Start=fillNA(Shift.Start) ) %>% 
  group_by(Employee.ID, Shift.Start) %>% 
  mutate(Shift.End=max(Out.Actual.dt),
         Shift.Length=as.numeric(difftime(Shift.End, Shift.Start, units="hours")) )
  






glimpse(tc)
stop()
# *************** ----------------------------------------------------------------------------------
# Experiment to make more Employees ----------------------------------------------------------------
# Employee IDs -------------------------------------------------------------------------------------


Employee.Count<-250

AM.Employee.IDs<-seq(1, Employee.Count/2, 1) %>% 
  str_pad(., width = 6, pad="0") %>% 
  paste0("_", .)

PM.Employee.IDs<-seq((Employee.Count/2)+1, Employee.Count, 1) %>% 
  str_pad(., width = 6, pad="0") %>% 
  paste0("_", .)

Employee.ID<-append(AM.Employee.IDs, PM.Employee.IDs) %>% 
  unique(.)


##TODO: Use the 250 simulated IDs from above to cross join 125 into the AM & PM shift EACH!
##TODO: THEN for each employee on each day & each clocking instance (+-)15min on every Instance length
## IF done correctly this will allow for the simulation of a class with approximately normal schedules
### with some reasonable variation for each person throughout the dataset.



