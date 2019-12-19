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


# dir.create() ------------------------------------------------------
#Working DIrectory
WD<-file.path("Projects", "Simulated Data", "Simulated-Timecard-Data")

#Sim.Data - Folder Generation
dir.create(file.path(WD, "Sim.Data"), showWarnings = F)
WD.Sim.Data<-file.path(WD, "Sim.Data")

####
####

# Packages Enablement -----------------------------------------------------

require("checkpoint")
checkpoint("2019-12-07", project=WD, forceProject = TRUE)
setSnapshot("2019-12-07")

pkgs<-rev(c("dplyr",        ## for data handling
            "stringr",      ## for strings
            "tidyr",        ## for data strunctures
            "lubridate",    ## for date handling
            "data.table",   ## for data structures and reading/writing
            "ggplot2",      ## for internal visualitions
            "plotly",       ## for interactive visualizations
            "readxl",       ## for ingesting xlsx files
            "openxlsx",     ## for exporting opjects to a finalized excel template
            "parallel",     ## for parallel process computing
            #"readtext",     ## for PDF extraction
            #"caret",       ## for machine learning modeling
            "devtools",     ## for certain functions lacking in base
            "tictoc",       ## for tracking calculation times
            "beepr" ))      ## for placing audio chimes to signal completed calculations

#as.data.frame(lapply(pkgs, install.packages))
as.data.frame(lapply(pkgs, require, character.only=TRUE)) #Reversed for 1st package priority

sessionInfo()


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
         Person.ID=ifelse(Shift.Type=="AM.Shift", "_000001", "UNKNOWN"),
         Person.ID=ifelse(Shift.Type=="PM.Shift", "_000002", Person.ID),
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
         Instance.Length.1=rnorm(n(), mean=4, sd=2),
         Instance.Length.1=(round(Instance.Length.1*60)/60),
         
         ## All negative Instance lengths are changed to be 1hr
         Instance.Length.1=ifelse(Instance.Length.1<0, (60/60), Instance.Length.1),
         Instance.Length.1.MINS=as.integer(Instance.Length.1*60),
         
        
        ##Instance 2
         ## Less wide second Instance concentrated around a mean of 2.5 hrs IF
         ### Instance.1 is... (1) less than 6hrs OR 
         ###                 (2) has a minute value divisible by 16min or 17min
         Instance.Length.2=ifelse(round(Instance.Length.1)<7 | 
                                   round(Instance.Length.1*60)%%16==0 |
                                   round(Instance.Length.1*60)%%17==0,
                                 rnorm(n(), mean=2.5, sd=2),
                                 0),
         Instance.Length.2=(round(Instance.Length.2*60)/60),
         ## All negative Instance lengths are changed to be 1hr
         Instance.Length.2=ifelse(Instance.Length.2<0, (60/60), Instance.Length.2),
         Instance.Length.2.MINS=as.integer(Instance.Length.2*60),
         
        
        ##Instance 3
         ## Less wide second Instance concentrated around a mean of 3.0 hrs IF
         ### Instance.1 & 2 are... (1) less than 6hrs OR 
         ###                      (2) has a minute value divisible by 18min or 19min
         Instance.Length.3=ifelse((Instance.Length.1+Instance.Length.2)<(8+runif(n(), min=0, max=2)),
                                  (round(runif(n(), min=0.3, max=2.5)*60)/60),
                                  0),
         Instance.Length.3=(round(Instance.Length.3*60)/60),
         Instance.Length.3=ifelse(Instance.Length.3<0, (0/60), Instance.Length.3),
         Instance.Length.3.MINS=as.integer(Instance.Length.3*60),
        
         Shift.Length.Total=(Instance.Length.1+Instance.Length.2+Instance.Length.3),
         Shift.Length.Total.MINS=(Instance.Length.1.MINS+Instance.Length.2.MINS+Instance.Length.3.MINS))
    
    
    qplot(df$Instance.Length.1)
    summary(df$Instance.Length.1)
    qplot(df$Instance.Length.2)
    summary(df$Instance.Length.2)
    qplot((df$Instance.Length.1+df$Instance.Length.2))
    summary((df$Instance.Length.1+df$Instance.Length.2))
    qplot(df$Instance.Length.3)
    summary(df$Instance.Length.3)
    table(df$Instance.Length.3==0)
    summary(df$Instance.Length.3==0)
    qplot(df$Shift.Length.Total)
    summary(df$Shift.Length.Total)
    
    qplot(df$Shift.Length.Total) %>% plotly::ggplotly()
    
    

# * Break Lengths ----------------------------------------------------------------------------------
    
    table(df$Instance.Length.1==0)
    table(df$Instance.Length.2==0)
    table(df$Instance.Length.3==0)

df<-df %>% 
      mutate(Meal.Break.1.MINS=ifelse(Instance.Length.1==0 & Instance.Length.2==0,
                                 0,
                                 rnorm(n(), mean=31, sd=3)),
             Meal.Break.1.MINS=floor(Meal.Break.1.MINS),
             Meal.Break.2.MINS=ifelse(Instance.Length.2==0 & Instance.Length.3==0,
                                 0,
                                 rnorm(n(), mean=29, sd=3)),
             Meal.Break.2.MINS=ceiling(Meal.Break.2.MINS) )
  

# ** Forced Missed Meal ----------------------------------------------------------------------------

table(df$Meal.Break.1.MINS, df$Instance.Length.1>8)

df<-df %>%
  mutate(Instance.Length.2=ifelse(Instance.Length.1>8, 0, Instance.Length.2),
         Instance.Length.3=ifelse(Instance.Length.1>8, 0, Instance.Length.3),
         Meal.Break.1.MINS=ifelse(Instance.Length.1>8, 0, Meal.Break.1.MINS),
         Meal.Break.2.MINS=ifelse(Instance.Length.1>8, 0, Meal.Break.2.MINS),
         Meal.Break.3.MINS=ifelse(Instance.Length.1>8, 0, Meal.Break.3.MINS),
         )

table(df$Meal.Break.1.MINS, df$Instance.Length.1>8)

beep(2)
stop()

summary(df$Date.ORG)
summary(df$Date) #Data spans from 2010-01-02 to 2020-03-01

glimpse(df)
summary(df$Instance.Length.1)
summary(df$Instance.Length.2)
summary(df$Instance.Length.1+df$Instance.Length.2)
summary(df$Instance.Length.3)
qplot(round(df$Shift.Length.Total, 1), bins=uniqueN(round(df$Shift.Length.Total, 1))) %>% 
  plotly::ggplotly()
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
         Out.Actual.dt3=In.Actual.dt3+ minutes(as.integer(Instance.Length.3.MINS)) ) %>%
  select(Person.ID, 
         contains("dt1"), Instance.Length.1.MINS, Meal.Break.1.MINS,
         contains("dt2"), Instance.Length.2.MINS, Meal.Break.2.MINS,
         contains("dt3"), Instance.Length.3.MINS,
         Shift.Length.Total,
         everything() )



table(df$Instance.Length.1!=0 & df$Instance.Length.2==0 & df$Instance.Length.3!=0)
table(df$Instance.Length.1!=0 & df$Instance.Length.2!=0 & df$Instance.Length.3==0)



# *** ----------------------------------------------------------------------------------------------
# Timecard Analysis --------------------------------------------------------------------------------

tc.TEMP1<-df %>% 
  select(Person.ID, contains("dt1")) %>% 
  rename(In.Actual.dt=In.Actual.dt1,
         Out.Actual.dt=Out.Actual.dt1)

tc.TEMP2<-df %>% 
  select(Person.ID, contains("dt2")) %>% 
  rename(In.Actual.dt=In.Actual.dt2,
         Out.Actual.dt=Out.Actual.dt2)

tc.TEMP3<-df %>% 
  select(Person.ID, contains("dt3")) %>% 
  rename(In.Actual.dt=In.Actual.dt3,
         Out.Actual.dt=Out.Actual.dt3) %>% 
  filter(In.Actual.dt!=Out.Actual.dt)       ##NOTE: This filter drops ~775 0 length .dt3 rows of data!

tc.TEMP<-full_join(tc.TEMP1, tc.TEMP2)
tc.TEMP<-full_join(tc.TEMP, tc.TEMP3)


tc<-tc.TEMP

fwrite.DF.to.csv.as.char(tc,
                         file.path(WD.Sim.Data,
                                "Simulated Timecards for 2 Employees across 2010-01 to 2020-03.csv"))


