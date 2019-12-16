rm(list =ls())

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
            "svDialogs",    ## for prompting user for mistyped inputs
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


# *************** ----------------------------------------------------------------------------------
# Loading tc Data ----------------------------------------------------------------------------------

tc<-fread(file.path(WD.Sim.Data, "Simulated Timecards for 2 Employees across 2010-01 to 2020-03.csv"))

tc<-tc %>% mutate(In.Actual.dt=ymd_hms(In.Actual.dt),
                  Out.Actual.dt=ymd_hms(Out.Actual.dt))
  
uniqueN(tc$Person.ID)==2                              ## 2 employees (1) AM shifts & (2) PM Shifts
stopifnot(min(date(tc$In.Actual.dt))=="2010-01-02" )  ## MIN date is... 2010-01-02
stopifnot(max(date(tc$In.Actual.dt))=="2020-03-02" )  ## MAX date is... 2020-03-02

glimpse(tc)



# *************** ----------------------------------------------------------------------------------
# Req Global Vars ----------------------------------------------------------------------------------
Filing.Date<-ymd("2018-01-01") ## This should be changed for each case
PAGA.Date<-Filing.Date         ## This should be changed for each case

ResetLength<-c(4)                ## This is the minimum number of HOURS that elapse b/w shifts
False.Positive.Clock<-c(0)       ## If the tmins b/w a time pair to the next THEN it is not real
Client.PP.Gap<-"Biweekly"     ##Should be either 'Weekly' or 'Biweekly'

#Meal.Req.Strictness<-c("Strict") ##Should be "Strict(>)" or "Relaxed(>=)"
Meal.1.Req<-5
Meal.2.Req<-10
Meal.Break.Req.MINS<-30

##These variables should be chosen from the below calendar of 2006
First.Work.Week.DATE<-ymd("2006-01-01")     ## This should be found in payroll (pr) or emails
First.Pay.Period.DATE<-ymd("2006-01-08")    ## This should be found in payroll (pr) or emails


# * Global Var Check -------------------------------------------------------------------------------

##TODO: Change the warnings to use svDialogs::dlg_input()
## NOTE: If you use svDialogs::dlg_form() we can have suggested fields.


if (is.null(Filing.Date)==T | is.na(Filing.Date)==T) 
  warning("Filing.Date must be of class: date")

if (is.null(PAGA.Date)==T | is.na(PAGA.Date)==T) 
  warning("PAGA.Date must be of class: date")

if (is.null(ResetLength)==T | is.na(ResetLength)==T | !is.numeric(ResetLength)==T) 
  warning("ResetLength must be of class: numeric")

if (is.null(False.Positive.Clock)==T | 
    is.na(False.Positive.Clock)==T | 
    !is.numeric(False.Positive.Clock)==T) 
  warning("False.Positive.Clock must be of class: numeric")

if (Client.PP.Gap %in% c("Weekly", "Biweekly")==F)
  warning("'Client.PP.Gap' must be either: 'Weekly' or 'Biweekly'", call. = FALSE)

## if (Meal.Req.Strictness %in% c("Strict", "Relaxed")==F)
##   warning("'Meal.Req.Strictness' must be either: 'Strict' or 'Relaxed'", call. = FALSE)



if (is.null(Meal.1.Req)==T | 
    is.na(Meal.1.Req)==T | 
    !is.numeric(Meal.1.Req)==T) 
  warning("Meal.1.Req must be of class: numeric")

if (is.null(Meal.2.Req)==T | 
    is.na(Meal.2.Req)==T | 
    !is.numeric(Meal.2.Req)==T) 
  warning("Meal.2.Req must be of class: numeric")




# *************** ----------------------------------------------------------------------------------


# * WorkWeekSpacer ---------------------------------------------------------------------------------

########################################################!
############### Calendar for January 2006 ##############!
## Weekdays: ## Su | Mo | Tu | We | Th | Fr | Sa |
## Week 1    ## 01 | 02 | 03 | 04 | 05 | 06 | 07 |
## Week 2    ## 08 | 09 | 10 | 11 | 12 | 13 | 14 |
## Week 3    ## 15 | 16 | 17 | 18 | 19 | 20 | 21 | 
## Week 4    ## 22 | 23 | 24 | 25 | 26 | 27 | 28 | 
## Week 5    ## 29 | 30 | 31 |
#######################################################!
###### The above helps align work weeks and pay periods!
#######################################################!

WorkWeekSpacer<-data.table(seq.Date(min(date(tc$In.Actual.dt))-days(30), ymd("2029-12-31"), 1)) %>% 
  mutate(Date=V1,
         Weekday=weekdays(Date)) %>% 
  left_join(., ##Adding weekly spacing on a Sunday to Sunday
            data.table(seq.Date(First.Work.Week.DATE, ymd("2029-12-31"), 7)) %>% 
              mutate(Start.Weekly.WW=V1,
                     End.Weekly.WW=Start.Weekly.WW+days(6)
                     )) %>% 
  left_join(., ##Adding biweekly spacing on a Sunday to NEXT Sunday
            data.table(seq.Date(First.Pay.Period.DATE, ymd("2029-12-31"), 14)) %>% 
              mutate(Start.Biweekly.WW=V1,
                     End.Biweekly.WW=Start.Biweekly.WW+days(13))) %>% 
  mutate(Start.Weekly.WW=fillNA(Start.Weekly.WW),
         End.Weekly.WW=fillNA(End.Weekly.WW),
         Start.Biweekly.WW=fillNA(Start.Biweekly.WW),
         End.Biweekly.WW=fillNA(End.Biweekly.WW) ) %>% 
  select(-V1)
  
  glimpse(WorkWeekSpacer)
  #View(WorkWeekSpacer)




# tc Analysis -----------------------------------------------------------------------------------------
# * Overnight Split --------------------------------------------------------------------------------
  
  tc.No.Overnights<-tc %>% ungroup() %>% filter(date(In.Actual.dt)==date(Out.Actual.dt))
  tc.Overnights<-tc %>% ungroup() %>% filter(date(In.Actual.dt)!=date(Out.Actual.dt))
  
  dim(tc.No.Overnights)
  dim(tc.Overnights)
  
  
  ## Splitting time pairs on overnights
    ##Censoring the 1st paert of the pair to be ymd_hms to ymd_00:00:00
    tc.Overnights.1<-tc.Overnights %>% 
      mutate(Out.Actual.dt=ymd_hms(paste(date(Out.Actual.dt), "00:00:00.01")) )
    
    ##Censoring the 2nd paert of the pair to be ymd_00:00:00 to ymd_hms
    tc.Overnights.2<-tc.Overnights %>% 
      mutate(In.Actual.dt=ymd_hms(paste(date(Out.Actual.dt), "00:00:00.01")) )
  
  
  #glimpse(tc.Overnights.1)
  #glimpse(tc.Overnights.2)
  
  tc.Overnights<-full_join(tc.Overnights.1, tc.Overnights.2) ##Join and then nullify these objects
    #tc.Overnights.1<-NULL
    #tc.Overnights.2<-NULL
  tc<-full_join(tc.No.Overnights, tc.Overnights) %>% 
    arrange(Person.ID, In.Actual.dt) %>% 
    mutate(Date=date(In.Actual.dt)) %>% 
    select(Person.ID, Date, contains(".dt"))
  

# *** WW Spacer --------------------------------------------------------------------------------
    tc<-left_join(tc, WorkWeekSpacer)
  

# * Gap.Next ---------------------------------------------------------------------------------------
tc<-tc %>% 
    ungroup() %>% 
    arrange(Person.ID, In.Actual.dt) %>% 
    group_by(Person.ID) %>% 
    ##NOTE: NAs here are '123456' to allow for later calculations
    mutate(Gap.Next=as.numeric(difftime(lead(In.Actual.dt), Out.Actual.dt, units="hours")),
           Gap.Next=ifelse(is.na(Gap.Next), 123456, Gap.Next),
         Gap.Last=as.numeric(difftime(In.Actual.dt, lag(Out.Actual.dt), units="hours")) ,
         Gap.Last=ifelse(is.na(Gap.Last), 123456, Gap.Last)) %>% 
    ungroup()

  
# * Clustering -------------------------------------------------------------------------------------
tc<-tc %>% 
    group_by(Person.ID) %>% 
    mutate(Pair.Length=as.numeric(difftime(Out.Actual.dt, In.Actual.dt, units="hours"))) %>% 
    mutate(Cluster.Start=ifelseC(Gap.Next<ResetLength & Gap.Last>ResetLength, In.Actual.dt, NA),
           Cluster.Start=fillNA(Cluster.Start) ) %>% 
    group_by(Person.ID, Cluster.Start) %>% 
    mutate(Cluster.End=max(Out.Actual.dt),
           Pair.Order=row_number(),
           Pairs.Per.Cluster=n(),
           Cluster.Accum=cumsum(Pair.Length),
           Cluster.Length=as.numeric(difftime(Cluster.End, Cluster.Start, units="hours")),
           Next.Break.Length.MINS=ifelse(Gap.Next>0 & Gap.Next<ResetLength, Gap.Next*60, NA) ) %>% 
    ungroup() %>% 
    mutate(Cluster.Overnight=ifelse(date(Cluster.Start)!=date(Cluster.End), T, F),
           False.Positive.Clock=ifelse(Gap.Next %in% False.Positive.Clock, T, F))




  
  

# * Meal Calcs -------------------------------------------------------------------------------------
# ** Late & Short ----------------------------------------------------------------------------------

tc<-tc %>% 
  ungroup() %>% 
  mutate(##Meal 1 Calculations
         Meal.1.Required=ifelse(Cluster.Length>Meal.1.Req, 1, 0),
         Meal.1.Late.MINS=ifelse(Cluster.Accum>Meal.1.Req & !is.na(Next.Break.Length.MINS), 
                                 (Cluster.Accum-Meal.1.Req)*60, 
                                 0),
         Meal.1.Short.MINS=ifelse(Next.Break.Length.MINS<=Meal.Break.Req.MINS, 
                                  Meal.Break.Req.MINS-Next.Break.Length.MINS,
                                  0),
         
         Meal.1.LateShort.MINS=Meal.1.Late.MINS+Meal.1.Short.MINS,
         
         Meal.1.Compliant=ifelse(Meal.1.Required==1 & Next.Break.Length.MINS>=Meal.Break.Req.MINS &
                                   Meal.1.Late.MINS==0 & Meal.1.Short.MINS==0, 1, 0),
         
         Meal.1.Length=ifelse(Meal.1.Compliant %in% c(0, 1) , Next.Break.Length.MINS, NA),
          ## Note: Meal.1.Expected.Break expects M1 1/3 through a shift for natural fit
         Meal.1.Expected.Break=ifelse(!is.na(Meal.1.Length),
                                      round(abs(Cluster.Length*(1/3)-Cluster.Accum), 3),
                                      NA),
         
         ## Meal 2 Calculations
         Meal.2.Required=ifelse(Cluster.Length>Meal.2.Req, 1, 0),
         Meal.2.Late.MINS=ifelse(Cluster.Accum>Meal.2.Req & !is.na(Next.Break.Length.MINS), 
                                 (Cluster.Accum-Meal.2.Req)*60, 
                                 0),
         Meal.2.Short.MINS=ifelse(Next.Break.Length.MINS<=Meal.Break.Req.MINS, 
                                  Meal.Break.Req.MINS-Next.Break.Length.MINS,
                                  0),
         Meal.2.LateShort.MINS=Meal.2.Late.MINS+Meal.2.Short.MINS,
         
         Meal.2.Compliant=ifelse(Meal.2.Required==1 & Next.Break.Length.MINS>=Meal.Break.Req.MINS &
                                   Meal.2.Late.MINS==0 & Meal.2.Short.MINS==0, 1, 0),
         Meal.2.Length=ifelse(Meal.2.Compliant==1, Next.Break.Length.MINS, NA),
          ## Note: Meal.1.Expected.Break expects M2 1/3 through a shift for natural fit
         Meal.2.Expected.Break=ifelse(!is.na(Meal.2.Length),
                                      round(abs(Cluster.Length*(2/3)-Cluster.Accum), 3),
                                      NA) )
  

# Meal 1 Compliant ---------------------------------------------------------------------------------
# * Ranking ---------------------------------------------------------------------------------------

  tc.M1<- tc %>% ## Below arranges tc by the shift and orders by M1 compliance & expected
    arrange(Person.ID, Cluster.Start, desc(Meal.1.Compliant), Meal.1.Expected.Break) %>% 
    group_by(Person.ID, Cluster.Start) %>% 
    mutate(Meal.1.Compliance.Ranking=ifelse(Meal.1.Compliant==1, row_number(), NA)) %>% 
    arrange(Person.ID, Cluster.Start, Pair.Order) %>%
    select(Person.ID, ends_with(".dt"), contains("Pair"), contains("Cluster"),
           contains("Meal.1"), Next.Break.Length.MINS) 
  
  tc<-left_join(tc, tc.M1)
    
# Meal 2 Compliant ---------------------------------------------------------------------------------
  # * Ranking ---------------------------------------------------------------------------------------
  
  tc.M2<- tc %>% 
    arrange(Person.ID, Cluster.Start, desc(Meal.2.Compliant), Meal.2.Expected.Break) %>% 
    group_by(Person.ID, Cluster.Start) %>% 
    mutate(Meal.2.Compliance.Ranking=ifelse(Meal.2.Compliant==1 &
                                    (Meal.1.Compliance.Ranking!=1 | ## Cannot be the same as M1
                                       is.na(Meal.1.Compliance.Ranking)), ## Can be NA for M1 compliant
                                            row_number(),
                                            NA) ) %>% 
    arrange(Person.ID, Cluster.Start, Pair.Order) %>%
    select(Person.ID, ends_with(".dt"), contains("Pair"), contains("Cluster"),  
           contains("Meal.2"), Next.Break.Length.MINS) 
  
  tc<-left_join(tc, tc.M2)
  
  

# tc Reorder ---------------------------------------------------------------------------------------

tc<-tc %>% select(everything(), ##Needed in order to keep 
                  -contains("Meal.1"), contains("Meal.1"), 
                  -contains("Meal.2"), contains("Meal.2"))
  
  glimpse(tc)
  
  
  ##TODO: Address bug with negative tc$Gap.Next being nagative!!
  
  #### THEN
  
  ##TODO: Write another feature into the tc simulation data that has segments over 8hrs with an
  ##      even minute value to be the only segment in the cluster. For testing missing both meals!!
  
  #### THEN
  
  ##TODO: Modify the meal compliant ranking system to rank non-compliant meals because otherwise
  ##       the non-compliant meals will be filtered out. Compliant ranking should be based on
  ##       meal.X.required AND THEN a second meal.X.ranking for use in the line identification.
  
  
  
  stop()
  
  
  #sh<-
  tc %>% 
    ungroup() %>% 
    group_by(Person.ID, Cluster.Start, Cluster.End) %>% 
    mutate(Cluster.Overnight=sum(Cluster.Overnight),
           False.Positive.Clock=sum(False.Positive.Clock)) %>% 
    ungroup() %>% 
    select(Person.ID, Weekday, Pairs.Per.Cluster, False.Positive.Clock, Cluster.Overnight,
           Cluster.Start, Cluster.End, Cluster.Length) %>% 
    glimpse()
    
      
    
  
  
  tc %>%
    filter(Meal.1.Compliance.Ranking==1) %>% glimpse()
    group_by(Person.ID, Cluster.Start) %>%
    select(contains("Meal.1")) %>% 
    glimpse()
    
    
    
    
    
    
    
    
beep(2)
stop()




# * SoL Calcs ------------------------------------------------------------------------------------
tc<-tc %>% 
  ungroup() %>% 
  mutate(In.4yr.SOL=ifelse(Date>date(Filing.Date)-years(4), 1, 0),
         WW.4yr.SOL=ifelseC(In.4yr.SOL==1, Start.Weekly.WW, NA),
         PP.4yr.SOL=ifelseC(In.4yr.SOL==1 & Client.PP.Gap=="Weekly", Start.Weekly.WW, NA),
         PP.4yr.SOL=ifelseC(In.4yr.SOL==1 & Client.PP.Gap!="Weekly", Start.Biweekly.WW, PP.4yr.SOL),
         
         In.3yr.SOL=ifelse(Date>date(Filing.Date)-years(3), 1, 0),
         WW.3yr.SOL=ifelseC(In.3yr.SOL==1, Start.Weekly.WW, NA),
         PP.3yr.SOL=ifelseC(In.3yr.SOL==1 & Client.PP.Gap=="Weekly", Start.Weekly.WW, NA),
         PP.3yr.SOL=ifelseC(In.3yr.SOL==1 & Client.PP.Gap!="Weekly", Start.Biweekly.WW, PP.3yr.SOL),
         
         In.2yr.SOL=ifelse(Date>date(Filing.Date)-years(2), 1, 0),
         WW.2yr.SOL=ifelseC(In.2yr.SOL==1, Start.Weekly.WW, NA),
         PP.2yr.SOL=ifelseC(In.2yr.SOL==1 & Client.PP.Gap=="Weekly", Start.Weekly.WW, NA),
         PP.2yr.SOL=ifelseC(In.2yr.SOL==1 & Client.PP.Gap!="Weekly", Start.Biweekly.WW, PP.2yr.SOL),
         
         In.1yr.SOL=ifelse(Date>date(Filing.Date)-years(1), 1, 0),
         WW.1yr.SOL=ifelseC(In.1yr.SOL==1, Start.Weekly.WW, NA),
         PP.1yr.SOL=ifelseC(In.1yr.SOL==1 & Client.PP.Gap=="Weekly", Start.Weekly.WW, NA),
         PP.1yr.SOL=ifelseC(In.1yr.SOL==1 & Client.PP.Gap!="Weekly", Start.Biweekly.WW, PP.1yr.SOL),
         
         In.PAGA.SOL=ifelse(Date>date(Filing.Date)-years(1), 1, 0),
         WW.PAGA.SOL=ifelseC(In.PAGA.SOL==1, Start.Weekly.WW, NA),
         PP.PAGA.SOL=ifelseC(In.PAGA.SOL==1 & Client.PP.Gap=="Weekly", Start.Weekly.WW, NA),
         PP.PAGA.SOL=ifelseC(In.PAGA.SOL==1 & Client.PP.Gap!="Weekly", Start.Biweekly.WW, PP.PAGA.SOL)
  ) %>% 
  select(-contains("In.*SOL"), -contains("WW.*SOL"), -contains("PP.*SOL"),
         everything(),
         contains("In.*SOL"), contains("WW.*SOL"), contains("PP.*SOL") )

table(tc$Person.ID, tc$In.4yr.SOL)
table(tc$Person.ID, !is.na(tc$WW.4yr.SOL))
table(tc$Person.ID, !is.na(tc$PP.4yr.SOL))


## tc %>%
##   group_by(Person.ID) %>% 
##   summarize(WW.4yr.SOL=uniqueN(WW.4yr.SOL, na.rm=TRUE),
##             PP.4yr.SOL=uniqueN(PP.4yr.SOL, na.rm=TRUE),
##             
##             WW.3yr.SOL=uniqueN(WW.3yr.SOL, na.rm=TRUE),
##             PP.3yr.SOL=uniqueN(PP.3yr.SOL, na.rm=TRUE),
##             
##             WW.2yr.SOL=uniqueN(WW.2yr.SOL, na.rm=TRUE),
##             PP.2yr.SOL=uniqueN(PP.2yr.SOL, na.rm=TRUE),
##             
##             WW.1yr.SOL=uniqueN(WW.1yr.SOL, na.rm=TRUE),
##             PP.1yr.SOL=uniqueN(PP.1yr.SOL, na.rm=TRUE),
##             
##             WW.PAGA.SOL=uniqueN(WW.PAGA.SOL, na.rm=TRUE),
##             PP.PAGA.SOL=uniqueN(PP.PAGA.SOL, na.rm=TRUE) )


##TODO: make a pay period and work week apscer object
##TODO: Statute of limitations calculations (WW, PPs, & Within)
##TODO: late & short meal calcs
##TODO: build compliance w/ waivers
##TODO: build violations w/ waivers
##TODO: check if compliance and violations have no overlap
##TODO: violation.type & break.types & non-meal break classifier
##TODO: deminimus with timecard data object


##TODO: Build shift object collapse
##TODO: merging in all data for meal 1
##TODO: merging in all data for meal 2
##TODO: all violations and compliance w/ waivers
##TODO: 



