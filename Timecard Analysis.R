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

#require("checkpoint")
#checkpoint("2019-12-07", project=WD, forceProject = TRUE)
#setSnapshot("2019-12-07")

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
            "beepr"))      ## for placing audio chimes to signal completed calculations

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
False.Positive.Clock.VAR<-c(0)       ## If the tmins b/w a time pair to the next THEN it is not real
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

if (is.null(False.Positive.Clock.VAR)==T | 
    is.na(False.Positive.Clock.VAR)==T | 
    !is.numeric(False.Positive.Clock.VAR)==T) 
  warning("False.Positive.Clock.VAR must be of class: numeric")

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
  mutate(Date=V1) %>% 
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




# *** ----------------------------------------------------------------------------------------------
# tc Analysis --------------------------------------------------------------------------------------
# * Overnight Split --------------------------------------------------------------------------------



  
  tc.No.Overnights<-tc %>% ungroup() %>% filter(date(In.Actual.dt)==date(Out.Actual.dt))
  tc.Overnights<-tc %>% ungroup() %>% filter(date(In.Actual.dt)!=date(Out.Actual.dt))
  
  dim(tc.No.Overnights)
  dim(tc.Overnights)
  
  
  ## Splitting time pairs on overnights
    ##Censoring the 1st paert of the pair to be ymd_hms to ymd_00:00:00
    tc.Overnights.1<-tc.Overnights %>% 
      mutate(Out.Actual.dt=ymd_hms(paste(date(Out.Actual.dt), "00:00:00")) )
    
    ##Censoring the 2nd paert of the pair to be ymd_00:00:00 to ymd_hms
    tc.Overnights.2<-tc.Overnights %>% 
      mutate(In.Actual.dt=ymd_hms(paste(date(Out.Actual.dt), "00:00:00")) )
  
  
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
    ungroup() %>% 
    filter(!(In.Actual.dt==Out.Actual.dt & Gap.Last==0)) %>% ## Critical Step due to sim data
                                                             ## Below is recalc code & note explaining
    arrange(Person.ID, In.Actual.dt) %>% 
    group_by(Person.ID) %>% 
    ##NOTE: NAs here are '123456' to allow for later calculations
    mutate(Gap.Next=as.numeric(difftime(lead(In.Actual.dt), Out.Actual.dt, units="hours")),
           Gap.Next=ifelse(is.na(Gap.Next), 123456, Gap.Next),
           Gap.Last=as.numeric(difftime(In.Actual.dt, lag(Out.Actual.dt), units="hours")) ,
           Gap.Last=ifelse(is.na(Gap.Last), 123456, Gap.Last))
  
  
  ## NOTE: This section is simply removing weird false positivie clocks where there is not even time
  ##       occuring between the clocks themselves. Example being a shift that truly had 1 pair would
  ##       seem to have 2 pairs despite one pair having a length of 0hrs & no time off the clock.
  ##       Effectively the data had 135 rows of data where it wasn't meaningful (same time & no gaps)
  

  
  
# * Shift Group -------------------------------------------------------------------------------------
tc<-tc %>% 
    group_by(Person.ID) %>% 
    mutate(Pair.Length=as.numeric(difftime(Out.Actual.dt, In.Actual.dt, units="hours"))) %>% 
    mutate(Shift.Start=ifelseC(Gap.Last>ResetLength, In.Actual.dt, NA),
           Shift.Start=fillNA(Shift.Start) ) %>% 
    group_by(Person.ID, Shift.Start) %>% 
    mutate(Shift.End=max(Out.Actual.dt),
           Pair.Order=row_number(),
           Pairs.Per.Shift=n(),
           Shift.Accum=cumsum(Pair.Length),
           Shift.Length=as.numeric(difftime(Shift.End, Shift.Start, units="hours")),
           Valid.Break=ifelse(Gap.Next>0 & Gap.Next<ResetLength, 1, 0),
           Next.Break.Length.MINS=ifelse(Valid.Break==1, Gap.Next*60, NA),
           ) %>% 
    ungroup() %>% 
    mutate(Shift.Overnight=ifelse(date(Shift.Start)!=date(Shift.End), T, F),
           False.Positive.Clock=ifelse(Gap.Next==0 | In.Actual.dt==Out.Actual.dt, T, F))

  
  

# * Meal Calcs -------------------------------------------------------------------------------------
# ** Late & Short ----------------------------------------------------------------------------------

tc<-tc %>% 
  ungroup() %>% 
  mutate(##Meal 1 Calculations
         Meal.1.Required=ifelse(Shift.Length>Meal.1.Req, 1, 0),
         Meal.1.Late.MINS=ifelse(Shift.Accum>Meal.1.Req, (Shift.Accum-Meal.1.Req)*60, 0),
         Meal.1.Short.MINS=ifelse(Next.Break.Length.MINS<=Meal.Break.Req.MINS &
                                    !is.na(Next.Break.Length.MINS), 
                                  Meal.Break.Req.MINS-Next.Break.Length.MINS,
                                  0),
         
         Meal.1.LateShort.MINS=Meal.1.Late.MINS+Meal.1.Short.MINS,
         
         Meal.1.Compliant=ifelse(Meal.1.Required==1, 0, NA),
         Meal.1.Compliant=ifelse(Meal.1.Required==1 &
                                   Next.Break.Length.MINS>=Meal.Break.Req.MINS &
                                   Meal.1.LateShort.MINS==0,
                                 1,
                                 Meal.1.Compliant),
         Meal.1.Compliant=ifelse(is.na(Next.Break.Length.MINS), 0, Meal.1.Compliant),
         Meal.1.Compliant=ifelse(False.Positive.Clock==TRUE, 0, Meal.1.Compliant),
         
         
         Meal.1.Length=ifelse(Meal.1.Required==1, Next.Break.Length.MINS, NA),
          ## Note: Meal.1.Norm.Meal.Time expects M1 1/3 through a shift for natural fit
         Meal.1.Norm.Meal.Time=ifelse(!is.na(Meal.1.Length),
                                        round(abs(Shift.Length*(1/3)-Shift.Accum), 3),
                                        NA),
         
         
         ## Meal 2 Calculations
         Meal.2.Required=ifelse(Shift.Length>Meal.2.Req, 1, 0),
         Meal.2.Late.MINS=ifelse(Shift.Accum>Meal.2.Req, (Shift.Accum-Meal.2.Req)*60, 0),
         Meal.2.Short.MINS=ifelse(Next.Break.Length.MINS<=Meal.Break.Req.MINS & 
                                    !is.na(Next.Break.Length.MINS), 
                                  Meal.Break.Req.MINS-Next.Break.Length.MINS,
                                  0),
         Meal.2.LateShort.MINS=Meal.2.Late.MINS+Meal.2.Short.MINS,
         
         Meal.2.Compliant=ifelse(Meal.2.Required==1, 0, NA),
         Meal.2.Compliant=ifelse(Meal.2.Required==1 & 
                                   Next.Break.Length.MINS>=Meal.Break.Req.MINS &
                                   Meal.2.LateShort.MINS==0,
                                 1,
                                 Meal.2.Compliant),
         Meal.2.Compliant=ifelse(is.na(Next.Break.Length.MINS), 0, Meal.2.Compliant),
         Meal.2.Compliant=ifelse(False.Positive.Clock==TRUE, 0, Meal.2.Compliant),
         
         Meal.2.Length=ifelse(Meal.2.Required==1, Next.Break.Length.MINS, NA),
          ## Note: Meal.1.Norm.Meal.Time expects M2 1/3 through a shift for natural fit
         Meal.2.Norm.Meal.Time=ifelse(!is.na(Meal.2.Length),
                                      round(abs(Shift.Length*(2/3)-Shift.Accum), 3),
                                      NA) )
  

# Meal 1 Compliant ---------------------------------------------------------------------------------
# * Ranking ---------------------------------------------------------------------------------------

  tc.M1<- tc %>% ## Below arranges tc by the shift and orders by M1 compliance & expected
    ungroup() %>% 
    ### TODO: CHANGE this to remove any is.na(Next.Break.Length.MINS), easier to remove false gaps this way & avoid assigning a ranking to an impossible gap in time
    filter(Meal.1.Required==1 & !is.na(Next.Break.Length.MINS) & False.Positive.Clock==FALSE) %>% 
    arrange(Person.ID, Shift.Start, desc(Meal.1.Compliant), Meal.1.Norm.Meal.Time, In.Actual.dt) %>% 
    group_by(Person.ID, Shift.Start) %>% 
    mutate(Meal.1.Compliance.Ranking=ifelse(Meal.1.Compliant==1, row_number(), NA),
           Meal.1.Rankings=row_number()) %>% 
    arrange(Person.ID, Shift.Start, Pair.Order) %>%
    select(Person.ID, ends_with(".dt"), contains("Pair"), contains("Shift"),
           contains("Meal.1"), Next.Break.Length.MINS) 
  
  tc<-left_join(tc, tc.M1)
    
# Meal 2 Compliant ---------------------------------------------------------------------------------
  # * Ranking ---------------------------------------------------------------------------------------
  
  tc.M2<- tc %>% 
    ungroup() %>% 
    filter(Meal.2.Required==1 & !is.na(Next.Break.Length.MINS) & False.Positive.Clock==FALSE) %>% 
    filter(Meal.1.Rankings!=1 | is.na(Meal.1.Rankings) ) %>% 
    filter(!is.na(Next.Break.Length.MINS)) %>% 
    arrange(Person.ID, Shift.Start, desc(Meal.2.Compliant), Meal.2.Norm.Meal.Time, In.Actual.dt) %>% 
    group_by(Person.ID, Shift.Start) %>% 
    mutate(Meal.2.Compliance.Ranking=ifelse(Meal.2.Compliant==1, ## Can be NA for M1 compliant
                                            row_number(),
                                            NA),
           Meal.2.Rankings=row_number() ) %>% 
    arrange(Person.ID, Shift.Start, Pair.Order) %>%
    select(Person.ID, ends_with(".dt"), contains("Pair"), contains("Shift"),  
           contains("Meal.2"), Next.Break.Length.MINS) 
  
  tc<-left_join(tc, tc.M2)

# tc Reorder ---------------------------------------------------------------------------------------

tc<-tc %>% select(everything(), ##Needed in order to keep 
                  -contains("Meal.1"), contains("Meal.1"), 
                  -contains("Meal.2"), contains("Meal.2"))
  

# ** Quick QC --------------------------------------------------------------------------------------
  
  tableRAW(tc$Meal.1.Rankings)
  tableRAW(tc$Meal.2.Rankings)
  
  table(paste(tc$Meal.1.Rankings), paste(tc$Meal.2.Rankings))
  table(paste(tc$Meal.1.Compliance.Ranking), paste(tc$Meal.2.Compliance.Ranking))
  

  tc %>% 
    group_by(Shift.Start) %>% 
    summarize(Meal.Breaks=sum(Gap.Next>=0.5, na.rm=T),
              Meal.1.Comp=sum(Meal.1.Compliant, na.rm=TRUE),
              M2.Req=sum(Shift.Length>10)>0) %>%
    arrange(desc(M2.Req), desc(Meal.1.Comp), desc(Meal.Breaks)) %>% 
    head()
  
  tc %>%
    filter(Shift.Start==ymd_hms("2010-03-08 15:54:00")) %>% 
    glimpse()
    #View(., "tc QC: 2010-03-08 15:54:00")
  
  
# *** ----------------------------------------------------------------------------------------------
# sh Analysis --------------------------------------------------------------------------------------

  sh<-tc %>% 
    ungroup() %>% 
    mutate(Date=lubridate::date(Shift.Start)) %>% 
    select(-ends_with("Weekly.WW")) %>% 
    left_join(., WorkWeekSpacer) %>% 
    group_by(Start.Weekly.WW, End.Weekly.WW, Start.Biweekly.WW, End.Biweekly.WW, 
             Person.ID, Date, 
             Shift.Start, Shift.End, Shift.Length, Pairs.Per.Shift) %>% 
    summarize(Shift.Overnight=sum(Shift.Overnight)>0,
              False.Positive.Clock=sum(False.Positive.Clock)>0,
              
              ##Meal Requirements
              Meal.Required=(sum(Meal.1.Required, na.rm=TRUE)+sum(Meal.2.Required, na.rm=T))>0,
              Meal.1.Required=sum(Meal.1.Required, na.rm=TRUE)>0,
              Meal.2.Required=sum(Meal.2.Required, na.rm=TRUE)>0 ) %>% 
    ungroup()
  
  #Quick test to double check if the shift collapse was grouped correctly!!
  stopifnot(uniqueN(paste(sh$Person.ID, sh$Shift.Start))==uniqueN(paste(tc$Person.ID, tc$Shift.Start)))
  

# * Joining M1 & M2 ------------------------------------------------------------------------------------

  sh<-sh %>% 
# ** M1 Join ---------------------------------------------------
    left_join(.,
              tc %>% 
                ungroup() %>% 
                mutate(Meal.1.Pair=paste("b/w", Pair.Order, "&", Pair.Order+1, "of", Pairs.Per.Shift),
                       Meal.1.Break.Start=ifelseC(Meal.1.Rankings==1, Out.Actual.dt, NA),
                       Meal.1.Break.End=ifelseC(Meal.1.Rankings==1, lead(In.Actual.dt), NA)) %>% 
                filter(Meal.1.Rankings==1) %>% 
                select(Person.ID, Shift.Start, contains("Meal.1"), 
                       -Meal.1.Length, Meal.1.Length, 
                       -Meal.1.Compliant, Meal.1.Compliant)
              ) %>% 
# ** M1 Features ----
  mutate(Meal.1.Violation=ifelse(Meal.1.Compliant!=1 & Meal.1.Required==1, 1, 0),
         Meal.1.Violation=ifelse(is.na(Meal.1.Compliant) & Meal.1.Required==1, 1, Meal.1.Violation),
         Meal.1.Violation=ifelse(Meal.1.Required==1, Meal.1.Violation, NA),
         Meal.1.Violation.TYPE=ifelse(Meal.1.Violation==1 & Meal.1.LateShort.MINS>0,
                                      "Short & Late",
                                      "ERROR"),
         Meal.1.Violation.TYPE=ifelse(Meal.1.Violation==1 & Meal.1.Short.MINS>0 & Meal.1.Late.MINS==0,
                                      "Short",
                                      Meal.1.Violation.TYPE),
         Meal.1.Violation.TYPE=ifelse(Meal.1.Violation==1 & Meal.1.Late.MINS>0 & Meal.1.Short.MINS==0,
                                      "Late",
                                      Meal.1.Violation.TYPE),
         Meal.1.Violation.TYPE=ifelse(is.na(Meal.1.Rankings),
                                      "Missed", 
                                      Meal.1.Violation.TYPE),
         Meal.1.Violation.TYPE=ifelse(Meal.1.Violation==0,
                                       "Compliant",
                                      Meal.1.Violation.TYPE)
         ) %>% 
    ##Imputing some data on the missed meals
    mutate(Meal.1.Late.MINS=ifelse(Meal.1.Violation.TYPE=="Missed", 0, Meal.1.Late.MINS),
           Meal.1.Short.MINS=ifelse(Meal.1.Violation.TYPE=="Missed", 0, Meal.1.Short.MINS),
           Meal.1.LateShort.MINS=ifelse(Meal.1.Violation.TYPE=="Missed", 0, Meal.1.LateShort.MINS)
           ) %>% 
    
# ** M2 Join ---------------------------------------------------
   left_join(.,
             tc %>% 
               ungroup() %>% 
               mutate(Meal.2.Pair=paste("b/w", Pair.Order, "&", Pair.Order+1, "of", Pairs.Per.Shift),
                      Meal.2.Break.Start=ifelseC(Meal.2.Rankings==1, Out.Actual.dt, NA),
                      Meal.2.Break.End=ifelseC(Meal.2.Rankings==1, lead(In.Actual.dt), NA) ) %>% 
               filter(Meal.2.Rankings==1) %>% 
               select(Person.ID, Shift.Start, contains("Meal.2"), 
                      -Meal.2.Length, Meal.2.Length, 
                      -Meal.2.Compliant, Meal.2.Compliant) ) %>% 
 ## ** M2 Features ----
   mutate(Meal.2.Violation=ifelse(Meal.2.Compliant!=1 & Meal.2.Required==1, 1, 0),
          Meal.2.Violation=ifelse(is.na(Meal.2.Compliant) & Meal.2.Required==1, 1, Meal.2.Violation),
          Meal.2.Violation=ifelse(Meal.2.Required==1, Meal.2.Violation, NA),
          Meal.2.Violation.TYPE=ifelse(Meal.2.Violation==1 & Meal.2.LateShort.MINS>0,
                                       "Short & Late",
                                       "ERROR"),
          Meal.2.Violation.TYPE=ifelse(Meal.2.Violation==1 & Meal.2.Short.MINS>0 & Meal.2.Late.MINS==0,
                                       "Short",
                                       Meal.2.Violation.TYPE),
          Meal.2.Violation.TYPE=ifelse(Meal.2.Violation==1 & Meal.2.Late.MINS>0 & Meal.2.Short.MINS==0,
                                       "Late",
                                       Meal.2.Violation.TYPE),
          Meal.2.Violation.TYPE=ifelse(is.na(Meal.2.Rankings),
                                       "Missed", 
                                       Meal.2.Violation.TYPE),
          Meal.2.Violation.TYPE=ifelse(Meal.2.Violation==0,
                                       "Compliant",
                                       Meal.2.Violation.TYPE) ) %>% 
    ##Imputing some data on the missed meals
    mutate(Meal.2.Late.MINS=ifelse(Meal.2.Violation.TYPE=="Missed", 0, Meal.2.Late.MINS),
           Meal.2.Short.MINS=ifelse(Meal.2.Violation.TYPE=="Missed", 0, Meal.2.Short.MINS),
           Meal.2.LateShort.MINS=ifelse(Meal.2.Violation.TYPE=="Missed", 0, Meal.2.LateShort.MINS)) %>% 
    
# *** Joint Features -------------------------------------------------------------------------------
    mutate(Shift.Compliant=ifelse( (Meal.1.Violation.TYPE=="Compliant" &
                                      Meal.2.Violation.TYPE=="Compliant") |
                                    (Meal.1.Violation.TYPE=="Compliant" & 
                                       is.na(Meal.2.Violation.TYPE)),
                                  1,
                                  0),
           Shift.Violation=ifelse(Shift.Compliant==0, 1, 0) )%>%
    mutate(Shift.Violation.Type="ERROR",
           ##No Meal Required
           Shift.Violation.Type=ifelse(is.na(Meal.1.Violation),
                                       "No Meal Required",
                                       Shift.Violation.Type),
           
           ##Both Meals Compliant
           Shift.Violation.Type=ifelse(Meal.1.Violation==0 & Meal.2.Violation==0,
                                       NA,
                                       Shift.Violation.Type),
           
           #Both Meals Violated
           Shift.Violation.Type=ifelse(Meal.1.Violation==1 & Meal.2.Violation==1,
                                       paste("Both - Meal 1:", Meal.1.Violation.TYPE,
                                             " & Meal 2:", Meal.2.Violation.TYPE),
                                       Shift.Violation.Type),
           #2nd Meal Violation
           Shift.Violation.Type=ifelse(Meal.1.Violation==0 & Meal.2.Violation==1,
                                        paste("Meal 2:", Meal.2.Violation.TYPE),
                                        Shift.Violation.Type),
           
           #1st Meal Violation
           Shift.Violation.Type=ifelse(Meal.1.Violation==1 & 
                                         (is.na(Meal.2.Violation) | Meal.2.Violation==0),
                                       paste("Meal 1:", Meal.1.Violation.TYPE),
                                       Shift.Violation.Type)
           )
  
  
  glimpse(sh)

  #tableRAW(sh$Meal.1.Violation)
  #tableRAW(sh$Meal.2.Violation)
  table(paste(sh$Meal.1.Violation), paste(sh$Meal.2.Violation==1))
  table(paste(sh$Shift.Violation.Type), paste(sh$Shift.Compliant))
  
#  tableRAW(sort(sh$Shift.Violation.Type2))
  beepr::beep(2)
  stop()
  
  
  
  
  

# * ------------------------------------------------------------------------------------------------
# Quick QC -----------------------------------------------------------------------------------------
  
  print(" --------------- Meal 1 Tables --------------- ")
  table(paste(sh$Meal.1.Compliant), paste(sh$Meal.1.Required))
  table(paste(sh$Meal.1.Violation.TYPE), paste(sh$Meal.1.Required))
  table(paste(sh$Meal.1.Violation.TYPE), paste(sh$Meal.1.Rankings))
  table(paste(sh$Meal.1.Violation.TYPE), paste(sh$Meal.1.Compliance.Ranking))
  
  print(" --------------- Meal 2 Tables --------------- ")
  table(paste(sh$Meal.2.Compliant), paste(sh$Meal.2.Required))
  table(paste(sh$Meal.2.Violation.TYPE), paste(sh$Meal.2.Required))
  table(paste(sh$Meal.2.Violation.TYPE), paste(sh$Meal.2.Rankings))
  table(paste(sh$Meal.2.Violation.TYPE), paste(sh$Meal.2.Compliance.Ranking))
  
  print("Shift Overall")
  table(paste(sh$Shift.Violation.Type), paste(sh$Meal.1.Required))
  table(paste(sh$Shift.Violation.Type), paste(sh$Meal.2.Required))
  table(paste(sh$Shift.Violation.Type), paste(sh$Meal.1.Violation))
  table(paste(sh$Shift.Violation.Type), paste(sh$Meal.2.Violation))
  table(paste(sh$Shift.Violation.Type), paste(sh$Shift.Violation))
  table(paste(sh$Shift.Violation.Type), paste(sh$Shift.Compliant))
  
  
  table(paste(sh$Meal.1.Violation.TYPE), paste(sh$Meal.2.Violation.TYPE))
  
  
  

  #tc %>% filter(str_detect(Shift.Start, "2014-08-30 09:53:00")) %>% View(., "tc: 2014-08-30 09:53:00")
  #sh %>% filter(str_detect(Shift.Start, "2014-08-30 09:53:00")) %>% View(., "sh: 2014-08-30 09:53:00")
  
  ##TODO need to address missing meal 1s due to it causing an issue with the summary stats...
  ##
  ## EXAMPLE: 2010-09-10 08:29:00
  ##
  
  
  glimpse(sh)
  
beep(2)
stop()









# *** ----------------------------------------------------------------------------------------------



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



glimpse(sh)
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


##TODO: make a pay period and work week apscer object                 ## done
##TODO: Statute of limitations calculations (WW, PPs, & Within)       ## done
##TODO: late & short meal calcs                                       ## done
##TODO: build compliance w/ waivers                                   ## 
##TODO: build violations w/ waivers                                   ##
##TODO: check if compliance and violations have no overlap            ##
##TODO: violation.type & break.types & non-meal break classifier      ##
##TODO: deminimus with timecard data object                           ##


##TODO: Build shift object collapse
##TODO: merging in all data for meal 1
##TODO: merging in all data for meal 2
##TODO: all violations and compliance w/ waivers
##TODO: 



