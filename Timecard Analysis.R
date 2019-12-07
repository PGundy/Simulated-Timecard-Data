rm(list=ls())

# *************** ----------------------------------------------------------------------------------
# Data Sourcing ----------------------------------------------------------------------------------

tic()
source(file.path("Projects/Simulated Data/Simulated-Timecard-Data", "Simulated Timecard Data.R"))
glimpse(tc)

tc<-tc %>% 
  arrange(Person.ID, In.Actual.dt) %>% 
  group_by(Person.ID) %>% 
  mutate(Gap.Next=as.numeric(difftime(lead(In.Actual.dt), Out.Actual.dt, units="hours")),
         Gap.Next=ifelse(is.na(Gap.Next), 123456, Gap.Next),
         Gap.Last=as.numeric(difftime(In.Actual.dt, lag(Out.Actual.dt), units="hours")) ,
         Gap.Last=ifelse(is.na(Gap.Last), 123456, Gap.Last)) %>% 
  ungroup()


ResetLength<-c(4)
False.Positive.Clock<-c(0) ## If the tmins b/w a time pair to the next THEN it is not real

tc<-tc %>% 
  group_by(Person.ID) %>% 
  mutate(Pair.Length=as.numeric(difftime(Out.Actual.dt, In.Actual.dt, units="hours"))) %>% 
  mutate(Cluster.Start=ifelseC(Gap.Next<ResetLength & Gap.Last>ResetLength, In.Actual.dt, NA),
         Cluster.Start=fillNA(Cluster.Start) ) %>% 
  group_by(Person.ID, Cluster.Start) %>% 
  mutate(Cluster.End=max(Out.Actual.dt),
         Cluster.Length=as.numeric(difftime(Cluster.End, Cluster.Start, units="hours")),
         Next.Break.Length.MINS=ifelse(Gap.Next>0 & Gap.Next<ResetLength, Gap.Next*60, NA) ) %>% 
  mutate(Clock.Overnight=ifelse(date(In.Actual.dt)!=date(Out.Actual.dt), 1, 0),
         False.Positive.Clock=ifelse(Gap.Next %in% False.Positive.Clock, 1, 0))



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



glimpse(tc)
toc()


# *************** ----------------------------------------------------------------------------------
# Experiment to make more Employees ----------------------------------------------------------------
# Employee IDs -------------------------------------------------------------------------------------


#Employee.Count<-250
#
#AM.Person.IDs<-seq(1, Employee.Count/2, 1) %>% 
#  str_pad(., width = 6, pad="0") %>% 
#  paste0("_", .)
#
#PM.Person.IDs<-seq((Employee.Count/2)+1, Employee.Count, 1) %>% 
#  str_pad(., width = 6, pad="0") %>% 
#  paste0("_", .)
#
#Person.ID<-append(AM.Person.IDs, PM.Person.IDs) %>% 
#  unique(.)



# *************** ----------------------------------------------------------------------------------
# TC Analysis --------------------------------------------------------------------------------------





##TODO: Use the 250 simulated IDs from above to cross join 125 into the AM & PM shift EACH!
##TODO: THEN for each employee on each day & each clocking instance (+-)15min on every Instance length
## IF done correctly this will allow for the simulation of a class with approximately normal schedules
### with some reasonable variation for each person throughout the dataset.



