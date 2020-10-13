

# Inputs & Parameters ------------------------------------------------------------------------------
Number.Of.Emps__<-400
Filing.Date<-ymd("2019-12-06") ## Implying SOL 4yr is 2015-12-06
Date.Sim.Start__<-(Filing.Date-years(4)-days(30*8))
Plot.Status.TRUE_FALSE__<-TRUE



Person.ID.Vector__<-paste0("_", str_pad( seq(1, Number.Of.Emps__, 1), width=9, side="left", pad="0" ))
Person.ID.Vector__ %>% head()

Hire.Date.Jump.Start__<-ceiling( 90 + ceiling( runif(Number.Of.Emps__, min = 0, max = (365*4)) ) )
summary(Hire.Date.Jump.Start__)
qplot(Hire.Date.Jump.Start__, binwidth=30)


Days.Employed__<-ceiling( runif(Number.Of.Emps__, min = 14, max = (365*2.0)) + 
                          ceiling( rnorm(Number.Of.Emps__, mean=(365), sd=180) ) )
Days.Employed__<-ifelse(Days.Employed__<0, 18, Days.Employed__)
##Days.Employed__<-ceiling( rnorm(Number.Of.Emps__, mean=(365*2), sd=120) )
summary(Days.Employed__)
qplot(Days.Employed__, binwidth=30)




# Build Census -------------------------------------------------------------------------------------

Employee.Census<-data.frame(
  stringsAsFactors = FALSE,
  Person.ID = Person.ID.Vector__,
  Date.Sim.Start=Date.Sim.Start__,
  Hire.Date.Jump.Start=Hire.Date.Jump.Start__,
  Days.Employed=Days.Employed__ )

Employee.Census %>% glimpse()

Employee.Census<-Employee.Census %>% 
  mutate(Hire.Date=(Date.Sim.Start+days(Hire.Date.Jump.Start)),
         Hire.Date=ifelse(Hire.Date<(Filing.Date-years(4)), 
                          paste((Filing.Date-years(4))), 
                          paste(Hire.Date)),
         Hire.Date=ymd(Hire.Date),
         
         Term.Date=(Hire.Date + days(Days.Employed)),
         Term.Date=ifelse(Term.Date<Hire.Date,
                          paste(Hire.Date),
                          paste(Term.Date)),
         Term.Date=ymd(Term.Date),
         # Term.Date=ifelse(Term.Date>ymd("2020-09-01"),
         #                  ymd(NA),
         #                  paste(Term.Date)),
         # Term.Date=ymd(Term.Date),
         
         
         Actual.Days.Employed=as.numeric(difftime(Term.Date, Hire.Date, unit="days")),
         Actual.Days.Employed_ALT=replace_na(Actual.Days.Employed, 1999),
         
         Interval_6Months=as.factor(Actual.Days.Employed%/%182.5) )

Employee.Census<-Employee.Census %>% 
  select(Date.Sim.Start, Hire.Date.Jump.Start, Days.Employed,
         Person.ID, Hire.Date, Term.Date, 
         Actual.Days.Employed, Actual.Days.Employed_ALT, Interval_6Months)

summary(Employee.Census$Actual.Days.Employed)


Employee.Census %>% glimpse()
table(Employee.Census$Term.Date>ymd("2020-09-01"))




# * Census Plots -----------------------------------------------------------------------------------

if (Plot.Status.TRUE_FALSE__){
  
  qplot(data=Employee.Census, x = Hire.Date, binwidth=14, fill = Interval_6Months)
  qplot(data=Employee.Census, x = Term.Date, binwidth=14, fill = Interval_6Months)
  qplot(data=Employee.Census, x= Actual.Days.Employed_ALT, binwidth=14)
  
  qplot(data = Employee.Census,
        x = Hire.Date, 
        y = Actual.Days.Employed_ALT,
        color = Interval_6Months,
        #binwidth=30
        )
  
  qplot(data=Employee.Census,
        main = "When were people Hired? And how long did they work? \n FACET: if(Active after 2020-09)",
        x = Hire.Date,
        y = Actual.Days.Employed_ALT,
        facets = Term.Date>ymd("2020-09-01")~.,
        color = Interval_6Months )
  
}



# Date Expansion -----------------------------------------------------------------------------------
DF.tc<-setDT(Employee.Census)[, .(Date = seq(Hire.Date, Term.Date, by = '1 day')), by = Person.ID]

if (Plot.Status.TRUE_FALSE__){
  
  DF.tc %>% 
    filter(Date<ymd("2020-09-01")) %>% 
    arrange(Date, Person.ID) %>% 
    mutate(Year.Month=ymd(paste(year(Date), month(Date), "01", sep="-")),
           Year_Color=as.factor(year(Date)) ) %>% 
    group_by(Year.Month, Year_Color) %>% 
    summarize(Person.ID.Count=n_distinct(Person.ID)) %>% 
  qplot(data=.,
        main = "Employee Count Monthly",
        x=Year.Month,
        y=Person.ID.Count,
        color=Year_Color)
  
}


# Timecard Features --------------------------------------------------------------------------------
DF.tc %>% dim()

DF.tc<-DF.tc %>% mutate(Day.Off=(runif(n(), min=0, max=7)>=5) )  ## (7-X) days off per week -- 


DF.tc %>% 
  group_by(Person.ID) %>% 
  summarize(Days=n(),
            Days.Off=sum(Day.Off),
            Perc.Days.Off=Days.Off/Days) %>% 
  ungroup() %>% 
  mutate(AVG_Perc.Days.Off=mean(Perc.Days.Off),
         MED_Perc.Days.Off=median(Perc.Days.Off),
         TOT_Perc.Days.Off=(sum(Days.Off)/sum(Days))) %>% 
  qplot(data=.,
        x = Perc.Days.Off,
        xlim = c(0, 1),
        binwidth = 0.025)



DF.tc<-DF.tc %>% 
  filter(Day.Off==FALSE) %>% 
  mutate(Worker.Type=ifelse(as.numeric(str_extract(Person.ID, "\\d+"))%%2==0,
                            "Morning",
                            "Evening"),
         Start.Hour=ifelse(Worker.Type=="Morning", 
                           rnorm(n(), mean=10, sd=1),
                           rnorm(n(), mean=18, sd=1)),
         
         Start.Hour_hm=paste(Start.Hour%/%1, floor((Start.Hour%%1)*60), sep=":"),
         Start.Time.dt=ymd_hm(paste(Date, Start.Hour_hm)),
         
         ### Instance 1 Lengths
         Instance.Length.1=rnorm(n(), mean=4, sd=2),
         Instance.Length.1=(round(Instance.Length.1*60)/60),
         Instance.Length.1=ifelse(Instance.Length.1<0, (60/60), Instance.Length.1), # if <0, then 60
         Instance.Length.1.MINS=as.integer(Instance.Length.1*60),
         
         
         ### Instance 2 Lengths -- This one is conditional upon a few arbitrary points
         Instance.Length.2=ifelse(round(Instance.Length.1)<7 | 
                                    round(Instance.Length.1*60)%%16==0 |
                                    round(Instance.Length.1*60)%%17==0,
                                  rnorm(n(), mean=2.5, sd=2),
                                  0),
         Instance.Length.2=(round(Instance.Length.2*60)/60),
         Instance.Length.2=ifelse(Instance.Length.2<0, (60/60), Instance.Length.2),# if <0, then 60
         Instance.Length.2.MINS=as.integer(Instance.Length.2*60),
         
         
         ### Instance 3 Lengths -- Jointly conditional upon instance 1+2
         Instance.Length.3=ifelse((Instance.Length.1+Instance.Length.2)<(8+runif(n(), min=0, max=2)),
                                  (round(runif(n(), min=0.3, max=2.5)*60)/60),
                                  0),
         Instance.Length.3=(round(Instance.Length.3*60)/60),
         Instance.Length.3=ifelse(Instance.Length.3<0, (0/60), Instance.Length.3),
         Instance.Length.3.MINS=as.integer(Instance.Length.3*60) )

DF.tc<-DF.tc %>% 
  mutate(## Forced censoring for creating Missed Meals
    Instance.Length.2=ifelse(Instance.Length.1>8, 0, Instance.Length.2),
    Instance.Length.2.MINS=ifelse(Instance.Length.1>8, 0, Instance.Length.2.MINS),
    Instance.Length.3=ifelse(Instance.Length.1>8, 0, Instance.Length.3),
    Instance.Length.3.MINS=ifelse(Instance.Length.1>8, 0, Instance.Length.3.MINS) )

DF.tc<-DF.tc %>% 
  mutate(## Shift Totals
    Shift.Length.Total=(Instance.Length.1+Instance.Length.2+Instance.Length.3),
    Shift.Length.Total.MINS=(Instance.Length.1.MINS+Instance.Length.2.MINS+Instance.Length.3.MINS) )

DF.tc<-DF.tc %>% 
  mutate(## Meal Break Timers
    Meal.Break.1.MINS=ifelse(Instance.Length.1==0 & Instance.Length.2==0,
                             0,
                             rbinom(5000, (31*2), 0.50)),
    Meal.Break.1.MINS=ifelse(Instance.Length.1>8, 0, Meal.Break.1.MINS),
    Meal.Break.1.MINS=floor(Meal.Break.1.MINS),
    Meal.Break.2.MINS=ifelse(Instance.Length.2==0 & Instance.Length.3==0,
                             0,
                             rbinom(5000, (29*2), 0.50)),
    Meal.Break.2.MINS=ifelse(Instance.Length.1>8, 0, Meal.Break.2.MINS),
    Meal.Break.2.MINS=ceiling(Meal.Break.2.MINS) )



# * Instance Builder --------------------------------------------------------------------------------
DF.tc<-DF.tc %>% select(-contains("dt"), everything(), Start.Time.dt, contains("dt"))


DF.tc<-DF.tc %>%
  mutate(In.Actual.dt1=Start.Time.dt,
         Out.Actual.dt1=In.Actual.dt1+ minutes(as.integer(Instance.Length.1.MINS)),
         
         In.Actual.dt2=Out.Actual.dt1+minutes(as.integer(Meal.Break.1.MINS)),
         Out.Actual.dt2=In.Actual.dt2+ minutes(as.integer(Instance.Length.2.MINS)),
         
         In.Actual.dt3=Out.Actual.dt2+minutes(as.integer(Meal.Break.2.MINS)),
         Out.Actual.dt3=In.Actual.dt3+ minutes(as.integer(Instance.Length.3.MINS)) ) %>%
  select(Person.ID, 
         contains("dt1"), Instance.Length.1.MINS, Meal.Break.1.MINS,
         contains("dt2"), Instance.Length.2.MINS, Meal.Break.2.MINS,
         contains("dt3"), Instance.Length.3.MINS,
         Shift.Length.Total,
         everything() )





tc.TEMP1<-DF.tc %>% 
  select(Person.ID, contains("dt1")) %>% 
  rename(In.Actual.dt=In.Actual.dt1,
         Out.Actual.dt=Out.Actual.dt1)

tc.TEMP2<-DF.tc %>% 
  select(Person.ID, contains("dt2")) %>% 
  rename(In.Actual.dt=In.Actual.dt2,
         Out.Actual.dt=Out.Actual.dt2)

tc.TEMP3<-DF.tc %>% 
  select(Person.ID, contains("dt3")) %>% 
  rename(In.Actual.dt=In.Actual.dt3,
         Out.Actual.dt=Out.Actual.dt3) %>% 
  filter(In.Actual.dt!=Out.Actual.dt)       ##NOTE: This filter drops ~775 0 length .dt3 rows of data!

tc.TEMP<-full_join(tc.TEMP1, tc.TEMP2)
tc.TEMP<-full_join(tc.TEMP, tc.TEMP3)


tc<-tc.TEMP
rm(list=str_subset(ls(), "^tc.TEMP|^DF.tc$|__$") )


