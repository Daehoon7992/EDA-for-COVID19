library(tidyverse)
library(COVID19)
library(data.table)
library(dplyr)
library(ggplot2)
library(forecast)
library(timetk)
library(grid)

set.seed(590184)


# load data ---------------------------------------------------------------
#setwd('./Documents/GitHub/DS440')

## load in Google dataset and merge with COVID dataset
gmr <- "2020_US_Region_Mobility_Report.csv"
DT <- covid19(country="United States", level=2, gmr = gmr)

# Lets exclude a few locals that have less reliable data / sparse data that 
# will only complicate our analysis.
DT <- DT %>% 
  filter(!(administrative_area_level_2 %in% c("Northern Mariana Islands", 
                                              "Guam", 
                                              "Puerto Rico", 
                                              "Virgin Islands", 
                                              "American Samoa")))

################################################################################
# Data cleaning ----
# select necessary variables
DT_cleaned <- DT %>%
  select(date,confirmed,deaths,administrative_area_level_2,
         retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline)

# modify the long name into short one
setnames(DT_cleaned,c('retail_and_recreation_percent_change_from_baseline',
                   'grocery_and_pharmacy_percent_change_from_baseline',
                   'parks_percent_change_from_baseline',
                   'transit_stations_percent_change_from_baseline',
                   'workplaces_percent_change_from_baseline',
                   'residential_percent_change_from_baseline'),
         c('retail_and_recreation', 'grocer_and_pharmacy', 'parks', 
           'transit_stations', 'workplaces', 'residential'))

# make a new variable called new_confirmed and new_deaths - for time series purpose
DT_cleaned <- DT_cleaned %>% 
  mutate(new_confirmed = confirmed - lag(confirmed), 
         new_deaths = deaths - lag(deaths))

# remove the outliers on new_deaths variable
#DT_cleaned[DT_cleaned$administrative_area_level_2=="New York",]$new_deaths > 900
DT_cleaned <- DT_cleaned %>% 
  mutate(new_deaths = ifelse(new_deaths < 0,NA,new_deaths), #for negative number of deaths
         new_deaths = ifelse(new_deaths > 900,NA,new_deaths))

# equalize the time for both Google and COVID dataset
DT_cleaned <- subset(DT_cleaned, date> "2020-03-15" & date < "2020-10-24")

################################################################################
# rolling average ----
# let's do the rolling average for this
mylist <- split(DT_cleaned, DT_cleaned$administrative_area_level_2)
for (i in seq_along(mylist)){
  #for death
  death = frollmean(mylist[[i]]$new_deaths, n=7, algo=c("fast"), align = c("center"))
  df2 = data.frame(movavg_death = death)
  mylist[[i]] <- cbind(mylist[[i]], df2)
  #for mobilities
  retail = frollmean(mylist[[i]]$retail_and_recreation, n=7, algo=c("fast"), align = c("center"))
  df3 = data.frame(movavg_retail = retail)
  mylist[[i]] <- cbind(mylist[[i]], df3)
  #grocery
  grocery = frollmean(mylist[[i]]$grocer_and_pharmacy, n=7, algo=c("fast"), align = c("center"))
  df4 = data.frame(movavg_grocery = grocery)
  mylist[[i]] <- cbind(mylist[[i]], df4)
  #parks
  parks = frollmean(mylist[[i]]$parks, n=7, algo=c("fast"), align = c("center"))
  df5 = data.frame(movavg_parks = parks)
  mylist[[i]] <- cbind(mylist[[i]], df5)
  #transit_stations
  transit = frollmean(mylist[[i]]$transit_stations, n=7, algo=c("fast"), align = c("center"))
  df6 = data.frame(movavg_transit = transit)
  mylist[[i]] <- cbind(mylist[[i]], df6)
  #workplaces
  workplaces = frollmean(mylist[[i]]$workplaces, n=7, algo=c("fast"), align = c("center"))
  df7 = data.frame(movavg_workplaces = workplaces)
  mylist[[i]] <- cbind(mylist[[i]], df7)
  #residential
  residential = frollmean(mylist[[i]]$residential, n=7, algo=c("fast"), align = c("center"))
  df8 = data.frame(movavg_residential = residential)
  mylist[[i]] <- cbind(mylist[[i]], df8)
}
k=2
dataReady<-rbind(mylist[[1]],mylist[[2]])
while(k != 52){
  dataReady<-rbind(dataReady,mylist[[k]])
  k = k + 1
}

################################################################################
#log transformation ----
#let's do log transformation for the data
dataReady2 = na.omit(dataReady)
log_trans.data <- dataReady2 %>%
  mutate(movavg_death = log(movavg_death - min(movavg_death) + 0.01),
         movavg_retail = log(movavg_retail - min(movavg_retail) + 0.01),
         movavg_grocery = log(movavg_grocery - min(movavg_grocery) + 0.01),
         movavg_parks = log(movavg_parks - min(movavg_parks) + 0.01),
         movavg_transit = log(movavg_transit - min(movavg_transit) + 0.01),
         movavg_workplaces = log(movavg_workplaces - min(movavg_workplaces) + 0.01),
         movavg_residential = log(movavg_residential - min(movavg_residential) + 0.01))


################################################################################
#Scale the data ----
### Function for scale
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

################################################################################
# for log transformed DT
transformed_DT <-
  log_trans.data %>%
  group_by(administrative_area_level_2) %>%
  mutate(new_deaths = scale_this(new_deaths),
         retail_and_recreation = scale_this(retail_and_recreation),
         grocer_and_pharmacy = scale_this(grocer_and_pharmacy),
         parks = scale_this(parks),
         transit_stations = scale_this(transit_stations),
         workplaces = scale_this(workplaces),
         residential = scale_this(residential),
         movavg_death = scale_this(movavg_death),
         movavg_retail = scale_this(movavg_retail),
         movavg_grocery = scale_this(movavg_grocery),
         movavg_parks = scale_this(movavg_parks),
         movavg_transit = scale_this(movavg_transit),
         movavg_workplaces = scale_this(movavg_workplaces),
         movavg_residential = scale_this(movavg_residential))


################################################################################
#Visualize ----
# make a plot for each category and mortality for each state
transformed_DT %>% 
  group_by(administrative_area_level_2) %>%
  ggplot(aes(date)) +
  #geom_smooth(aes(y = retail_and_recreation, colour = "retail_and_recreation")) +
  #geom_smooth(aes(y = grocer_and_pharmacy, colour = "grocer_and_pharmacy")) +
  #geom_smooth(aes(y = parks, colour = "parks")) +
  #geom_smooth(aes(y = transit_stations, colour = "transit_stations")) +
  #geom_smooth(aes(y = workplaces, colour = "workplaces")) +
  #geom_smooth(aes(y = residential, colour = "residential")) +
  #geom_smooth(aes(y = new_deaths, colour = "new_deaths")) +
  #geom_smooth(aes(y = movavg_retail, colour = "movavg_retail")) +
  #geom_smooth(aes(y = movavg_grocery, colour = "movavg_grocery")) +
  #geom_smooth(aes(y = movavg_parks, colour = "movavg_parks")) +
  #geom_smooth(aes(y = movavg_transit, colour = "movavg_transit")) +
  #geom_smooth(aes(y = movavg_workplaces, colour = "movavg_workplaces")) +
  geom_smooth(aes(y = movavg_residential, colour = "movavg_residential")) +
  geom_smooth(aes(y = movavg_death, colour = "movavg_deaths")) +
  facet_wrap(~administrative_area_level_2, scales = 'free') +
  theme_bw()+
  labs(y="percent change from baseline",
       title="percent change from baseline-movavg_residentail & movavg_deaths;Log transformed ver.") +
  theme(axis.title.x=element_blank())


#for sample state - NY
DT_NY = subset(scaled_DT, administrative_area_level_2 == "New York")

DT_NY %>%
  ggplot(aes(date)) +
  #geom_smooth(aes(y = movavg_residential, colour = "movavg_residential")) +
  #geom_smooth(aes(y = movavg_retail, colour = "movavg_retail")) +
  #geom_smooth(aes(y = movavg_grocery, colour = "movavg_grocery")) +
  #geom_smooth(aes(y = movavg_parks, colour = "movavg_parks")) +
  geom_smooth(aes(y = movavg_transit, colour = "movavg_transit")) +
  #geom_smooth(aes(y = movavg_workplaces, colour = "movavg_workplaces")) +
  geom_smooth(aes(y= movavg_death, colour = "movavg_deaths")) +
  theme_bw()+
  labs(y="percent change from baseline",
       title="percent change from baseline for NY-transit & death;Log transformed ver.") +
  theme(axis.title.x=element_blank())

#ccf plot
ccf_NY = ggCcf(DT_NY$new_deaths, DT_NY$workplaces,lag.max=35) +
  ggtitle("New York") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
ccf_NY
  
################################################################################
#Cross correlation ----
scaled_DT=na.omit(transformed_DT)

###Let us make a ccf plot for every state
DT_Alabama = subset(scaled_DT_movavg, administrative_area_level_2 == "Alabama")
ccf_Alabama = ggCcf(DT_Alabama$new_deaths,DT_Alabama$residential,lag.max=35) + 
  ggtitle("Alabama") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Alaska = subset(scaled_DT_movavg, administrative_area_level_2 == "Alaska")
ccf_Alaska = ggCcf(DT_Alaska$new_deaths, DT_Alaska$residential,lag.max=35) +
  ggtitle("Alaska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arizona = subset(scaled_DT_movavg, administrative_area_level_2 == "Arizona")
ccf_Arizona = ggCcf(DT_Arizona$new_deaths, DT_Arizona$residential,lag.max=35) +
  ggtitle("Arizona") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arkansas = subset(scaled_DT_movavg, administrative_area_level_2 == "Arkansas")
ccf_Arkansas = ggCcf(DT_Arkansas$new_deaths, DT_Arkansas$residential,lag.max=35) +
  ggtitle("Arkansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_California = subset(scaled_DT_movavg, administrative_area_level_2 == "California")
ccf_California = ggCcf(DT_California$new_deaths, DT_California$residential,lag.max=35) +
  ggtitle("California") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Colorado = subset(scaled_DT_movavg, administrative_area_level_2 == "Colorado")
ccf_Colorado = ggCcf(DT_Colorado$new_deaths, DT_Colorado$residential,lag.max=35) +
  ggtitle("Colorado") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Connecticut = subset(scaled_DT_movavg, administrative_area_level_2 == "Connecticut")
ccf_Connecticut = ggCcf(DT_Connecticut$new_deaths, DT_Connecticut$residential,lag.max=35) +
  ggtitle("Connecticut") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Delaware = subset(scaled_DT_movavg, administrative_area_level_2 == "Delaware")
ccf_Delaware = ggCcf(DT_Delaware$new_deaths,DT_Delaware$residential,lag.max=35) + 
  ggtitle("Delaware") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_DC = subset(scaled_DT_movavg, administrative_area_level_2 == "District of Columbia")
ccf_DC = ggCcf(DT_DC$new_deaths, DT_DC$residential,lag.max=35) +
  ggtitle("District of Columbia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Florida = subset(scaled_DT_movavg, administrative_area_level_2 == "Florida")
ccf_Florida = ggCcf(DT_Florida$new_deaths, DT_Florida$residential,lag.max=35) +
  ggtitle("Florida") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Georgia = subset(scaled_DT_movavg, administrative_area_level_2 == "Georgia")
ccf_Georgia = ggCcf(DT_Georgia$new_deaths, DT_Georgia$residential,lag.max=35) +
  ggtitle("Georgia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Hawaii = subset(scaled_DT_movavg, administrative_area_level_2 == "Hawaii")
ccf_Hawaii = ggCcf(DT_Hawaii$new_deaths, DT_Hawaii$residential,lag.max=35) +
  ggtitle("Hawaii") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Idaho = subset(scaled_DT_movavg, administrative_area_level_2 == "Idaho")
ccf_Idaho = ggCcf(DT_Idaho$new_deaths, DT_Idaho$residential,lag.max=35) +
  ggtitle("Idaho") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Illinois = subset(scaled_DT_movavg, administrative_area_level_2 == "Illinois")
ccf_Illinois = ggCcf(DT_Illinois$new_deaths, DT_Illinois$residential,lag.max=35) +
  ggtitle("Illinois") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Indiana = subset(scaled_DT_movavg, administrative_area_level_2 == "Indiana")
ccf_Indiana = ggCcf(DT_Indiana$new_deaths, DT_Indiana$residential,lag.max=35) +
  ggtitle("Indiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Iowa = subset(scaled_DT_movavg, administrative_area_level_2 == "Iowa")
ccf_Iowa = ggCcf(DT_Iowa$new_deaths, DT_Iowa$residential,lag.max=35) +
  ggtitle("Iowa") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kansas = subset(scaled_DT_movavg, administrative_area_level_2 == "Kansas")
ccf_Kansas = ggCcf(DT_Kansas$new_deaths, DT_Kansas$residential,lag.max=35) +
  ggtitle("Kansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kentucky = subset(scaled_DT_movavg, administrative_area_level_2 == "Kentucky")
ccf_Kentucky = ggCcf(DT_Kentucky$new_deaths, DT_Kentucky$residential,lag.max=35) +
  ggtitle("Kentucky") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Louisiana = subset(scaled_DT_movavg, administrative_area_level_2 == "Louisiana")
ccf_Louisiana = ggCcf(DT_Louisiana$new_deaths, DT_Louisiana$residential,lag.max=35) +
  ggtitle("Louisiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maine = subset(scaled_DT_movavg, administrative_area_level_2 == "Maine")
ccf_Maine = ggCcf(DT_Maine$new_deaths, DT_Maine$residential,lag.max=35) +
  ggtitle("Maine") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maryland = subset(scaled_DT_movavg, administrative_area_level_2 == "Maryland")
ccf_Maryland = ggCcf(DT_Maryland$new_deaths, DT_Maryland$residential,lag.max=35) +
  ggtitle("Maryland") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Massachusetts = subset(scaled_DT_movavg, administrative_area_level_2 == "Massachusetts")
ccf_Massachusetts = ggCcf(DT_Massachusetts$new_deaths, DT_Massachusetts$residential,lag.max=35) +
  ggtitle("Massachusetts") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Michigan = subset(scaled_DT_movavg, administrative_area_level_2 == "Michigan")
ccf_Michigan = ggCcf(DT_Michigan$new_deaths, DT_Michigan$residential,lag.max=35) +
  ggtitle("Michigan") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Minnesota = subset(scaled_DT_movavg, administrative_area_level_2 == "Minnesota")
ccf_Minnesota = ggCcf(DT_Minnesota$new_deaths, DT_Minnesota$residential,lag.max=35) +
  ggtitle("Minnesota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Mississippi = subset(scaled_DT_movavg, administrative_area_level_2 == "Mississippi")
ccf_Mississippi = ggCcf(DT_Mississippi$new_deaths, DT_Mississippi$residential,lag.max=35) +
  ggtitle("Mississippi") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Missouri = subset(scaled_DT_movavg, administrative_area_level_2 == "Missouri")
ccf_Missouri = ggCcf(DT_Missouri$new_deaths, DT_Missouri$residential,lag.max=35) +
  ggtitle("Missouri") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Montana = subset(scaled_DT_movavg, administrative_area_level_2 == "Montana")
ccf_Montana = ggCcf(DT_Montana$new_deaths, DT_Montana$residential,lag.max=35) +
  ggtitle("Montana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nebraska = subset(scaled_DT_movavg, administrative_area_level_2 == "Nebraska")
ccf_Nebraska = ggCcf(DT_Nebraska$new_deaths, DT_Nebraska$residential,lag.max=35) +
  ggtitle("Nebraska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nevada = subset(scaled_DT_movavg, administrative_area_level_2 == "Nevada")
ccf_Nevada = ggCcf(DT_Nevada$new_deaths, DT_Nevada$residential,lag.max=35) +
  ggtitle("Nevada") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NH = subset(scaled_DT_movavg, administrative_area_level_2 == "New Hampshire")
ccf_NH = ggCcf(DT_NH$new_deaths, DT_NH$residential,lag.max=35) +
  ggtitle("New Hampshire") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NJ = subset(scaled_DT_movavg, administrative_area_level_2 == "New Jersey")
ccf_NJ = ggCcf(DT_NJ$new_deaths, DT_NJ$residential,lag.max=35) +
  ggtitle("New Jersey") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NM = subset(scaled_DT_movavg, administrative_area_level_2 == "New Mexico")
ccf_NM = ggCcf(DT_NM$new_deaths, DT_NM$residential,lag.max=35) +
  ggtitle("New Mexico") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NY = subset(scaled_DT_movavg, administrative_area_level_2 == "New York")
ccf_NY = ggCcf(DT_NY$new_deaths, DT_NY$movavg_residential,lag.max=35) +
  ggtitle("New York") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NC = subset(scaled_DT_movavg, administrative_area_level_2 == "North Carolina")
ccf_NC = ggCcf(DT_NC$new_deaths, DT_NC$residential,lag.max=35) +
  ggtitle("North Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_ND = subset(scaled_DT_movavg, administrative_area_level_2 == "North Dakota")
ccf_ND = ggCcf(DT_ND$new_deaths, DT_ND$residential,lag.max=35) +
  ggtitle("North Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Ohio = subset(scaled_DT_movavg, administrative_area_level_2 == "Ohio")
ccf_Ohio = ggCcf(DT_Ohio$new_deaths, DT_Ohio$residential,lag.max=35) +
  ggtitle("Ohio") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oklahoma = subset(scaled_DT_movavg, administrative_area_level_2 == "Oklahoma")
ccf_Oklahoma = ggCcf(DT_Oklahoma$new_deaths, DT_Oklahoma$residential,lag.max=35) +
  ggtitle("Oklahoma") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oregon = subset(scaled_DT_movavg, administrative_area_level_2 == "Oregon")
ccf_Oregon = ggCcf(DT_Oregon$new_deaths, DT_Oregon$residential,lag.max=35) +
  ggtitle("Oregon") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Pennsylvania = subset(scaled_DT_movavg, administrative_area_level_2 == "Pennsylvania")
ccf_Pennsylvania = ggCcf(DT_Pennsylvania$new_deaths, DT_Pennsylvania$residential,lag.max=35) +
  ggtitle("Pennsylvania") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_RI = subset(scaled_DT_movavg, administrative_area_level_2 == "Rhode Island")
ccf_RI = ggCcf(DT_RI$new_deaths, DT_RI$residential,lag.max=35) +
  ggtitle("Rhode Island") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SC = subset(scaled_DT_movavg, administrative_area_level_2 == "South Carolina")
ccf_SC = ggCcf(DT_SC$new_deaths, DT_SC$residential,lag.max=35) +
  ggtitle("South Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SD = subset(scaled_DT_movavg, administrative_area_level_2 == "South Dakota")
ccf_SD = ggCcf(DT_SD$new_deaths, DT_SD$residential,lag.max=35) +
  ggtitle("South Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Texas = subset(scaled_DT_movavg, administrative_area_level_2 == "Texas")
ccf_Texas = ggCcf(DT_Texas$new_deaths, DT_Texas$residential,lag.max=35) +
  ggtitle("Texas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Tennessee = subset(scaled_DT_movavg, administrative_area_level_2 == "Tennessee")
ccf_Tennessee = ggCcf(DT_Tennessee$new_deaths, DT_Tennessee$residential,lag.max=35) +
  ggtitle("Tennessee") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Utah = subset(scaled_DT_movavg, administrative_area_level_2 == "Utah")
ccf_Utah = ggCcf(DT_Utah$new_deaths, DT_Utah$residential,lag.max=35) +
  ggtitle("Utah") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Vermont = subset(scaled_DT_movavg, administrative_area_level_2 == "Vermont")
ccf_Vermont = ggCcf(DT_Vermont$new_deaths, DT_Vermont$residential,lag.max=35) +
  ggtitle("Vermont") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Virginia = subset(scaled_DT_movavg, administrative_area_level_2 == "Virginia")
ccf_Virginia = ggCcf(DT_Virginia$new_deaths, DT_Virginia$residential,lag.max=35) +
  ggtitle("Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Washington = subset(scaled_DT_movavg, administrative_area_level_2 == "Washington")
ccf_Washington = ggCcf(DT_Washington$new_deaths, DT_Washington$residential,lag.max=35) +
  ggtitle("Washington") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_WV = subset(scaled_DT_movavg, administrative_area_level_2 == "West Virginia")
ccf_WV = ggCcf(DT_WV$new_deaths, DT_WV$residential,lag.max=35) +
  ggtitle("West Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wisconsin = subset(scaled_DT_movavg, administrative_area_level_2 == "Wisconsin")
ccf_Wisconsin = ggCcf(DT_Wisconsin$new_deaths, DT_Wisconsin$residential,lag.max=35) +
  ggtitle("Wisconsin") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wyoming = subset(scaled_DT_movavg, administrative_area_level_2 == "Wyoming")
ccf_Wyoming = ggCcf(DT_Wyoming$new_deaths, DT_Wyoming$residential,lag.max=35) +
  ggtitle("Wyoming") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

################################################################################
###Make a plot!


#Make a page
grid.newpage()
pushViewport(viewport(layout=grid.layout(9,8))) #set up layout
grid.text("CCF Plot for Every State - Residential", vp = viewport(layout.pos.row = 1, layout.pos.col = 5))
grid.text("Lags", y = unit(1, "lines"))
grid.text("ACF", x = unit(1, "lines"), rot = 90)
print(ccf_Alabama, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(ccf_Alaska, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(ccf_Arizona, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))
print(ccf_Arkansas, vp=viewport(layout.pos.row = 2, layout.pos.col = 5))
print(ccf_California, vp=viewport(layout.pos.row = 2, layout.pos.col = 6))
print(ccf_Colorado, vp=viewport(layout.pos.row = 2, layout.pos.col = 7))
print(ccf_Connecticut, vp=viewport(layout.pos.row = 2, layout.pos.col = 8))

print(ccf_Delaware, vp=viewport(layout.pos.row = 3, layout.pos.col = 2))
print(ccf_DC, vp=viewport(layout.pos.row = 3, layout.pos.col = 3))
print(ccf_Florida, vp=viewport(layout.pos.row = 3, layout.pos.col = 4))
print(ccf_Georgia, vp=viewport(layout.pos.row = 3, layout.pos.col = 5))
print(ccf_Hawaii, vp=viewport(layout.pos.row = 3, layout.pos.col = 6))
print(ccf_Idaho, vp=viewport(layout.pos.row = 3, layout.pos.col = 7))
print(ccf_Illinois, vp=viewport(layout.pos.row = 3, layout.pos.col = 8))

print(ccf_Indiana, vp=viewport(layout.pos.row = 4, layout.pos.col = 2))
print(ccf_Iowa, vp=viewport(layout.pos.row = 4, layout.pos.col = 3))
print(ccf_Kansas, vp=viewport(layout.pos.row = 4, layout.pos.col = 4))
print(ccf_Kentucky, vp=viewport(layout.pos.row = 4, layout.pos.col = 5))
print(ccf_Louisiana, vp=viewport(layout.pos.row = 4, layout.pos.col = 6))
print(ccf_Maine, vp=viewport(layout.pos.row = 4, layout.pos.col = 7))
print(ccf_Maryland, vp=viewport(layout.pos.row = 4, layout.pos.col = 8))

print(ccf_Massachusetts, vp=viewport(layout.pos.row = 5, layout.pos.col = 2))
print(ccf_Michigan, vp=viewport(layout.pos.row = 5, layout.pos.col = 3))
print(ccf_Minnesota, vp=viewport(layout.pos.row = 5, layout.pos.col = 4))
print(ccf_Mississippi, vp=viewport(layout.pos.row = 5, layout.pos.col = 5))
print(ccf_Missouri, vp=viewport(layout.pos.row = 5, layout.pos.col = 6))
print(ccf_Montana, vp=viewport(layout.pos.row = 5, layout.pos.col = 7))
print(ccf_Nebraska, vp=viewport(layout.pos.row = 5, layout.pos.col = 8))

print(ccf_Nevada, vp=viewport(layout.pos.row = 6, layout.pos.col = 2))
print(ccf_NH, vp=viewport(layout.pos.row = 6, layout.pos.col = 3))
print(ccf_NJ, vp=viewport(layout.pos.row = 6, layout.pos.col = 4))
print(ccf_NM, vp=viewport(layout.pos.row = 6, layout.pos.col = 5))
print(ccf_NY, vp=viewport(layout.pos.row = 6, layout.pos.col = 6))
print(ccf_NC, vp=viewport(layout.pos.row = 6, layout.pos.col = 7))
print(ccf_ND, vp=viewport(layout.pos.row = 6, layout.pos.col = 8))

print(ccf_Ohio, vp=viewport(layout.pos.row = 7, layout.pos.col = 2))
print(ccf_Oklahoma, vp=viewport(layout.pos.row = 7, layout.pos.col = 3))
print(ccf_Oregon, vp=viewport(layout.pos.row = 7, layout.pos.col = 4))
print(ccf_Pennsylvania, vp=viewport(layout.pos.row = 7, layout.pos.col = 5))
print(ccf_RI, vp=viewport(layout.pos.row = 7, layout.pos.col = 6))
print(ccf_SC, vp=viewport(layout.pos.row = 7, layout.pos.col = 7))
print(ccf_SD, vp=viewport(layout.pos.row = 7, layout.pos.col = 8))

print(ccf_Texas, vp=viewport(layout.pos.row = 8, layout.pos.col = 2))
print(ccf_Tennessee, vp=viewport(layout.pos.row = 8, layout.pos.col = 3))
print(ccf_Utah, vp=viewport(layout.pos.row = 8, layout.pos.col = 4))
print(ccf_Vermont, vp=viewport(layout.pos.row = 8, layout.pos.col = 5))
print(ccf_Virginia, vp=viewport(layout.pos.row = 8, layout.pos.col = 6))
print(ccf_Washington, vp=viewport(layout.pos.row = 8, layout.pos.col = 7))
print(ccf_WV, vp=viewport(layout.pos.row = 8, layout.pos.col = 8))

print(ccf_Wisconsin, vp=viewport(layout.pos.row = 9, layout.pos.col = 2))
print(ccf_Wyoming, vp=viewport(layout.pos.row = 9, layout.pos.col = 3))



