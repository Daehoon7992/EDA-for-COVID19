library(tidyverse)
library(COVID19)
library(data.table)
library(reshape2)
library(ggplot2)
library(lubridate)
library(grid)
library(tidyquant)
library(forecast)
library(timetk)

amr <- "applemobilitytrends-2020-12-23.csv" 
x <- covid19(country="United States", level=2, amr = amr)

#data prep
x <- x %>% 
  filter(!(administrative_area_level_2 %in% c("Northern Mariana Islands", 
                                              "Guam", 
                                              "Puerto Rico", 
                                              "Virgin Islands", 
                                              "American Samoa")))

x <- x %>%
  select(date,confirmed,deaths,administrative_area_level_2,
         driving,walking,transit)

x <- x %>% 
  mutate(new_confirmed = confirmed - lag(confirmed), 
         new_deaths = deaths - lag(deaths))

x <- x %>%
  mutate(new_deaths = ifelse(new_deaths < 0,NA,new_deaths), #for negative number of deaths
         new_deaths = ifelse(new_deaths > 900,NA,new_deaths))

#plot of driving data
ggplot(data=x,aes(x=date,y=driving)) +
  geom_line() +
  facet_wrap(~administrative_area_level_2) +
  ggtitle("Driving")

#plot of transit data
ggplot(data=x,aes(x=date,y=transit)) +
  geom_line() +
  facet_wrap(~administrative_area_level_2) +
  ggtitle("Transit")

#plot of walking data
ggplot(data=x,aes(x=date,y=walking)) +
  geom_line() +
  facet_wrap(~administrative_area_level_2) +
  labs(title="Walking")

#all 3 combined
ggplot(data=x,aes(x=date)) +
  geom_line(aes(y=driving,colour="driving")) +
  geom_line(aes(y=transit,colour="transit")) +
  geom_line(aes(y=walking,colour="walking")) +
  facet_wrap(~administrative_area_level_2) +
  labs(title="Driving, Transit and Walking")


#all 3 combined + new deaths
x %>%
  mutate(driving=scale(driving),
         walking=scale(walking),
         transit=scale(transit),
         new_deaths=scale(new_deaths)) %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=driving,colour="driving")) +
  geom_line(aes(y=walking,colour="walking")) +
  geom_line(aes(y=transit,colour="transit")) +
  geom_line(aes(y=new_deaths,colour="new_deaths")) +
  facet_wrap(~administrative_area_level_2) +
  labs(title="combined data")

#only new deaths
x %>% 
  ggplot(aes(x=date, y=new_deaths)) +
  geom_line() +
  facet_wrap(~administrative_area_level_2) +
  theme_bw()+
  theme(axis.title.x=element_blank())



### Function for scale - We need to scale the dataset
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

scaled_DT <-
  x %>% 
  group_by(administrative_area_level_2) %>%
  mutate(new_deaths = scale_this(new_deaths),
         driving = scale_this(driving),
         walking = scale_this(walking),
         transit = scale_this(transit))

#scaled_DT <- na.omit(scaled_DT)

#prep
states <- unique(c(x$administrative_area_level_2))
states <- sort(states)


###Let us make a ccf plot for every state - for driving, walking, transit

#ccf for driving
grid.newpage()
pushViewport(viewport(layout=grid.layout(9,8))) #set up layout
grid.text("CCF Plot for Every State - driving", vp = viewport(layout.pos.row = 1, layout.pos.col = 5))
grid.text("Lags", y = unit(1, "lines"))
grid.text("ACF", x = unit(1, "lines"), rot = 90)

DT_Alabama = subset(scaled_x_DT, administrative_area_level_2 == "Alabama")
ccf_Alabama = ggCcf(DT_Alabama$new_deaths,DT_Alabama$driving,lag.max=35) + 
  ggtitle("Alabama") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Alaska = subset(scaled_x_DT, administrative_area_level_2 == "Alaska")
ccf_Alaska = ggCcf(DT_Alaska$new_deaths, DT_Alaska$driving,lag.max=35) +
  ggtitle("Alaska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arizona = subset(scaled_x_DT, administrative_area_level_2 == "Arizona")
ccf_Arizona = ggCcf(DT_Arizona$new_deaths, DT_Arizona$driving,lag.max=35) +
  ggtitle("Arizona") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arkansas = subset(scaled_x_DT, administrative_area_level_2 == "Arkansas")
ccf_Arkansas = ggCcf(DT_Arkansas$new_deaths, DT_Arkansas$driving,lag.max=35) +
  ggtitle("Arkansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_California = subset(scaled_x_DT, administrative_area_level_2 == "California")
ccf_California = ggCcf(DT_California$new_deaths, DT_California$driving,lag.max=35) +
  ggtitle("California") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Colorado = subset(scaled_x_DT, administrative_area_level_2 == "Colorado")
ccf_Colorado = ggCcf(DT_Colorado$new_deaths, DT_Colorado$driving,lag.max=35) +
  ggtitle("Colorado") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Connecticut = subset(scaled_x_DT, administrative_area_level_2 == "Connecticut")
ccf_Connecticut = ggCcf(DT_Connecticut$new_deaths, DT_Connecticut$driving,lag.max=35) +
  ggtitle("Connecticut") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Delaware = subset(scaled_x_DT, administrative_area_level_2 == "Delaware")
ccf_Delaware = ggCcf(DT_Delaware$new_deaths,DT_Delaware$driving,lag.max=35) + 
  ggtitle("Delaware") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_DC = subset(scaled_x_DT, administrative_area_level_2 == "District of Columbia")
ccf_DC = ggCcf(DT_DC$new_deaths, DT_DC$driving,lag.max=35) +
  ggtitle("District of Columbia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Florida = subset(scaled_x_DT, administrative_area_level_2 == "Florida")
ccf_Florida = ggCcf(DT_Florida$new_deaths, DT_Florida$driving,lag.max=35) +
  ggtitle("Florida") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Georgia = subset(scaled_x_DT, administrative_area_level_2 == "Georgia")
ccf_Georgia = ggCcf(DT_Georgia$new_deaths, DT_Georgia$driving,lag.max=35) +
  ggtitle("Georgia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Hawaii = subset(scaled_x_DT, administrative_area_level_2 == "Hawaii")
ccf_Hawaii = ggCcf(DT_Hawaii$new_deaths, DT_Hawaii$driving,lag.max=35) +
  ggtitle("Hawaii") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Idaho = subset(scaled_x_DT, administrative_area_level_2 == "Idaho")
ccf_Idaho = ggCcf(DT_Idaho$new_deaths, DT_Idaho$driving,lag.max=35) +
  ggtitle("Idaho") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Illinois = subset(scaled_x_DT, administrative_area_level_2 == "Illinois")
ccf_Illinois = ggCcf(DT_Illinois$new_deaths, DT_Illinois$driving,lag.max=35) +
  ggtitle("Illinois") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Indiana = subset(scaled_x_DT, administrative_area_level_2 == "Indiana")
ccf_Indiana = ggCcf(DT_Indiana$new_deaths, DT_Indiana$driving,lag.max=35) +
  ggtitle("Indiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Iowa = subset(scaled_x_DT, administrative_area_level_2 == "Iowa")
ccf_Iowa = ggCcf(DT_Iowa$new_deaths, DT_Iowa$driving,lag.max=35) +
  ggtitle("Iowa") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kansas = subset(scaled_x_DT, administrative_area_level_2 == "Kansas")
ccf_Kansas = ggCcf(DT_Kansas$new_deaths, DT_Kansas$driving,lag.max=35) +
  ggtitle("Kansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kentucky = subset(scaled_x_DT, administrative_area_level_2 == "Kentucky")
ccf_Kentucky = ggCcf(DT_Kentucky$new_deaths, DT_Kentucky$driving,lag.max=35) +
  ggtitle("Kentucky") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Louisiana = subset(scaled_x_DT, administrative_area_level_2 == "Louisiana")
ccf_Louisiana = ggCcf(DT_Louisiana$new_deaths, DT_Louisiana$driving,lag.max=35) +
  ggtitle("Louisiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maine = subset(scaled_x_DT, administrative_area_level_2 == "Maine")
ccf_Maine = ggCcf(DT_Maine$new_deaths, DT_Maine$driving,lag.max=35) +
  ggtitle("Maine") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maryland = subset(scaled_x_DT, administrative_area_level_2 == "Maryland")
ccf_Maryland = ggCcf(DT_Maryland$new_deaths, DT_Maryland$driving,lag.max=35) +
  ggtitle("Maryland") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Massachusetts = subset(scaled_x_DT, administrative_area_level_2 == "Massachusetts")
ccf_Massachusetts = ggCcf(DT_Massachusetts$new_deaths, DT_Massachusetts$driving,lag.max=35) +
  ggtitle("Massachusetts") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Michigan = subset(scaled_x_DT, administrative_area_level_2 == "Michigan")
ccf_Michigan = ggCcf(DT_Michigan$new_deaths, DT_Michigan$driving,lag.max=35) +
  ggtitle("Michigan") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Minnesota = subset(scaled_x_DT, administrative_area_level_2 == "Minnesota")
ccf_Minnesota = ggCcf(DT_Minnesota$new_deaths, DT_Minnesota$driving,lag.max=35) +
  ggtitle("Minnesota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Mississippi = subset(scaled_x_DT, administrative_area_level_2 == "Mississippi")
ccf_Mississippi = ggCcf(DT_Mississippi$new_deaths, DT_Mississippi$driving,lag.max=35) +
  ggtitle("Mississippi") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Missouri = subset(scaled_x_DT, administrative_area_level_2 == "Missouri")
ccf_Missouri = ggCcf(DT_Missouri$new_deaths, DT_Missouri$driving,lag.max=35) +
  ggtitle("Missouri") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Montana = subset(scaled_x_DT, administrative_area_level_2 == "Montana")
ccf_Montana = ggCcf(DT_Montana$new_deaths, DT_Montana$driving,lag.max=35) +
  ggtitle("Montana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nebraska = subset(scaled_x_DT, administrative_area_level_2 == "Nebraska")
ccf_Nebraska = ggCcf(DT_Nebraska$new_deaths, DT_Nebraska$driving,lag.max=35) +
  ggtitle("Nebraska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nevada = subset(scaled_x_DT, administrative_area_level_2 == "Nevada")
ccf_Nevada = ggCcf(DT_Nevada$new_deaths, DT_Nevada$driving,lag.max=35) +
  ggtitle("Nevada") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NH = subset(scaled_x_DT, administrative_area_level_2 == "New Hampshire")
ccf_NH = ggCcf(DT_NH$new_deaths, DT_NH$driving,lag.max=35) +
  ggtitle("New Hampshire") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NJ = subset(scaled_x_DT, administrative_area_level_2 == "New Jersey")
ccf_NJ = ggCcf(DT_NJ$new_deaths, DT_NJ$driving,lag.max=35) +
  ggtitle("New Jersey") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NM = subset(scaled_x_DT, administrative_area_level_2 == "New Mexico")
ccf_NM = ggCcf(DT_NM$new_deaths, DT_NM$driving,lag.max=35) +
  ggtitle("New Mexico") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NY = subset(scaled_x_DT, administrative_area_level_2 == "New York")
ccf_NY = ggCcf(DT_NY$new_deaths, DT_NY$driving,lag.max=35) +
  ggtitle("New York") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NC = subset(scaled_x_DT, administrative_area_level_2 == "North Carolina")
ccf_NC = ggCcf(DT_NC$new_deaths, DT_NC$driving,lag.max=35) +
  ggtitle("North Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_ND = subset(scaled_x_DT, administrative_area_level_2 == "North Dakota")
ccf_ND = ggCcf(DT_ND$new_deaths, DT_ND$driving,lag.max=35) +
  ggtitle("North Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Ohio = subset(scaled_x_DT, administrative_area_level_2 == "Ohio")
ccf_Ohio = ggCcf(DT_Ohio$new_deaths, DT_Ohio$driving,lag.max=35) +
  ggtitle("Ohio") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oklahoma = subset(scaled_x_DT, administrative_area_level_2 == "Oklahoma")
ccf_Oklahoma = ggCcf(DT_Oklahoma$new_deaths, DT_Oklahoma$driving,lag.max=35) +
  ggtitle("Oklahoma") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oregon = subset(scaled_x_DT, administrative_area_level_2 == "Oregon")
ccf_Oregon = ggCcf(DT_Oregon$new_deaths, DT_Oregon$driving,lag.max=35) +
  ggtitle("Oregon") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Pennsylvania = subset(scaled_x_DT, administrative_area_level_2 == "Pennsylvania")
ccf_Pennsylvania = ggCcf(DT_Pennsylvania$new_deaths, DT_Pennsylvania$driving,lag.max=35) +
  ggtitle("Pennsylvania") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_RI = subset(scaled_x_DT, administrative_area_level_2 == "Rhode Island")
ccf_RI = ggCcf(DT_RI$new_deaths, DT_RI$driving,lag.max=35) +
  ggtitle("Rhode Island") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SC = subset(scaled_x_DT, administrative_area_level_2 == "South Carolina")
ccf_SC = ggCcf(DT_SC$new_deaths, DT_SC$driving,lag.max=35) +
  ggtitle("South Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SD = subset(scaled_x_DT, administrative_area_level_2 == "South Dakota")
ccf_SD = ggCcf(DT_SD$new_deaths, DT_SD$driving,lag.max=35) +
  ggtitle("South Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Texas = subset(scaled_x_DT, administrative_area_level_2 == "Texas")
ccf_Texas = ggCcf(DT_Texas$new_deaths, DT_Texas$driving,lag.max=35) +
  ggtitle("Texas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Tennessee = subset(scaled_x_DT, administrative_area_level_2 == "Tennessee")
ccf_Tennessee = ggCcf(DT_Tennessee$new_deaths, DT_Tennessee$driving,lag.max=35) +
  ggtitle("Tennessee") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Utah = subset(scaled_x_DT, administrative_area_level_2 == "Utah")
ccf_Utah = ggCcf(DT_Utah$new_deaths, DT_Utah$driving,lag.max=35) +
  ggtitle("Utah") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Vermont = subset(scaled_x_DT, administrative_area_level_2 == "Vermont")
ccf_Vermont = ggCcf(DT_Vermont$new_deaths, DT_Vermont$driving,lag.max=35) +
  ggtitle("Vermont") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Virginia = subset(scaled_x_DT, administrative_area_level_2 == "Virginia")
ccf_Virginia = ggCcf(DT_Virginia$new_deaths, DT_Virginia$driving,lag.max=35) +
  ggtitle("Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Washington = subset(scaled_x_DT, administrative_area_level_2 == "Washington")
ccf_Washington = ggCcf(DT_Washington$new_deaths, DT_Washington$driving,lag.max=35) +
  ggtitle("Washington") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_WV = subset(scaled_x_DT, administrative_area_level_2 == "West Virginia")
ccf_WV = ggCcf(DT_WV$new_deaths, DT_WV$driving,lag.max=35) +
  ggtitle("West Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wisconsin = subset(scaled_x_DT, administrative_area_level_2 == "Wisconsin")
ccf_Wisconsin = ggCcf(DT_Wisconsin$new_deaths, DT_Wisconsin$driving,lag.max=35) +
  ggtitle("Wisconsin") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wyoming = subset(scaled_x_DT, administrative_area_level_2 == "Wyoming")
ccf_Wyoming = ggCcf(DT_Wyoming$new_deaths, DT_Wyoming$driving,lag.max=35) +
  ggtitle("Wyoming") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())


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



#ccf for walking
grid.newpage()
pushViewport(viewport(layout=grid.layout(9,8))) #set up layout
grid.text("CCF Plot for Every State - walking", vp = viewport(layout.pos.row = 1, layout.pos.col = 5))
grid.text("Lags", y = unit(1, "lines"))
grid.text("ACF", x = unit(1, "lines"), rot = 90)

DT_Alabama = subset(scaled_x_DT, administrative_area_level_2 == "Alabama")
ccf_Alabama = ggCcf(DT_Alabama$new_deaths,DT_Alabama$walking,lag.max=35) + 
  ggtitle("Alabama") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Alaska = subset(scaled_x_DT, administrative_area_level_2 == "Alaska")
ccf_Alaska = ggCcf(DT_Alaska$new_deaths, DT_Alaska$walking,lag.max=35) +
  ggtitle("Alaska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arizona = subset(scaled_x_DT, administrative_area_level_2 == "Arizona")
ccf_Arizona = ggCcf(DT_Arizona$new_deaths, DT_Arizona$walking,lag.max=35) +
  ggtitle("Arizona") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arkansas = subset(scaled_x_DT, administrative_area_level_2 == "Arkansas")
ccf_Arkansas = ggCcf(DT_Arkansas$new_deaths, DT_Arkansas$walking,lag.max=35) +
  ggtitle("Arkansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_California = subset(scaled_x_DT, administrative_area_level_2 == "California")
ccf_California = ggCcf(DT_California$new_deaths, DT_California$walking,lag.max=35) +
  ggtitle("California") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Colorado = subset(scaled_x_DT, administrative_area_level_2 == "Colorado")
ccf_Colorado = ggCcf(DT_Colorado$new_deaths, DT_Colorado$walking,lag.max=35) +
  ggtitle("Colorado") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Connecticut = subset(scaled_x_DT, administrative_area_level_2 == "Connecticut")
ccf_Connecticut = ggCcf(DT_Connecticut$new_deaths, DT_Connecticut$walking,lag.max=35) +
  ggtitle("Connecticut") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Delaware = subset(scaled_x_DT, administrative_area_level_2 == "Delaware")
ccf_Delaware = ggCcf(DT_Delaware$new_deaths,DT_Delaware$walking,lag.max=35) + 
  ggtitle("Delaware") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_DC = subset(scaled_x_DT, administrative_area_level_2 == "District of Columbia")
ccf_DC = ggCcf(DT_DC$new_deaths, DT_DC$walking,lag.max=35) +
  ggtitle("District of Columbia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Florida = subset(scaled_x_DT, administrative_area_level_2 == "Florida")
ccf_Florida = ggCcf(DT_Florida$new_deaths, DT_Florida$walking,lag.max=35) +
  ggtitle("Florida") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Georgia = subset(scaled_x_DT, administrative_area_level_2 == "Georgia")
ccf_Georgia = ggCcf(DT_Georgia$new_deaths, DT_Georgia$walking,lag.max=35) +
  ggtitle("Georgia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Hawaii = subset(scaled_x_DT, administrative_area_level_2 == "Hawaii")
ccf_Hawaii = ggCcf(DT_Hawaii$new_deaths, DT_Hawaii$walking,lag.max=35) +
  ggtitle("Hawaii") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Idaho = subset(scaled_x_DT, administrative_area_level_2 == "Idaho")
ccf_Idaho = ggCcf(DT_Idaho$new_deaths, DT_Idaho$walking,lag.max=35) +
  ggtitle("Idaho") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Illinois = subset(scaled_x_DT, administrative_area_level_2 == "Illinois")
ccf_Illinois = ggCcf(DT_Illinois$new_deaths, DT_Illinois$walking,lag.max=35) +
  ggtitle("Illinois") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Indiana = subset(scaled_x_DT, administrative_area_level_2 == "Indiana")
ccf_Indiana = ggCcf(DT_Indiana$new_deaths, DT_Indiana$walking,lag.max=35) +
  ggtitle("Indiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Iowa = subset(scaled_x_DT, administrative_area_level_2 == "Iowa")
ccf_Iowa = ggCcf(DT_Iowa$new_deaths, DT_Iowa$walking,lag.max=35) +
  ggtitle("Iowa") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kansas = subset(scaled_x_DT, administrative_area_level_2 == "Kansas")
ccf_Kansas = ggCcf(DT_Kansas$new_deaths, DT_Kansas$walking,lag.max=35) +
  ggtitle("Kansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kentucky = subset(scaled_x_DT, administrative_area_level_2 == "Kentucky")
ccf_Kentucky = ggCcf(DT_Kentucky$new_deaths, DT_Kentucky$walking,lag.max=35) +
  ggtitle("Kentucky") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Louisiana = subset(scaled_x_DT, administrative_area_level_2 == "Louisiana")
ccf_Louisiana = ggCcf(DT_Louisiana$new_deaths, DT_Louisiana$walking,lag.max=35) +
  ggtitle("Louisiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maine = subset(scaled_x_DT, administrative_area_level_2 == "Maine")
ccf_Maine = ggCcf(DT_Maine$new_deaths, DT_Maine$walking,lag.max=35) +
  ggtitle("Maine") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maryland = subset(scaled_x_DT, administrative_area_level_2 == "Maryland")
ccf_Maryland = ggCcf(DT_Maryland$new_deaths, DT_Maryland$walking,lag.max=35) +
  ggtitle("Maryland") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Massachusetts = subset(scaled_x_DT, administrative_area_level_2 == "Massachusetts")
ccf_Massachusetts = ggCcf(DT_Massachusetts$new_deaths, DT_Massachusetts$walking,lag.max=35) +
  ggtitle("Massachusetts") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Michigan = subset(scaled_x_DT, administrative_area_level_2 == "Michigan")
ccf_Michigan = ggCcf(DT_Michigan$new_deaths, DT_Michigan$walking,lag.max=35) +
  ggtitle("Michigan") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Minnesota = subset(scaled_x_DT, administrative_area_level_2 == "Minnesota")
ccf_Minnesota = ggCcf(DT_Minnesota$new_deaths, DT_Minnesota$walking,lag.max=35) +
  ggtitle("Minnesota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Mississippi = subset(scaled_x_DT, administrative_area_level_2 == "Mississippi")
ccf_Mississippi = ggCcf(DT_Mississippi$new_deaths, DT_Mississippi$walking,lag.max=35) +
  ggtitle("Mississippi") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Missouri = subset(scaled_x_DT, administrative_area_level_2 == "Missouri")
ccf_Missouri = ggCcf(DT_Missouri$new_deaths, DT_Missouri$walking,lag.max=35) +
  ggtitle("Missouri") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Montana = subset(scaled_x_DT, administrative_area_level_2 == "Montana")
ccf_Montana = ggCcf(DT_Montana$new_deaths, DT_Montana$walking,lag.max=35) +
  ggtitle("Montana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nebraska = subset(scaled_x_DT, administrative_area_level_2 == "Nebraska")
ccf_Nebraska = ggCcf(DT_Nebraska$new_deaths, DT_Nebraska$walking,lag.max=35) +
  ggtitle("Nebraska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nevada = subset(scaled_x_DT, administrative_area_level_2 == "Nevada")
ccf_Nevada = ggCcf(DT_Nevada$new_deaths, DT_Nevada$walking,lag.max=35) +
  ggtitle("Nevada") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NH = subset(scaled_x_DT, administrative_area_level_2 == "New Hampshire")
ccf_NH = ggCcf(DT_NH$new_deaths, DT_NH$walking,lag.max=35) +
  ggtitle("New Hampshire") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NJ = subset(scaled_x_DT, administrative_area_level_2 == "New Jersey")
ccf_NJ = ggCcf(DT_NJ$new_deaths, DT_NJ$walking,lag.max=35) +
  ggtitle("New Jersey") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NM = subset(scaled_x_DT, administrative_area_level_2 == "New Mexico")
ccf_NM = ggCcf(DT_NM$new_deaths, DT_NM$walking,lag.max=35) +
  ggtitle("New Mexico") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NY = subset(scaled_x_DT, administrative_area_level_2 == "New York")
ccf_NY = ggCcf(DT_NY$new_deaths, DT_NY$walking,lag.max=35) +
  ggtitle("New York") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NC = subset(scaled_x_DT, administrative_area_level_2 == "North Carolina")
ccf_NC = ggCcf(DT_NC$new_deaths, DT_NC$walking,lag.max=35) +
  ggtitle("North Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_ND = subset(scaled_x_DT, administrative_area_level_2 == "North Dakota")
ccf_ND = ggCcf(DT_ND$new_deaths, DT_ND$walking,lag.max=35) +
  ggtitle("North Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Ohio = subset(scaled_x_DT, administrative_area_level_2 == "Ohio")
ccf_Ohio = ggCcf(DT_Ohio$new_deaths, DT_Ohio$walking,lag.max=35) +
  ggtitle("Ohio") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oklahoma = subset(scaled_x_DT, administrative_area_level_2 == "Oklahoma")
ccf_Oklahoma = ggCcf(DT_Oklahoma$new_deaths, DT_Oklahoma$walking,lag.max=35) +
  ggtitle("Oklahoma") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oregon = subset(scaled_x_DT, administrative_area_level_2 == "Oregon")
ccf_Oregon = ggCcf(DT_Oregon$new_deaths, DT_Oregon$walking,lag.max=35) +
  ggtitle("Oregon") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Pennsylvania = subset(scaled_x_DT, administrative_area_level_2 == "Pennsylvania")
ccf_Pennsylvania = ggCcf(DT_Pennsylvania$new_deaths, DT_Pennsylvania$walking,lag.max=35) +
  ggtitle("Pennsylvania") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_RI = subset(scaled_x_DT, administrative_area_level_2 == "Rhode Island")
ccf_RI = ggCcf(DT_RI$new_deaths, DT_RI$walking,lag.max=35) +
  ggtitle("Rhode Island") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SC = subset(scaled_x_DT, administrative_area_level_2 == "South Carolina")
ccf_SC = ggCcf(DT_SC$new_deaths, DT_SC$walking,lag.max=35) +
  ggtitle("South Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SD = subset(scaled_x_DT, administrative_area_level_2 == "South Dakota")
ccf_SD = ggCcf(DT_SD$new_deaths, DT_SD$walking,lag.max=35) +
  ggtitle("South Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Texas = subset(scaled_x_DT, administrative_area_level_2 == "Texas")
ccf_Texas = ggCcf(DT_Texas$new_deaths, DT_Texas$walking,lag.max=35) +
  ggtitle("Texas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Tennessee = subset(scaled_x_DT, administrative_area_level_2 == "Tennessee")
ccf_Tennessee = ggCcf(DT_Tennessee$new_deaths, DT_Tennessee$walking,lag.max=35) +
  ggtitle("Tennessee") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Utah = subset(scaled_x_DT, administrative_area_level_2 == "Utah")
ccf_Utah = ggCcf(DT_Utah$new_deaths, DT_Utah$walking,lag.max=35) +
  ggtitle("Utah") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Vermont = subset(scaled_x_DT, administrative_area_level_2 == "Vermont")
ccf_Vermont = ggCcf(DT_Vermont$new_deaths, DT_Vermont$walking,lag.max=35) +
  ggtitle("Vermont") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Virginia = subset(scaled_x_DT, administrative_area_level_2 == "Virginia")
ccf_Virginia = ggCcf(DT_Virginia$new_deaths, DT_Virginia$walking,lag.max=35) +
  ggtitle("Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Washington = subset(scaled_x_DT, administrative_area_level_2 == "Washington")
ccf_Washington = ggCcf(DT_Washington$new_deaths, DT_Washington$walking,lag.max=35) +
  ggtitle("Washington") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_WV = subset(scaled_x_DT, administrative_area_level_2 == "West Virginia")
ccf_WV = ggCcf(DT_WV$new_deaths, DT_WV$walking,lag.max=35) +
  ggtitle("West Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wisconsin = subset(scaled_x_DT, administrative_area_level_2 == "Wisconsin")
ccf_Wisconsin = ggCcf(DT_Wisconsin$new_deaths, DT_Wisconsin$walking,lag.max=35) +
  ggtitle("Wisconsin") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wyoming = subset(scaled_x_DT, administrative_area_level_2 == "Wyoming")
ccf_Wyoming = ggCcf(DT_Wyoming$new_deaths, DT_Wyoming$walking,lag.max=35) +
  ggtitle("Wyoming") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())


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



#ccf for transit
grid.newpage()
pushViewport(viewport(layout=grid.layout(9,8))) #set up layout
grid.text("CCF Plot for Every State - transit", vp = viewport(layout.pos.row = 1, layout.pos.col = 5))
grid.text("Lags", y = unit(1, "lines"))
grid.text("ACF", x = unit(1, "lines"), rot = 90)

DT_Alabama = subset(scaled_x_DT, administrative_area_level_2 == "Alabama")
ccf_Alabama = ggCcf(DT_Alabama$new_deaths,DT_Alabama$transit,lag.max=35) + 
  ggtitle("Alabama") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Alaska = subset(scaled_x_DT, administrative_area_level_2 == "Alaska")
ccf_Alaska = ggCcf(DT_Alaska$new_deaths, DT_Alaska$transit,lag.max=35) +
  ggtitle("Alaska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arizona = subset(scaled_x_DT, administrative_area_level_2 == "Arizona")
ccf_Arizona = ggCcf(DT_Arizona$new_deaths, DT_Arizona$transit,lag.max=35) +
  ggtitle("Arizona") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Arkansas = subset(scaled_x_DT, administrative_area_level_2 == "Arkansas")
ccf_Arkansas = ggCcf(DT_Arkansas$new_deaths, DT_Arkansas$transit,lag.max=35) +
  ggtitle("Arkansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_California = subset(scaled_x_DT, administrative_area_level_2 == "California")
ccf_California = ggCcf(DT_California$new_deaths, DT_California$transit,lag.max=35) +
  ggtitle("California") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Colorado = subset(scaled_x_DT, administrative_area_level_2 == "Colorado")
ccf_Colorado = ggCcf(DT_Colorado$new_deaths, DT_Colorado$transit,lag.max=35) +
  ggtitle("Colorado") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Connecticut = subset(scaled_x_DT, administrative_area_level_2 == "Connecticut")
ccf_Connecticut = ggCcf(DT_Connecticut$new_deaths, DT_Connecticut$transit,lag.max=35) +
  ggtitle("Connecticut") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Delaware = subset(scaled_x_DT, administrative_area_level_2 == "Delaware")
ccf_Delaware = ggCcf(DT_Delaware$new_deaths,DT_Delaware$transit,lag.max=35) + 
  ggtitle("Delaware") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_DC = subset(scaled_x_DT, administrative_area_level_2 == "District of Columbia")
ccf_DC = ggCcf(DT_DC$new_deaths, DT_DC$transit,lag.max=35) +
  ggtitle("District of Columbia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Florida = subset(scaled_x_DT, administrative_area_level_2 == "Florida")
ccf_Florida = ggCcf(DT_Florida$new_deaths, DT_Florida$transit,lag.max=35) +
  ggtitle("Florida") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Georgia = subset(scaled_x_DT, administrative_area_level_2 == "Georgia")
ccf_Georgia = ggCcf(DT_Georgia$new_deaths, DT_Georgia$transit,lag.max=35) +
  ggtitle("Georgia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Hawaii = subset(scaled_x_DT, administrative_area_level_2 == "Hawaii")
ccf_Hawaii = ggCcf(DT_Hawaii$new_deaths, DT_Hawaii$transit,lag.max=35) +
  ggtitle("Hawaii") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Idaho = subset(scaled_x_DT, administrative_area_level_2 == "Idaho")
ccf_Idaho = ggCcf(DT_Idaho$new_deaths, DT_Idaho$transit,lag.max=35) +
  ggtitle("Idaho") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Illinois = subset(scaled_x_DT, administrative_area_level_2 == "Illinois")
ccf_Illinois = ggCcf(DT_Illinois$new_deaths, DT_Illinois$transit,lag.max=35) +
  ggtitle("Illinois") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Indiana = subset(scaled_x_DT, administrative_area_level_2 == "Indiana")
ccf_Indiana = ggCcf(DT_Indiana$new_deaths, DT_Indiana$transit,lag.max=35) +
  ggtitle("Indiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Iowa = subset(scaled_x_DT, administrative_area_level_2 == "Iowa")
ccf_Iowa = ggCcf(DT_Iowa$new_deaths, DT_Iowa$transit,lag.max=35) +
  ggtitle("Iowa") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kansas = subset(scaled_x_DT, administrative_area_level_2 == "Kansas")
ccf_Kansas = ggCcf(DT_Kansas$new_deaths, DT_Kansas$transit,lag.max=35) +
  ggtitle("Kansas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Kentucky = subset(scaled_x_DT, administrative_area_level_2 == "Kentucky")
ccf_Kentucky = ggCcf(DT_Kentucky$new_deaths, DT_Kentucky$transit,lag.max=35) +
  ggtitle("Kentucky") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Louisiana = subset(scaled_x_DT, administrative_area_level_2 == "Louisiana")
ccf_Louisiana = ggCcf(DT_Louisiana$new_deaths, DT_Louisiana$transit,lag.max=35) +
  ggtitle("Louisiana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maine = subset(scaled_x_DT, administrative_area_level_2 == "Maine")
ccf_Maine = ggCcf(DT_Maine$new_deaths, DT_Maine$transit,lag.max=35) +
  ggtitle("Maine") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Maryland = subset(scaled_x_DT, administrative_area_level_2 == "Maryland")
ccf_Maryland = ggCcf(DT_Maryland$new_deaths, DT_Maryland$transit,lag.max=35) +
  ggtitle("Maryland") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Massachusetts = subset(scaled_x_DT, administrative_area_level_2 == "Massachusetts")
ccf_Massachusetts = ggCcf(DT_Massachusetts$new_deaths, DT_Massachusetts$transit,lag.max=35) +
  ggtitle("Massachusetts") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Michigan = subset(scaled_x_DT, administrative_area_level_2 == "Michigan")
ccf_Michigan = ggCcf(DT_Michigan$new_deaths, DT_Michigan$transit,lag.max=35) +
  ggtitle("Michigan") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Minnesota = subset(scaled_x_DT, administrative_area_level_2 == "Minnesota")
ccf_Minnesota = ggCcf(DT_Minnesota$new_deaths, DT_Minnesota$transit,lag.max=35) +
  ggtitle("Minnesota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Mississippi = subset(scaled_x_DT, administrative_area_level_2 == "Mississippi")
ccf_Mississippi = ggCcf(DT_Mississippi$new_deaths, DT_Mississippi$transit,lag.max=35) +
  ggtitle("Mississippi") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Missouri = subset(scaled_x_DT, administrative_area_level_2 == "Missouri")
ccf_Missouri = ggCcf(DT_Missouri$new_deaths, DT_Missouri$transit,lag.max=35) +
  ggtitle("Missouri") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Montana = subset(scaled_x_DT, administrative_area_level_2 == "Montana")
ccf_Montana = ggCcf(DT_Montana$new_deaths, DT_Montana$transit,lag.max=35) +
  ggtitle("Montana") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nebraska = subset(scaled_x_DT, administrative_area_level_2 == "Nebraska")
ccf_Nebraska = ggCcf(DT_Nebraska$new_deaths, DT_Nebraska$transit,lag.max=35) +
  ggtitle("Nebraska") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Nevada = subset(scaled_x_DT, administrative_area_level_2 == "Nevada")
ccf_Nevada = ggCcf(DT_Nevada$new_deaths, DT_Nevada$transit,lag.max=35) +
  ggtitle("Nevada") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NH = subset(scaled_x_DT, administrative_area_level_2 == "New Hampshire")
ccf_NH = ggCcf(DT_NH$new_deaths, DT_NH$transit,lag.max=35) +
  ggtitle("New Hampshire") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NJ = subset(scaled_x_DT, administrative_area_level_2 == "New Jersey")
ccf_NJ = ggCcf(DT_NJ$new_deaths, DT_NJ$transit,lag.max=35) +
  ggtitle("New Jersey") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NM = subset(scaled_x_DT, administrative_area_level_2 == "New Mexico")
ccf_NM = ggCcf(DT_NM$new_deaths, DT_NM$transit,lag.max=35) +
  ggtitle("New Mexico") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NY = subset(scaled_x_DT, administrative_area_level_2 == "New York")
ccf_NY = ggCcf(DT_NY$new_deaths, DT_NY$transit,lag.max=35) +
  ggtitle("New York") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_NC = subset(scaled_x_DT, administrative_area_level_2 == "North Carolina")
ccf_NC = ggCcf(DT_NC$new_deaths, DT_NC$transit,lag.max=35) +
  ggtitle("North Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_ND = subset(scaled_x_DT, administrative_area_level_2 == "North Dakota")
ccf_ND = ggCcf(DT_ND$new_deaths, DT_ND$transit,lag.max=35) +
  ggtitle("North Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Ohio = subset(scaled_x_DT, administrative_area_level_2 == "Ohio")
ccf_Ohio = ggCcf(DT_Ohio$new_deaths, DT_Ohio$transit,lag.max=35) +
  ggtitle("Ohio") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oklahoma = subset(scaled_x_DT, administrative_area_level_2 == "Oklahoma")
ccf_Oklahoma = ggCcf(DT_Oklahoma$new_deaths, DT_Oklahoma$transit,lag.max=35) +
  ggtitle("Oklahoma") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Oregon = subset(scaled_x_DT, administrative_area_level_2 == "Oregon")
ccf_Oregon = ggCcf(DT_Oregon$new_deaths, DT_Oregon$transit,lag.max=35) +
  ggtitle("Oregon") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Pennsylvania = subset(scaled_x_DT, administrative_area_level_2 == "Pennsylvania")
ccf_Pennsylvania = ggCcf(DT_Pennsylvania$new_deaths, DT_Pennsylvania$transit,lag.max=35) +
  ggtitle("Pennsylvania") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_RI = subset(scaled_x_DT, administrative_area_level_2 == "Rhode Island")
ccf_RI = ggCcf(DT_RI$new_deaths, DT_RI$transit,lag.max=35) +
  ggtitle("Rhode Island") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SC = subset(scaled_x_DT, administrative_area_level_2 == "South Carolina")
ccf_SC = ggCcf(DT_SC$new_deaths, DT_SC$transit,lag.max=35) +
  ggtitle("South Carolina") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_SD = subset(scaled_x_DT, administrative_area_level_2 == "South Dakota")
ccf_SD = ggCcf(DT_SD$new_deaths, DT_SD$transit,lag.max=35) +
  ggtitle("South Dakota") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Texas = subset(scaled_x_DT, administrative_area_level_2 == "Texas")
ccf_Texas = ggCcf(DT_Texas$new_deaths, DT_Texas$transit,lag.max=35) +
  ggtitle("Texas") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Tennessee = subset(scaled_x_DT, administrative_area_level_2 == "Tennessee")
ccf_Tennessee = ggCcf(DT_Tennessee$new_deaths, DT_Tennessee$transit,lag.max=35) +
  ggtitle("Tennessee") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Utah = subset(scaled_x_DT, administrative_area_level_2 == "Utah")
ccf_Utah = ggCcf(DT_Utah$new_deaths, DT_Utah$transit,lag.max=35) +
  ggtitle("Utah") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Vermont = subset(scaled_x_DT, administrative_area_level_2 == "Vermont")
ccf_Vermont = ggCcf(DT_Vermont$new_deaths, DT_Vermont$transit,lag.max=35) +
  ggtitle("Vermont") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Virginia = subset(scaled_x_DT, administrative_area_level_2 == "Virginia")
ccf_Virginia = ggCcf(DT_Virginia$new_deaths, DT_Virginia$transit,lag.max=35) +
  ggtitle("Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Washington = subset(scaled_x_DT, administrative_area_level_2 == "Washington")
ccf_Washington = ggCcf(DT_Washington$new_deaths, DT_Washington$transit,lag.max=35) +
  ggtitle("Washington") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_WV = subset(scaled_x_DT, administrative_area_level_2 == "West Virginia")
ccf_WV = ggCcf(DT_WV$new_deaths, DT_WV$transit,lag.max=35) +
  ggtitle("West Virginia") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wisconsin = subset(scaled_x_DT, administrative_area_level_2 == "Wisconsin")
ccf_Wisconsin = ggCcf(DT_Wisconsin$new_deaths, DT_Wisconsin$transit,lag.max=35) +
  ggtitle("Wisconsin") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

DT_Wyoming = subset(scaled_x_DT, administrative_area_level_2 == "Wyoming")
ccf_Wyoming = ggCcf(DT_Wyoming$new_deaths, DT_Wyoming$transit,lag.max=35) +
  ggtitle("Wyoming") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),  # move title to center
        panel.background = element_blank(), # remove background
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())


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