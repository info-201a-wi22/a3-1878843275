# It MUST point to the directory within which you are working.

# Here, some assignment functions are loaded. 
library("dplyr")
library("ggplot2")
library("stringr")
library("tidyverse")
library("sf")
library("giscoR")
library("mapproj")
library("usdata")
# Load the *incarceration-trends* data into a variable. `incarceration`data frame.
# NOTE: This is a large dataset. It may take 30-60 seconds to load.
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# How many observations (rows) are in this dataset?
# Create `obs_incarceration`.
obs_incarceration <- nrow(incarceration)

# How many features (columns) are there in this dataset?
# Create `num_features_incarceration`.
# also the name of columns.`name_features_incarceration`
num_features_incarceration<- ncol(incarceration)
name_features_incarceration<-colnames(incarceration)

# Exploratory Analysis ----------------------------------------------------

# How many total incarcerations(15 to 64) have there been in each state by the most recent date
# in the dataset? `total_each_state_incarcerations`
total_each_state_incarcerations<-incarceration%>%
  filter(year == max(year))%>%
  group_by(state)%>%
  summarize(num_incar=sum(total_pop_15to64))

# Which state has had the highest number of incarcerations?
# `state_highest_incarcerations`
state_highest_incarcerations<-total_each_state_incarcerations %>%
  filter(num_incar == max(num_incar)) %>%
  pull(state)

# What is the highest number of incarcerations in this state?
# `num_highest_state`
num_highest_state<-total_each_state_incarcerations %>%
  filter(state==state_highest_incarcerations)%>%
  pull(num_incar)

# Which county is the highest number of incarcerations(15to64)?
# `name_highest_county`
name_highest_county<-incarceration %>%
  filter(state == state_highest_incarcerations)%>%
  filter(year == max(year))%>%
  filter(total_pop_15to64 == max(total_pop_15to64)) %>%
  pull(county_name)

# Which county is the lowest number of incarcerations(15to64)?
# `name_lowest_county`
name_lowest_county<-incarceration %>%
  filter(state == state_highest_incarcerations)%>%
  filter(year == max(year))%>%
  filter(total_pop_15to64 == min(total_pop_15to64)) %>%
  pull(county_name)

#Find the state name of the top 5 number of incarcerations in `Name_state`
Name_state<-total_each_state_incarcerations%>%
  arrange(desc(num_incar)) %>% 
  slice(1:5)
  
# Make a data frame of all incarcerations of the top 5 states in different years.
# `state_incarceration`
state_incarceration<-incarceration%>%
  filter(state=="CA" | state=="TX" | state=="FL" | state=="NY" | state=="IL")%>%
  group_by(year,state)%>%
  summarize(total_pop_15to64=sum(total_pop_15to64))

# Make a data frame to summarize the total female and male incarceration in California.`CA_incar`
CA_incarceration<-incarceration%>%
  filter(state=="CA")%>%
  group_by(year)%>%
  summarize(female_pop_15to64=sum(female_pop_15to64), male_pop_15to64=sum(male_pop_15to64))

# What is the highest number of female incarcerations in California in the most recent year?
# `female_highest_CA`
female_highest_CA<-CA_incarceration%>%
  filter(year==max(year))%>%
  pull(female_pop_15to64)

# What is the highest number of male incarcerations in California in the most recent year?
# `female_highest_CA`
male_highest_CA<-CA_incarceration%>%
  filter(year==max(year))%>%
  pull(male_pop_15to64)

## A data fame about female incarceration in different county in us in recent year.
total_female<-incarceration%>%
  filter(year == max(year))%>%
  group_by(state)%>%
  summarize(num_female=sum(female_pop_15to64))%>%
  mutate(state=abbr2state(state))
total_female$state = tolower(total_female$state)

# Data Visualization
# Different states Incarceration Trends Over Time
cols <- c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA", "#9632B8")
first_chart<-ggplot(state_incarceration, aes(x = year, y = total_pop_15to64, color = state)) +
  geom_line() +
  scale_color_manual(values = cols)+
  labs(title="Top Five states Incarceration Trends Over Time",
       x="year",
       y="total incarceration (15 to 64)")

# Female vs Male Incarceration
second_chart<-ggplot(CA_incarceration, aes(x=year)) +                    
  geom_line(aes(y=female_pop_15to64), colour="red") +  
  geom_line(aes(y=male_pop_15to64), colour="green")+
  labs(title="Female vs Male Incarceration in CA (15 to 64)",
       x="year",
       y="incarceration")

# Incarceration by Geography
state_shape <-map_data("state")%>%
  rename(state = region)%>%
  left_join(total_female,by="state")

third_map<- ggplot(state_shape)+
               geom_polygon(
                 mapping = aes(x=long, y =lat, group=group, fill=num_female),
                 color="white",
                 size =.1)+
  coord_map()+
  scale_fill_continuous(low="#EEA236", high="red")+
                          labs(fill = "number female incarceration")+
  labs(title="Female Incarceration by Geography")
