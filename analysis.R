
library(tidyverse)
library(tidyr)
library(data.table)
library(janitor)
library(lubridate)
library(zoo)



#Import the CSV file with the names of ecah location
metadata <- readxl::read_excel("data/VicRoads_Bike_Site_Number_Listing.xlsx") %>% 
  distinct(SITE_XN_ROUTE,LOC_LEG,.keep_all = TRUE) %>% 
  clean_names() %>% 
  select(site_xn_route,loc_leg,loc_desc)


##import the public holidays data

#Look for every csv file in the subfolder "public_holidays"

filenames_ph <- list.files(pattern="*.csv*",
                           recursive = TRUE,
                           path = "public_holidays/")

filenames_ph = paste0("public_holidays/",filenames_ph)

#Import every filename we mentioned above, and clean up the data. 
public_holidays = lapply(filenames_ph, fread) %>% 
  bind_rows() %>% 
  clean_names() %>%
  mutate(date = ymd(date),
         jurisdiction = if_else(is.na(jurisdiction),applicable_to,jurisdiction),
         jurisdiction = tolower(jurisdiction)) %>% 
  filter(str_detect(jurisdiction,"vic|nat"))%>% 
  select(date) %>% 
  mutate(public_holiday = 1)

##import the bike counts data

filenames_counts <- list.files(pattern="*.csv*",
                  recursive = TRUE,
                  path = "data") 

filenames_counts = paste0("data/",filenames_counts)

myfiles = lapply(filenames_counts, fread)
  
  
  output <- bind_rows(myfiles) %>% 
     clean_names()%>%
                    group_by(vehicle, 
                             site_xn_route,
                             direction,
                             date,
                             loc_leg) %>% 
                    summarise( n = n()) %>% 
                    ungroup() %>% 
                    mutate(vehicle = tolower(vehicle)) %>% 
                    filter(vehicle == "cycle") %>% 
    mutate(date    = dmy(date),
           year    = paste0("y",year(date)),
           month   = month(date),
           day     = day(date),
           wday    = wday(date,label = TRUE,abbr = FALSE),
           weekend = if_else(wday %in% c("Saturday","Sunday"),"weekend","weekday"),
           week    = week(date)) %>% 
    left_join(metadata) %>% 
    left_join(public_holidays) %>% 
    filter(is.na(public_holiday)) %>% 
    ungroup() %>% 
    group_by(weekend,loc_leg) %>% 
    arrange(date) %>% 
    mutate(path_lane = case_when(str_detect(loc_desc,"BIKE PATH") ~ "bike_path",
                                 str_detect(loc_desc,"BIKE LANE") ~ "bike_lane",
                                 TRUE ~ "other"),
           roll_length = if_else(weekend == "weekend",11,31),
           median_rolling = rollmedian(n,roll_length,align = "right", fill = 0)) %>% 
    filter( n > (median_rolling * .2),
            median_rolling != 0,
            path_lane !="other")
  

  
  
yearly_graph <- output %>% 
  filter(week<20) %>% 
  group_by(week,
           year,
           path_lane,
           weekend,
           loc_leg) %>% 
  filter(n()>1) %>% 
  summarise(median = median(n)) %>% 
  spread(year,median) %>% 
  filter(!is.na(y2019), 
         !is.na(y2020)) %>%
  mutate(ratio = y2020/
                 y2019)
  
  
  yearly_graph %>% 
    filter(ratio != 0 ) %>% 
    ggplot(aes(x = week, 
               y = ratio))+
  geom_point(stat = "identity")+
  facet_grid(path_lane ~ weekend) +
  geom_smooth()+
  coord_cartesian(ylim = c(0,4))+
  labs(y = "ratio of median value this year compared with the median value of the same weekend/week last year",
       caption = "excludes public holidays.")
  
  
  