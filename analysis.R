
library(tidyverse)
library(tidyr)
library(data.table)
library(janitor)
library(lubridate)

metadata <- readxl::read_excel("data/VicRoads_Bike_Site_Number_Listing.xlsx") %>% 
  distinct(SITE_XN_ROUTE,LOC_LEG,.keep_all = TRUE) %>% clean_names()

temp = list.files(pattern="*.csv*",recursive = TRUE)
view(temp)
myfiles = lapply(temp, fread)
  
  
  output <- dplyr::bind_rows(myfiles) %>% clean_names()
  
  
  
  output_grouped <- output %>% 
                    group_by(vehicle, 
                             site_xn_route,
                             direction,
                             date,
                             loc_leg) %>% 
                    summarise(n=n()) %>% 
                    ungroup() %>% 
                    mutate(vehicle = tolower(vehicle)) %>% 
                    filter(vehicle == "cycle") %>% 
    mutate(date = dmy(date),
           year = paste0("y",year(date)),
           month = month(date),
           day = day(date))
  
month_av <- output_grouped %>% group_by(month,
                                        year,
                                        site_xn_route,
                                        loc_leg,
                                        direction) %>% 
    summarise(month_median = median(n, na.rm = TRUE))
  
  
graph <- output_grouped %>% 
            left_join(month_av) %>% 
             mutate(month_day = mdy(paste0(month,"-",day,"-2020"))) %>% 
             filter(n < (month_median)) %>%
             group_by(site_xn_route,
                      loc_leg,
                      month,
                      day) %>% 
             select(-month_median, - date) %>% 
             spread(year,n) %>% 
             mutate(ratio = y2020/y2019) %>% 
             filter(!is.na(ratio)) %>% 
  left_join(metadata)

write_csv(graph,"results.csv")




graph %>% 
  filter(str_detect(loc_desc,"UPFIELD")) %>% view()
    ggplot(aes(x = month_day, 
               y = ratio,
               fill = as.factor(loc_leg)))+
  geom_smooth()+
  labs(title = "More people are cycling!",
       subtitle = "Ratio of cycling from 2019 to 2020. Loess fit. ",
       caption = "Data from Vicroads sensors on shared paths.",
       x = element_blank())+
  geom_hline(yintercept = 1)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_point()
  
                    
  