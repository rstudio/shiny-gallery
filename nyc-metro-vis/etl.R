library(tidyverse)
library(lubridate)

data_station <- read_csv(paste0(here::here(),"/data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv"))
data3 <- read_csv("http://web.mta.info/developers/data/nyct/turnstile/turnstile_190302.txt")


all <- inner_join(data_station %>% 
                    mutate(`Station Name`=toupper(`Station Name`)),data3,by=c("Station Name"="STATION","Division"="DIVISION")) %>% 
  filter(DESC == 'REGULAR',
         Entry =='YES') 

all_clean <- all %>% 
  mutate(TIME_C = paste(DATE,TIME) %>% mdy_hms(),
         ENTRIES = as.numeric(ENTRIES),
         EXITS = as.numeric(EXITS)) %>% 
  distinct(`Station Latitude`,`Station Longitude`,TIME_C,Line,ENTRIES,SCP
           ,`C/A`
           #,UNIT,Division,LINENAME
  ) %>% 
  filter(SCP == '00-00-00') %>% 
  group_by(`Station Latitude`,`Station Longitude`,`C/A`) %>% 
  arrange(TIME_C) %>% 
  mutate(ENTRIES_new = ENTRIES - lag(ENTRIES)) %>% 
  mutate(ENTRIES_new = case_when(
    is.na(ENTRIES_new) ~ 0,
    TRUE ~ ENTRIES_new
  )) %>% ungroup()

write_rds(data_station,"data/cache_coordinate.rds")


write_rds(all_clean,"data/cache_all_data.rds")



