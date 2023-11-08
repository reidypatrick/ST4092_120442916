library(stringi)

geo_loc <- read_csv("data/input/France_Regions_data.csv")

geo_loc %>% select(`Region Name`) %>% distinct() %>% View()
reg_dict <- read_csv("data/input/regions_FR.csv", col_names = F) %>% 
  mutate(code = paste0('R', X2),
         region_name = toupper(X1)) %>% 
  select(-X2, -X1) %>%  
  mutate(region_name = stri_trans_general(str = region_name, 
                                   id = "Latin-ASCII")) %>% 
  mutate(region_name = ifelse(startsWith(region_name, "CENTRE"), "CENTRE", region_name))

cars_geo = cars %>% 
  left_join(reg_dict, join_by(Region == code)) 

test<- cross_join(reg_dict %>% select(region_name) %>% distinct,
geo_loc %>% select(`Region Name`) %>% distinct()) %>% filter(region_name == `Region Name`)

write_csv(cars_geo, "data/output/cars_geo.csv")
getwd()
