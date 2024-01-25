cars_density <- cars_orig %>% 
  group_by(Region) %>% 
  group_by(Density) %>% 
  summarise(Density = sum(Density))
cars_density

cars_density <- cars_orig %>% 
  group_by(Region, Density) %>% 
  summarise(Density = sum(Density)) %>% 
  group_by(Region) %>% 
  summarise(Density = mean(Density)) 

create_map(cars_density, Density, shape)


