cars
cars %>% 
  mutate(VehBrand = as.integer(substr(VehBrand, 2, 3)))

cars$VehBrand = as.integer(substr(cars$VehBrand,2,3))
cars           
as.numeric(levels(cars$Region))
as.numeric(levels(cars$Region))

unclass(cars$Region) %>%  view()


boxplot(cars$Exposure)
abline(h=1)


cbind(cars$Region %>%  unclass,
      cars_geo$region_name)

factor(cars_geo$region_name, levels = sort(unique(cars_geo$region_name))) %>% unclass()



