cars
cars %>% 
  mutate(VehBrand = as.integer(substr(VehBrand, 2, 3)))

cars$VehBrand = as.integer(substr(cars$VehBrand,2,3))
cars           
as.numeric(levels(cars$Region))
as.numeric(levels(cars$Region))

unclass(cars$Region) %>%  view()
factor(cars$Region, levels = c(1length(cars$IDpol)

boxplot(cars$Exposure)
abline(h=1)
unique(cars$Region)

cbind(cars$Region %>%  unclass,
      cars_geo$region_name)

factor(cars_geo$region_name, levels = sort(unique(cars_geo$region_name))) %>% unclass()
