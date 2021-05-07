library(ggmap)
library(readr)
library(ggplot2)


water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')


peru_map <- get_stamenmap(bbox = c(left = -81.91, bottom = -11.09031, right = -70.03, top = -2.800186), 
                          zoom = 6, 
                          color = "color", 
                          maptype = "toner")




ggmap(peru_map) +
  geom_point(data=water[! is.na(water$install_year) & (water$country_name == "Peru"),], 
             aes(x=lon_deg, y=lat_deg, colour= install_year), 
             size=.5, alpha=1) +
  labs(x = "", 
       y = "" , 
       title = "Age of Water Installations in Peru",
       colour = "Year Installed",
       caption = "(Data provided by Water Point Data Exchange)") +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
