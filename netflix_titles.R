library(dplyr)
library(ggplot2)

year_from_date <- function(input_string){
  substr(input_string, 
         start = nchar(input_string) - 3, 
         stop = nchar(input_string)) %>% 
    as.numeric() %>% 
    return()
}

netflix_titles <- read.csv('/Users/zacharyclement/Desktop/tidy_tuesday/netflix_titles.csv') %>% 
  mutate(year_added = year_from_date(date_added)) %>% 
  filter(!is.na(year_added) & 
           year_added >= 2015 & ## There were 0 movies added in 2014 in our dataset
           !is.na(release_year))
         


ggplot(data = netflix_titles, aes(x = factor(year_added), y = year_added - release_year, color = type)) +
  geom_boxplot() +
  scale_color_manual(values = c("red3", "grey40")) +
  labs(y = "Age when added (Years)", 
       x = "Year added", 
       color = "Type", 
       title = "Age of Movies and TV Shows when \nadded to Netflix", 
       caption = "(Data provided by Flixable)") +
  theme_bw()
  



