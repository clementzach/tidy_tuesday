library('tidytuesdayR')
library(ggplot2)

library(dplyr)
library(tidyr)

five_year_round <- function(input_year){
  new_num = round(input_year +1, digits = -1)
  if ((input_year - new_num) < 0){
    new_num <- new_num-5
  }
  return(paste(new_num, substr(as.character(new_num+5), start = 3, stop = 4), sep = "-"))
}

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

departures_five_year <- departures %>% mutate(five_year = sapply(fyear, five_year_round))

named_dismissal <- departures$departure_code %>% replace_na(replace = 10) %>% factor()
levels(named_dismissal) <- c("Death" , 
                             "Illness" , 
                             "Job Performance", 
                             "Legal", 
                             "Retirement",
                             "Advancement",
                             "Other", 
                             "Missing", 
                             "Error" , 
                             "na" )

named_dismissal <- ordered(named_dismissal, levels = c( "Other", 
                                                       "Missing", 
                                                       "Error" , 
                                                       "na" ,
                                                       "Death" , 
                                                       "Illness",
                                                       "Job Performance", 
                                                       "Legal", 
                                                       "Retirement",
                                                       "Advancement"))

ggplot(data = departures_five_year, aes(fill =  named_dismissal, y = departure_code, x = factor(five_year ))) + 
  scale_fill_manual(values = c( "gray1", "gray20", "gray40", "tan1", "tan4", "violetred1", "violetred4", "skyblue1", "skyblue4"))+
  geom_bar(position = "fill", stat = "identity") +
  labs(y = "", 
       x = "Five-Year Period", 
       fill = "Reason for dismissal") +
  theme_bw()
 
       

                                                                                                                                                                                                                                                                                                  five_year_round(departures$fyear)       
