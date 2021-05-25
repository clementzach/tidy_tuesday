library(readr)
library(dplyr)
library(ggplot2)

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

levels(as.factor(tolower(survey$country)))

sum(survey$country == "Virginia")


#how to figure out whether people are based in the USA:

is_usa <- function(input_string){
  cleaned_string <- tolower(gsub("[[:punct:][:space:]]", "", input_string))
  if (substr(cleaned_string, start = 1, stop = 2) == "us") {
    return(TRUE)
  }
  else if (("un" %in% cleaned_string) & ("stat" %in% cleaned_string) ){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

levels(as.factor(survey$years_of_experience_in_field))

process_experience <- function(input_string){
  named_list <- c("1 year or less" = 1,   "11 - 20 years"  = 16, "2 - 4 years" = 3.5, "21 - 30 years" = 26,
                  "31 - 40 years" = 36,  "41 years or more" = 46, "5-7 years" = 6.5, "8 - 10 years" = 9.5 )
  my_num <- named_list[input_string]
  if (my_num < 1){
    return(my_num + runif(1, min = -1, max = 1)) ## This may be shady but it will give our graph more readability
  }
  else if (my_num == 3){
    return(my_num + runif(1, min = -1.5, max = 1.5))
  }
  else if (my_num < 10){
    return(my_num + runif(1, min = -1.5, max = 1.5))
  }
  else if (my_num > 10){
    return(my_num + runif(1, min = -5, max = 5))
  }
}

process_category <- function(input_string){
  if (is.na(input_string)){
    return("unidentified")
  }
  else if (tolower(substr(input_string, start = 1, stop = 8))  == "research"){
    return("tech")
  }
  else if (tolower(substr(input_string, start = 1, stop = 4))  == "tech"){
    return("tech")
  }
  else if (tolower(substr(input_string, start = 1, stop = 5))  == "scien"){
    return("tech")
  }
  else if (tolower(substr(input_string, start = 1, stop = 8))  == "real est"){
    return("real estate")
  }
  else if (tolower(substr(input_string, start = 1, stop = 5))  == "pharma"){
    return("pharmaceutical")
  }
  else if (tolower(substr(input_string, start = 1, stop = 6))  == "govern"){
    return("government")
  }
  else if (tolower(substr(input_string, start = 1, stop = 4))  == "food"){
    return("food")
  }
  else if (tolower(substr(input_string, start = 1, stop = 3))  == "env"){
    return("environment")
  }
  else if (tolower(substr(input_string, start = 1, stop = 5))  == "educa"){
    return("education")
  }
  else if (tolower(substr(input_string, start = 1, stop = 9))  == "construct"){
    return("construction")
  }
  
  else{
    return("unidentified")
  }
    
}




only_usa <- survey %>% filter(sapply(country, is_usa)) %>% 
  mutate(numeric_experience = sapply(years_of_experience_in_field, process_experience),
         filtered_category = sapply(industry, process_category), 
         thousand_salary = annual_salary / 1000,
         highest_level_of_education_completed <- factor(highest_level_of_education_completed, 
                                                           levels = c("High School", "Some college", 
                                                                      "College degree","Master's degree",
                                                                      "Professional degree (MD, JD, etc.)", 
                                                                      "PhD" ))) %>% 
  filter(annual_salary < 500000, ! is.na(highest_level_of_education_completed))



## is this 
my_df <- only_usa %>% filter(filtered_category == "unidentified") %>% 
  group_by(industry) %>% 
  summarise(Unique_Elements = n_distinct(annual_salary))



ggplot(data = only_usa) + 
  scale_color_brewer(type = "qual", palette = 3) +
  theme_bw() +
  geom_point(aes(y = thousand_salary, x = numeric_experience, col = highest_level_of_education_completed), size = .5) +
  geom_smooth(method = "lm", aes(x = numeric_experience, y = thousand_salary, col = highest_level_of_education_completed)) +
  labs(color = "Highest Level of Education Completed", x = "Years of experience", y = "Annual Salary (Thousands)")

