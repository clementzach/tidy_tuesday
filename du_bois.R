library(dplyr)
library(tidyr)
library(ggplot2)


tuesdata <- tidytuesdayR::tt_load('2021-06-15')


find_source <- function(input_text){
  if (grepl('Android', input_text )){
    return("Android")
  }
  else if (grepl('Web App', input_text )){
    return("Web App")
  }
  else if (grepl('iPad', input_text )){
    return("iPad")
  }
  else if (grepl('iPhone', input_text )){
    return("iPhone")
  }
  else if (grepl('TweetDeck', input_text )){
    return("TweetDeck")
  }
  else if (grepl('Web App', input_text )){
    return("Web App")
  }
  else{
    return("other")
  }
}

tweets = tuesdata$tweets %>% mutate(source = sapply(text, find_source),
                                    num_tags = sapply(content, function(x){length(gregexpr('#', x)[[1]])}), 
                                    num_mentions = sapply(content, function(x){length(gregexpr('@', x)[[1]])}))


hist(tweets$num_tags)


ggplot(data = tweets) + geom_boxplot(mapping = aes(y = num_tags, x = source, col = source)) + 
  theme_classic() + 
  labs(title = "Number of Hashtags Used Per Tweet") + 
       xlab("App Used") + 
       ylab('Number of Hashtags')

ggplot(data = tweets) + geom_boxplot(mapping = aes(y = num_mentions, x = source, col = source)) + 
  theme_classic() + 
  labs(title = "Number of Mentions Per Tweet") + 
  xlab("App Used") + 
  ylab('Number of Mentions')
