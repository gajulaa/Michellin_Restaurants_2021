library(tidyverse)
library(readr)
michelin_my_maps <- read_csv("michelin_my_maps.csv")
View(michelin_my_maps)

#loaded the tidyverse package and imported the dataset automatically using the
#files tab on the pane layout

view(michelin_my_maps)
glimpse(michelin_my_maps)

#the above commands allowed to me to look through the dataset and view sample
#data. the data had some redundant and irrelevant variables for this analysis. 
#Some variables could also be re-coded more efficiently owing to their input
#values

attach(michelin_my_maps) #this command allowed for direct reference of
#variables
unique(Award)
unique(Currency)
Award <- as.factor(Award)
levels(Award)
#discussed above, Currency and Cuisine qualified for re-coding to factor type 
#variables. However, going by grouping, the Currency variable had too many 
#currency types and levels was not viable as currency is defined as being 
#relative with each type. e.g, USD can be represented in equivalent JPY values
#or GBP values and so on. Nevertheless, Award was a perfect candidate for factor
#re-coding as unique values increased in level relative to each other. e.g, 1 
#Michelin, 2 Michelin Star, 3 Michelin Star and so on. 

Award <- factor((Award), levels = c("Bib Gourmand", "1 MICHELIN Star", 
                                    "2 MICHELIN Stars", "3 MICHELIN Stars"))
levels(Award) # verifying if the levels were assigned correctly. 

#After re-coding the Award variable, the levels were listed wrong and needed to
#be changed. Since, Bib Gourmand is not a higher level than 3 Michelin stars, 
#the first level was assigned to it.

colSums(is.na(michelin_my_maps))

#Number of NA values is the highest for Website URL variable with 1127 values.
#Followed by the Phone Number variable. Why? incorrect collection methods? What 
#are the parameters of data collection?

michelin_my_maps %>% 
  select(Address, Location, MinPrice, Currency, Cuisine, Award) %>% 
  filter(!complete.cases(.))
# testing variable selection to try and identify observation. get a sense of the 
# "missingness" of the data. The function returned a single observation where 
# MinPrice and Currency is missing. Further evaluation reveals Location is 
# incorrectly inputted in that same observation. Is MinPrice and Currency input
# dependent on Location? What are the parameters?

michelin_my_maps %>% 
select(Address, Location, MinPrice, Currency, Cuisine, Award, WebsiteUrl) %>% 
  filter(!complete.cases(.)) %>% 
  view()

# Location, Longitude and Latitude were redundant. Location was more favorable.
# From an ease of understanding and familiarity standpoint. Decided to include 
# Website URL variable as people reading the analysis could want to know more 
# about the restaurant on their own. 

michelin_no_na <-michelin_my_maps %>% 
  select(Address, Location, MinPrice, Currency, Cuisine, Award, WebsiteUrl) %>% 
  na.omit(WebsiteUrl, Currency, MinPrice,) %>% 
  view()

#Dropping the NA values of the above variables was the logical choice given the 
#exploration beforehand. Selected data is clean without NA values in the data
#subset. A 

michelin_my_maps %>% 
  distinct() %>% 
  view()

#no duplicate values were found. 
# recoding the variable input isn't feasible in this dataset

#since data is intangible with contrasting datatypes, summary statistics are not
#feasible. 

michelin_award_minprice <-michelin %>% 
  drop_na(Award) %>% 
  group_by(Award) %>% 
  summarize(Lower = min(MinPrice), 
            Average = mean(MinPrice),
            Upper = max(MinPrice),
            Difference = max(MinPrice) - min(MinPrice)) %>% 
  view()

#created a data frame where Award was grouped and with regard to the variable, 
#MinPrice summary statistics were established according to the grouped values. 
#Bib Gourmand as establihsed, has the lowest price among the other distinctions. 