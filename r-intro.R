#Set working directory to r-analysis, the file with the data and scripts
#load required packages
# load required packages
library(readr)
library(dplyr)
library(ggplot2)

# load health and wealth of nations data
nations1 <- read_csv("nations1.csv")
nations2 <- read_csv("nations2.csv")

# view the first six rows of nations1 data
head(nations1)

#look at the population column specifically
nations1$population

# convert population to integers
nations1$population <- as.integer(nations1$population)

#now check to see the change
head(nations1)

#Exercise: do the same thing for the year column
#look at year specifically by typing nations1 and the $ operator and then 'year'

#now use the as.integer function to change year 

#now check again using the head function to make sure the change took effect

# summary of nations1 data
summary(nations1)

#uh-oh income is character? is that right?
nations1$income

#Manipulating with dplyr
#filtering and sorting
#filter nations1 for 2014 and create new data called longevity
longevity <- nations1 %>%
  filter(year == 2014 & !is.na(life_expect)) %>%
  select(country, life_expect, income, region)

#check the result 
head(longevity)

#Exercise: did we miss the newest data in 2015? Replicate for 2015 and see what happens -- call the result longevity2015

#now, let's look for the top 10 high-income countries with the lowest life expectancy
high_income_short_life <- longevity %>%
  filter(income == "High income") %>%
  arrange(life_expect) %>%
  head(10)
#Now look at what we have
head(high_income_short_life,10)

# find the 20 countries with the longest life expectancies, 
# plus the United States with its rank, if it lies outside the top 20
long_life <- longevity %>%
  arrange(desc(life_expect)) %>%
  mutate(rank = c(1:195)) %>%
  filter(rank <= 20 | country == "United States")
#take a look
head(long_life,21)
#Tibble won't let us see that many rows so we fall back on print()
print(tbl_df(long_life), n=21)

# find the 20 countries with the longest life expectancies,
# plus the United States and Russia with their ranks
long_life <- longevity %>%
  arrange(desc(life_expect)) %>%
  mutate(rank = c(1:195)) %>%
  filter(rank <= 20 | grepl("United States|Russia", country))
#Again use print to see all the rows
print(tbl_df(long_life), n=22)

# write data to a csv file
write_csv(long_life, "long_life.csv", na="")
#Group and summarize
# summarize the data by year, finding the maximum and minimum country-level life expectancies, and then calculate the range of values
longevity_summary <- nations1 %>%
  filter(!is.na(life_expect)) %>%
  group_by(year) %>%
  summarize(countries = n(),
            max_life_expect = max(life_expect),
            min_life_expect = min(life_expect)) %>%
  mutate(range_life_expect = max_life_expect - min_life_expect) %>%
  arrange(desc(year))

#look at data
print(tbl_df(longevity_summary), n=25)
#Joining
#join nations and nations2
nations <- inner_join(nations1, nations2)

# total GDP, in trillions of dollars, by region, over time
gdp_regions <- nations %>%
  mutate(gdp = gdp_percap * population,
         gdp_tn = gdp/1000000000000) %>%
  group_by(region, year) %>%
  summarize(total_gdp_tn = sum(gdp_tn, na.rm = TRUE))
#look at first 10
head(gdp_regions,10) 

# what happens if we don't remove missing values? 
gdp_regions <- nations %>%
  mutate(gdp = gdp_percap * population,
         gdp_tn = gdp/1000000000000) %>%
  group_by(region, year) %>%
  summarize(total_gdp_tn = sum(gdp_tn))
#look at first 10
head(gdp_regions,10)   

# Draw a line chart showing total GDP, by region, over time.
ggplot(gdp_regions, aes(x=year, y=total_gdp_tn, color=region)) +
  geom_line(size=1) +
  xlab("") +
  ylab("Total GDP ($ trillions)") +
  theme_minimal(base_size = 12)
#Bar chart of 10 high income countries with lowest life expectancies
ggplot(high_income_short_life, aes(x=reorder(country,-life_expect), y=life_expect)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.7) +
  xlab("") +
  ylab("Life expectancy at birth (2014)") + 
  ggtitle("Rich countries with low life expectancies") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  coord_flip()
