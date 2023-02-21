### ========== a5-data ==========
# This is a problem set about rmarkdown and plotting (using ggplot). Unlike the previous problem sets, this one does not give you a ready-made GH repo with a code file–it is now your task to create a repo and include your rmarkdown file in there.\
# You should answer the questions below in that file, knit it, and submit both the compiled html and link to your repo on canvas.\
#  This problem sets asks you to write extensively when commenting your results. Please write clearly! Answer questions in a way that if the code chunks are hidden then the result is still readable! \#  * All substantial questions need explanations. You do not have to explain the simple things like “how many rows are there in data”, but if you make a plot of life expectancy, then you should explain what does the plot tell you. \
#  * Write explanations as markdown and use the styles like bold and italic as appropriate.\
#  Do not print too much results. It is all well to print a few lines of data for evaluation/demonstration purposes. But do not print dozens (or thousands!) of lines–no one bothers to look at that many numbers. You will lose points for annoying others (here your graders, but later potentially your boss). \
#  Do not make code lines too long. 80-100 characters is a good choice. Your grader may not be able to follow all the code if the line is too long–most of us are using small laptop screens! (And again–you want to keep your graders happy!) \
# Gapminder data
# We use gapminder dataset, downloaded from [**https://www.gapminder.org/data/**](https://www.gapminder.org/data/), however, the data structure there is quire complex, please use the dataset provided on canvas (in files/data). \
# The variables are: \
# **name** country name \
# **iso3** 3-letter country code \
# **iso2** 2-letter country code \
#v**region** broad geographic region \
# **sub-region** more precise region \
# v**intermediate-region** \
# v**time** year \
# **totalPopulation** total population 
# **GDP_PC** GDP per capita (constant 2010 US$) \
# **accessElectricity** Access to electricity (% of population) \
# **agriculturalLand** Agricultural land (sq. km) \
# **agricultureTractors** Agricultural machinery, tractors (count) \
# **cerealProduction** Cereal production (metric tons) \
# **feritilizerHa** Fertilizer consumption (kilograms per hectare of arable land) \
# **fertilityRate** total fertility rate (births per woman) \
# **lifeExpectancy** Life expectancy at birth, total (years) \
# **childMortality** Mortality rate, under-5 (per 1,000 live births) \
# **youthFemaleLiteracy** Literacy rate, youth female (% of females ages 15-24) \
# **youthMaleLiteracy** Literacy rate, youth male (% of males ages 15-24) \
# **adultLiteracy** Literacy rate, adult total (% of people ages 15 and above) \
# **co2** CO2 emissions (kt) \
# **greenhouseGases** Total greenhouse gas emissions (kt of CO2 equivalent) \
# **co2_PC** CO2 emissions (metric tons per capita) \
# **pm2.5_35** PM2.5 pollution, population exposed to levels exceeding WHO Interim Target-1 value 36ug/m3 \
# **battleDeaths** Battle-related deaths (number of people) \
# 1 Load and check data (5pt)
#You first task is to do a very simple data check: 
#1. (1pt) For solving the problems, and answering the questions, create a new rmarkdown document with an appropriate title. See [**https://faculty.washington.edu/otoomet/info201-book/r-markdown.html#r-markdown-rstudio-creating**](https://faculty.washington.edu/otoomet/info201-book/r-markdown.html#r-markdown-rstudio-creating). \
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
#2. (2pt) Load data. How many rows/columns do we have?  \
data00 <- read.csv("/Users/apple/documents/Github/gapminder.csv",head = TRUE, sep="\t")
nrow(data00)
ncol(data00)
# 3. (2pt) Print a small sample of data. Does it look OK? 
data00 %>%
  sample_n(1)
# it contains NA VALUES.



# 2 Descriptive statistics (15pt)

# 1. (3pt) How many countries are there in the dataset? Analyze all three: *iso3*, *iso2* and *name*.
data00 %>%
  summarise(n = n_distinct(iso2))
data00 %>%
  summarise(n = n_distinct(iso3))
data00 %>%
  summarise(n = n_distinct(name))
# 2. If you did this correctly, you saw that there are more iso-2 codes than names, and there are even more *iso3*-codes. What is going on? Can you find it out?

#(a) (5pt) Find how many names are there for each iso-2 code. Are there any iso-2 codes that correspond to more than one name? What are these countries?
data00 %>%
  group_by(iso2) %>%
  summarise(count_iso2 = n_distinct(name))

## there are iso2 codes that correspond to more one name.

#(b) (5pt) Now repeat the same for name and iso3-code. Are there country names that have more than one iso3-code? What are these countries? Hint: two of these entitites are *CHANISL* and *NLD CURACAO*.
data00 %>%
  group_by(name) %>%
  summarise(count_iso3 = n_distinct(iso3)) %>%
  arrange(desc(count_iso3))

# 3. (2pt) What is the minimum and maximum year in these data?
data00 %>%
  filter(!is.na(time))%>%
  summarise(min = min(time), 
            max = max(time))


# 3 CO2 emissions (30pt)
#Next, let’s analyze CO2 emissions.

# 1. (2pt) How many missing co2 emissions are there for each year? Analyze both missing *CO2* and *co2_PC*. Which years have most missing data? 
# data00 %>%
data00 %>%
  group_by(time) %>%
  summarise(missing_co2 = sum(is.na(co2))) %>%
  arrange(desc(missing_co2))

data00 %>%
  group_by(time) %>%
  summarise(missing_co2_pc = sum(is.na(co2_PC))) %>%
  arrange(desc(missing_co2_pc))
### 2017,2018, 2019 have the most missing data.


# 2. (5pt) Make a plot of total CO2 emissions over time for the U.S, China, and India. Add a few more countries of your # # # # choice. Explain what do you see. \
data001 <- data00 %>%
  filter(iso2 %in% c("US", "CN", "IN", "AU", "BY")) %>%
  filter(!is.na(co2)) %>%
  group_by(name) %>%
  summarise(co2_total = sum(co2))

ggplot(data001, aes(x=name, y=co2_total)) +
  geom_bar(stat="identity", width = .5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
# 3. (5pt) Now let’s analyze the CO2 emissions per capita (*co2_PC*). Make a similar plot of the  same countries. What does this figure suggest? 
data006 <- data00 %>%
  filter(iso2 %in% c("US", "CN", "IN", "AU", "BY")) %>%
  filter(!is.na(co2_PC))
ggplot(data006, aes(x=name, y=co2_PC)) +
  geom_bar(stat="identity", width = .5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
# 4. (6pt) Compute average CO2 emissions per capita across the continents (assume *region* is the same as continent). Comment what do you see. \
# Note: just compute averages over countries and ignore the fact that countries are of different size. \
# Hint: Americas 2016 should be 4.80. \
data00 %>%
  filter(!is.na(co2_PC) & region!="") %>%
  group_by(region) %>%
  summarise(avg_co2_pc=mean(co2_PC))
###I can see that Europe is the region  with the highest average co2 emissions per capital.
### Africa has the lowest average co2 emissions per capital.

# 5. (7pt) Make a barplot where you show the previous results–average CO2 emissions per capita across continents in 1960 and 2016. \
# Hint: it should look something along these lines(**Please refer to the spec for the graph**) \
data007 <-data00 %>%
  filter(!is.na(co2_PC) & region!="" & time %in% c(1960, 2016)) %>%
  group_by(region, time) %>%
  summarise(avg_co2_pc=mean(co2_PC))
data007$year <- as.factor(data007$time)
ggplot(data007, aes(x=region, y=avg_co2_pc, fill=year)) +
  geom_bar(stat="identity") +
  labs(x='Region', y='Average CO2 per capital')
# 6. Which countries are the three largest, and three smallest CO2 emitters (in terms of CO2 per capita) in 2019 for each # continent? (Assume *region* is continent). \
sort_data <- data00 %>%
  filter(time == 2019) %>%
  filter(is.na(co2_PC)) %>%
  arrange(co2_PC)
head(sort_data, 3)
tail(sort_data, 3)
### there was no answer to this queastion as the co2_pc values are missing.

# 4 GDP per capita (50pt)
# Let’s look at GDP per capita (*GDP_PC*)

# 1. (8pt) Make a scatterplot of GDP per capita versus life expectancy by country, using data for 1960. Make the point size dependent on the country size, and color those according to the continent. Feel free to adjust the plot in other ways to make it better. \
# Comment what do you see there.
data_1960 <- data00 %>%
  filter(time == 1960 & !is.na(GDP_PC))
ggplot(data_1960, aes(x=GDP_PC, y=lifeExpectancy)) +
  geom_point(aes(size=totalPopulation, col=region)) +
  labs(y="LIFE expectancy", 
       x="GDP_PC", 
       title="GDP per capital Vs Life expectancy")

# 2. (4pt) Make a similar plot, but this time use 2019 data only. 
data_2019 <- data00 %>%
  filter(time == 2019 & !is.na(GDP_PC))
ggplot(data_2019, aes(x=GDP_PC, y=lifeExpectancy)) +
  geom_point(aes(size=totalPopulation, col=region)) +
  labs(y="LIFE expectancy", 
       x="GDP_PC", 
       title="GDP per capital Vs Life expectancy")

# 3. (6pt) Compare these two plots and comment what do you see. How has world developed through the last 60 years? 

###I noticed that the GDP gap between African region and American region is getting bigger and bigger. Also, the life expectancy for 
### African countries is decreasing significantly over the years. American and European regions' life expectancy and GDP_PC
### has increased significantly over this period.

# 4. (6pt) Compute the average life expectancy for each continent in 1960 and 2019. Do the results fit with what do you see on the figures? \
# Note: here as *average*I mean just average over countries, ignore the fact that countries are of different size.\
data_2019 %>%
  filter(!is.na(lifeExpectancy & region != "")) %>%
  group_by(region) %>%
  summarise(le_avg_2019 = mean(lifeExpectancy))

data_1960 %>%
  filter(!is.na(lifeExpectancy) & region != "") %>%
  group_by(region) %>%
  summarise(le_avg_1960 = mean(lifeExpectancy))


# 5. (8pt) Compute the average LE growth from 1960-2019 across the continents. Show the results in the order of growth. Explain what do you see. \
# Hint: these data (data in long form) is not the simplest to compute growth. But you may want to check out the lag() function. And do not forget to group data by continent when using lag(), otherwise your results will be messed up! See [**https://faculty.washington.edu/otoomet/info201-book/dplyr.html#dplyr-helpers-compute**](https://faculty.washington.edu/otoomet/info201-book/dplyr.html#dplyr-helpers-compute).\
data00 %>%
  filter(!is.na(lifeExpectancy) & region != "") %>%
  group_by(region, time) %>%
  summarise(avg_lE = mean(lifeExpectancy)) %>%
  mutate(lag1_LE = lag(avg_lE, n=1, order_by=time)) %>%
  filter(time==2019) %>%
  summarise(LE_growth = avg_lE - lag1_LE) %>%
  arrange(desc(LE_growth))
### I see that Africa, Asia, and Americas are the top 3 regions with the highest average LE growth during this period.

# 6. (6pt) Show the histogram of GDP per capita for years of 1960 and 2019. Try to put both histograms on the same graph, see how well you can do it! \
data002 <- data00 %>%
  filter(!is.na(GDP_PC) & time %in% c(1960, 2019))
ggplot(data002, aes(GDP_PC, fill=as.factor(time))) +
  geom_histogram() + 
  labs(title="GDP per capita for years of 1960 and 2019")
# 7. (6pt) What was the ranking of US in terms of life expectancy in 1960 and in 2019? (When counting from top.) \
# Hint: check out the function rank()! 
# Hint2: 17 for 1960. 
world_1960 <- data00 %>%
  filter(time==1960 & name!="")
world_2019 <- data00 %>%
  filter(time==2019 & name!="")
world_1960$rank1 <- rank(world_1960$lifeExpectancy, ties.method = "first")
us_1960 <- world_1960 %>%
  select(iso2, time, rank1) %>%
  filter(iso2=="US")
world_2019$rank2 <- rank(world_2019$lifeExpectancy, ties.method="first")
us_2019 <- world_2019 %>%
  select(iso2, time, rank2) %>%
  filter(iso2=="US" )
us_1960
us_2019
# 8. (6pt) If you did this correctly, then you noticed that US ranking has been falling quite a bit. But we also have more countries in 2019–what about the relative rank divided by the corresponding number of countries that have LE data in the corresponding year? \
# Hint: 0.0904 for 1960. 
us_1960$rank/n_distinct(world_1960$name)
us_2019$rank/n_distinct(world_2019$name)

# Finally tell us how many hours did you spend on this PS.
