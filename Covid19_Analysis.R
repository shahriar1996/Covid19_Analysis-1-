# Load libraries

library(tidyverse)
library(maps)


# Read covid data set

covid <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
covid


# Look at the column vetor type


glimpse(covid)


# We see date is in chr. Change it to Date formation

covid$dateRep <- as.Date(covid$dateRep, "%d/%m/%Y")

# Extract month from the date

covid <-
  covid %>% 
  mutate(
    month = format(as.Date(covid$dateRep), "%m")
  )
covid


# Look at the data set again

glimpse(covid)


# dateRed has changed to Date.

# I won't use geoId in this project, so let's get rid of that.

covid <- 
  covid %>% 
  select(-(8))
glimpse(covid)


# Let's first look in to the graph of cases vs deaths worldwide

covid %>% 
  ggplot(aes(x = cases, y = deaths, color = continentExp)) +
  geom_point()


# There is a lot of data squeezed in the left side of the graph, but we can
# clearly notice that some of the countries in America continent has more cases
# and deaths than any other countries in different continent.

# I will use scale_x_log10() function to visualize the data a little better.

covid %>% 
  ggplot(aes(x = cases, y = deaths, color = continentExp)) +
  geom_point() +
  scale_x_log10()


# Now we can see that there are a very few cases in other continent. Most cases
# and deaths are shown in the countires of America continent, than countries in
# Europe. In Asia there is also more cases, but the death is lower than other two
# continent.
 
# Let's see in which continent there is most cases and in which there are more deaths.

covid %>%
  ggplot(aes(x = continentExp, y = cases)) +
  geom_col()


# America and Europe stand out for the most cases and the lowest cases are
# captured in Oceania and Africa.


covid %>%
  ggplot(aes(x = continentExp, y = deaths)) +
  geom_col()


# In European countries there are more deaths than the American countries and in
# Oceania there is the least deaths.


# Let's look at the actual numbers of cases and deaths by continent

continent_cases_death <- 
  covid %>% 
  group_by(Continent = continentExp) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths))
continent_cases_death


# We can see the actual number here and it is clear that America and Europe have
# the most covid cases and deaths.
 
# Filtering Asia and America

Asia <-
  covid %>% 
  filter(continentExp == "Asia")
Asia


America <-
  covid %>% 
  filter(continentExp == "America")
America


# I want to see the total number of cases and deaths in Asia over the months.

Asia %>% 
  group_by(month) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths))


# Both the number of cases and deaths in Asia are increasing. Assigning the
# previous code to a variable. This code has written in the middle of May, so I
# can't conclude anything about month 5. However, it looks like the cases will be
# almost doubled from previous month and the deaths will be close to month 4.


Asia_cases_death <- 
  Asia %>% 
  group_by(month) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths)) %>% 
  ungroup()
Asia_cases_death


# I want to plot a graph to visualize the previous stats for total number of cases
# over the year

Asia_cases_death %>% 
  ggplot(aes(month, total_cases, group = 1)) +
  geom_line(color = "black") +
  xlab("Month") +
  ylab("Total cases in Asia")

# I can see that the number of cases has increased significantly starting in the 
# middle of the year.

# I want to plot a graph to visualize the previous stats for total number of deaths.

Asia_cases_death %>% 
  ggplot(aes(month, total_deaths, group = 1)) +
  geom_line(color = "red") +
  xlab("Month") +
  ylab("Total deaths in Asia")

# It shows that the deaths in Asia also increased significantly starting in the 
# middle of the year.

# Plot both cases and deaths graph together.

Asia_cases_death %>% 
  ggplot(aes(month, group = 1)) +
  geom_line(aes(y = total_cases), color = "green") +
  geom_line(aes(y = total_deaths), color = "red") +
  ylab("Total Cases vs Deaths")

# It looks like there are a lot of cases compare to deaths. 

# Use log function to see it better

Asia_cases_death %>% 
  ggplot(aes(month, group = 1)) +
  geom_line(aes(y = total_cases), color = "green") +
  geom_line(aes(y = total_deaths), color = "red") +
  ylab("Total Cases vs Deaths") +
  scale_y_log10()


# Filtering world covid data from Jan to Apr, and looking at the total cases and
# deaths.

covid %>% 
  group_by(continentExp, month) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths)) %>% 
  filter(month == "01" | month == "02" | month == "03" | month == "04")


# This is world's total cases and deaths for Jan to Mar. Assign previous code to a
# variable.

World_cases_death <-
  covid %>% 
  group_by(continentExp, month) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths)) %>% 
  filter(month == "01" | month == "02" | month == "03" | month == "04")
World_cases_death


# Plot the total cases by continent. This will help me to look at the covid cases
# by each continent.

World_cases_death %>% 
  ggplot(aes(month, total_cases, group = 1)) +
  geom_line(aes(color = continentExp)) +
  facet_wrap(~ continentExp)

# Most cases are captured in America, followed by Europe and Asia.

# Plot the total deaths by continent. This will help me to look at the covid
# deaths by each continent.

World_cases_death %>% 
  ggplot(aes(month, total_deaths, group = 1)) +
  geom_line(aes(color = continentExp)) +
  facet_wrap(~ continentExp)


# Most deaths are captured in Europe, followed by America. Compared to America and
# Europe, Asia has much lower deaths.

# Plot a graph where it will show the cases growth for each continent from Jan 1st
# to May 1st.

covid %>% 
  group_by(continentExp, dateRep) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths)) %>% 
  filter(dateRep > "2020-01-01" & dateRep < "2020-05-01") %>% 
  ggplot(aes(dateRep, total_cases, group = continentExp, color = continentExp)) +
  geom_line() +
  xlab("Month") +
  ylab("Total Cases")


# America: Cases are going up crazy. In the end of April the cases were lowering,
# but then it went up again.
# Europe: In the month of April total cases reached its peak, then it started
# decreasing.
# Asia: In the middle of Feb there were most cases captured. After that in between
# Mar and Apr the cases went down significantly. However, after Apr the cases are
# increasing.

# Plot a graph where it will show the death growth for each continent from Jan 1st
# to May 1st.

covid %>% 
  group_by(continentExp, dateRep) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths)) %>% 
  filter(dateRep > "2020-01-01" & dateRep < "2020-05-01") %>% 
  ggplot(aes(dateRep, total_deaths, group = continentExp, color = continentExp)) +
  geom_line() +
  xlab("Month") +
  ylab("Total Deaths")

# In America and Europe more people started dying in the middle of Mar and they
# has experienced there highest deaths in Apr.

# I want to visualize covid situation for China and USA.
# Filter data from Jan to Apr for China


is_china <-
  Asia %>% 
  filter((countriesAndTerritories == "China") &
           (dateRep > "2020-01-01" & dateRep < "2020-05-01"))
is_china


# Filter data from Jan to Apr for USA

is_usa <-
  America %>% 
  filter((countriesAndTerritories == "United_States_of_America") &
           (dateRep > "2020-01-01" & dateRep < "2020-05-01"))

is_usa


# I need to combine those two table to plot graphs.

is_china_usa <- 
  is_china %>% union(is_usa)


is_china_usa %>% 
  ggplot(aes(dateRep, color = countriesAndTerritories)) +
  geom_line(aes(y = cases)) +
  geom_line(aes(y = cases)) +
  xlab("Months") +
  ylab("Cases")

# From Jan to Mar China's covid cases has increased and in feb the number reached
# its peak; during that time USA didn't have much cases that we can observe. In
# the end of Mar number of cases in China has decreased significantly. In the
# middle of Mar both USA and China had almost same amount of cases. After that USA
# started to see more cases and it has increased significantly over the months
# while China did a lot better to keep the cases lower.

is_china_usa %>% 
  ggplot(aes(dateRep, color = countriesAndTerritories)) +
  geom_line(aes(y = deaths)) +
  geom_line(aes(y = deaths)) +
  xlab("Months") +
  ylab("Deaths")

# For number of deaths we can observe that China had a few deaths in Jan. In Feb
# the number of deaths started increasing. During that time USA didn't have any
# death cases. In March China has lowered the number of deaths, but in USA the
# number of deaths started increasing. In the beginning of Apr China did a few
# death cases but in the middle of the month deaths jumped to 1000+. For USA, the
# number of deaths never decreased, and it continued rising over the month.

# See the actual number of cases to match graphs.

is_china_usa %>% 
  group_by(countriesAndTerritories, month) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths))


# Plot covid19 data in worldmap.
# Read another data set.

covid19 <- 
  read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/69dfb78f504b20869ecc65dcd9b74d0e076b6af8/csse_covid_19_data/csse_covid_19_daily_reports/05-12-2020.csv")

covid19

# Get worldmap

worldmap <- map_data("world")
worldmap

worldmap %>% 
  head(1)

covid19 %>% 
  head(1)


# Plot worldmap with covid19 data

worldmap %>% 
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group), fill = 'white', color = 'black') +
  geom_point(data = covid19, aes(Long_, Lat, size = Deaths), color = 'red')

worldmap %>% 
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group), fill = 'white', color = 'black') +
  geom_point(data = covid19, aes(Long_, Lat, size = Confirmed), color = 'red')