#install.packages(c("tidyverse", "terra", "sf"))
#install.packages("here")

#load packages
library(tidyverse)
library(here)

here()
here('data')

#download the data
download.file(
  "https://bit.ly/geospatial_data",
  here("data", "gapminder_data.csv")
)

# vector of numbers - numeric data type.
numeric_vector <- c(2, 6, 3)
numeric_vector

ab_vector <- c('a', 'b')
ab_vector

abcd_vector <- c(ab_vector, 'c', 'd')
abcd_vector

combined_vector <- c(abcd_vector, numeric_vector)
combined_vector
####

gapminder <- read.csv("data/gapminder_data.csv")
str(gapminder)

head(gapminder) # shows first 6  rows of the data set

summary(gapminder) # basic statistical information about each column.

# Information format differs by data type.

nrow(gapminder) # returns number of rows in a dataset

ncol(gapminder) # returns number of columns in a dataset

# This notation means: From dataset gapminder, give me column country. You can
# see that the column accessed in this way is just a vector of characters.
country_vec <- gapminder$country

head(country_vec)

year_country_gdp <- select(gapminder, year, country, gdpPercap)

head(year_country_gdp)
###

year_country_gdp <- gapminder |>
  select(year, continent, country, gdpPercap)

head(year_country_gdp)

###

year_country_gdp_euro <- gapminder %>%
  filter(continent != "Europe" & year >= 2000) %>%
  select(year, continent, country, gdpPercap)
# '&' operator (AND) - both conditions must be met

head(year_country_gdp_euro)

###

le_country_year_eurasia <- gapminder %>%
  filter (continent == "Europe" | continent == "Asia")  %>%
  select(gdpPercap, country, year)
head (le_country_year_eurasia)
nrow(le_country_year_eurasia)

###
continents <- gapminder %>%
  filter (year == 2000)  %>%
  select (year, continent)
head (continents)
continents
ggplot(data = continents) +
  geom_sf(size = 3, color = "black", fill = "cyan1") +
  labs(title = "Delft Administrative Boundary") +
  coord_sf(datum = st_crs(28992))

