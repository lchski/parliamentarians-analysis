library(tidyverse)
library(ggthemr)

ggthemr('light')

# Load data

## Mmm, CSV!
import_lop_mps <- read_csv("data/lop-mps.csv") %>%
  rename(
    picture = Picture,
    name = Name,
    birth_date = `Date of Birth`,
    birth_city = `City of Birth`,
    birth_province_region = `Province/Region of Birth`,
    birth_country = `Country of Birth`,
    deceased_date = `Deceased Date`,
    gender = Gender,
    profession = Profession,
    seat_riding_senatorial_division = `Riding/Senatorial Division`,
    seat_province_territory = `Province/Territory`,
    political_affiliation = `Political Affiliation`,
    date_of_parliamentary_entry = `Date Appointed/Date of First Election`,
    role_type_of_parliamentarian = `Type of Parliamentarian`,
    role_minister = Minister,
    role_critic = Critic,
    years_of_service = `Years of Service`,
    military_service = `Military Service`
  )

# Tidy data

## Separate "Type of Parliamentarian" column into multiple rows per period of MP's service
lop_mps <- import_lop_mps %>%
  separate_rows(role_type_of_parliamentarian, sep="\\)") %>%
  filter(role_type_of_parliamentarian != "") %>%
  mutate(days_of_service = as.numeric(str_extract(years_of_service, "(\\d+)"))) %>%
  mutate(years_of_service = days_of_service / 365)

by_service <- import_lop_mps %>%
  mutate(role_type_of_parliamentarian = list(head(str_split(role_type_of_parliamentarian, "\\)")[[1]], -1)))

## TODO: extract type of service, service start, service end for each record

# Analyze

## Number of MPs born per year
lop_mps %>%
  transmute(date = substr(birth_date, 1, 4)) %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  arrange(date)

## Age at election
age_at_election <- import_lop_mps %>%
  filter(birth_date != "") %>%
  mutate(age_at_first_election = as.integer(substr(as.character(date_of_parliamentary_entry), 1, 4)) - as.integer(substr(birth_date, 1, 4))) %>%
  mutate(years_of_service = as.numeric(str_extract(years_of_service, "(\\d+)")) / 365) %>%
  select(name, birth_date, gender, political_affiliation, date_of_parliamentary_entry, age_at_first_election, years_of_service) %>%
  arrange(age_at_first_election)

## % of MPs first elected at age 65 or above
(age_at_election %>% filter(age_at_first_election >= 65) %>% count() %>% pull(n)) / (age_at_election %>% count() %>% pull(n))

# Summarize

## Age at first election summary stats
age_at_election %>%
  summarize(
    mean_age_at_first_election = mean(age_at_first_election, na.rm = TRUE),
    median_age_at_first_election = median(age_at_first_election, na.rm = TRUE),
    min_age_at_first_election = min(age_at_first_election, na.rm = TRUE),
    max_age_at_first_election = max(age_at_first_election, na.rm = TRUE)
  )

## Years of service summary stats
age_at_election %>%
  summarize(
    mean_years_of_service = mean(years_of_service, na.rm = TRUE),
    median_years_of_service = median(years_of_service, na.rm = TRUE),
    min_years_of_service = min(years_of_service, na.rm = TRUE),
    max_years_of_service = max(years_of_service, na.rm = TRUE)
  )

# Visualize

## Histogram of age of first election
age_at_election %>%
  ggplot(aes(x = age_at_first_election)) +
  geom_histogram(binwidth = 2) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    title = "Age at first election to the House of Commons",
    x = "Age at first election",
    y = "Count",
    caption = "By @lchski with data from Library of Parliament."
  )

## Frequency polygram of age at first election by gender (freqpoly allows comparison between categorical data, i.e. gender)
age_at_election %>%
  ggplot(aes(x = age_at_first_election, stat(density), colour = gender)) +
  geom_freqpoly(binwidth = 2) +
  labs(
    title = "Age at first election to the House of Commons by gender",
    x = "Age at first election",
    y = "Density",
    caption = "By @lchski with data from Library of Parliament."
  )

## Hexplot (binned scatterplot, to indicate density) of age at first election by years of service
age_at_election %>%
  ggplot(aes(x = age_at_first_election, y = years_of_service)) +
  geom_hex() +
  geom_smooth(method = lm, se = FALSE) +
  labs(
    title = "Age at first election to the House of Commons by years of service",
    x = "Age at first election",
    y = "Years of service",
    caption = "Note: Years of service includes time in the Senate. By @lchski with data from Library of Parliament."
  )
