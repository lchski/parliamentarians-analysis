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
  ) %>%
  mutate(id = row_number())

# Tidy data

## Separate days of service, estimate years of service
lop_mps <- import_lop_mps %>%
  mutate(days_of_service = as.numeric(str_extract(years_of_service, "(\\d+)"))) %>%
  mutate(years_of_service = days_of_service / 365)

## Separate "Type of Parliamentarian" column into multiple rows per period of MP's service
lop_mps <- lop_mps %>%
  separate_rows(role_type_of_parliamentarian, seat_riding_senatorial_division, sep="\\)") %>%
  filter(role_type_of_parliamentarian != "")

by_service <- import_lop_mps %>%
  mutate(role_type_of_parliamentarian = list(head(str_split(role_type_of_parliamentarian, "\\)")[[1]], -1)))

## TODO: extract type of service, service start, service end for each record