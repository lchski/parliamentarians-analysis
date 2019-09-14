library(tidyverse)
library(lubridate)
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

lop_mps_role_type_of_parliamentarian_by_role <- lop_mps %>%
  select(id, role = role_type_of_parliamentarian) %>%
  separate_rows(role, sep="\\)") %>%
  filter(role != "") %>%
  separate(role, c("role", "period"), sep = " \\(") %>%
  separate(period, c("period_start", "period_end"), sep = " - ") %>%
  mutate(period_end_nchar = nchar(period_end), period_end = case_when(
    period_end_nchar == 0 ~ as.character(today()),
    period_end_nchar == 4 ~ paste0(period_end, "/01/01"),
    TRUE ~ period_end
  )) %>%
  mutate(
    period_start = ymd(period_start),
    period_end = ymd(period_end)
  ) %>%
  arrange(id, period_start) %>%
  group_by(id) %>%
  mutate(role_id = paste0(id, "-", row_number())) %>%
  ungroup() %>%
  select(id, role_id, role, period_start, period_end)

seq_date <- function(x) seq(min(x), max(x), by = "day")

lop_mps_role_type_of_parliamentarian_by_role_by_day <- lop_mps_role_type_of_parliamentarian_by_role %>%
  gather(period_start, period_end, key = "period_bound", value = "date") %>%
  select(id, role_id, role, date) %>%
  arrange(id, role_id, date) %>%
  group_by(role_id) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), nesting(id, role))
  
## dates are `nchar`:
### 13 (current parliament, round to today)
### 17 (ends in a four-digit year, round to January 1)
### 23 (complete date)

