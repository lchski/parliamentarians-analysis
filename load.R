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

lop_mps_fixed_vars <- lop_mps %>%
  select(
    id,
    name:deceased_date,
    gender:profession,
    military_service,
    date_of_parliamentary_entry,
    years_of_service,
    days_of_service
  )

# Determine geographies for each type of service for each MP
# (`geography_id` corresponds to `role_id` in `lop_mps_role_type_of_parliamentarian_by_role`)
lop_mps_seat_riding_senatorial_division <- lop_mps %>%
  select(id, geography = seat_riding_senatorial_division) %>%
  separate_rows(geography, sep="(?<=[ 0-9])(\\))") %>%
  filter(geography != "") %>%
  separate(geography, c("geography", "period"), sep = " (\\()(?=[0-9])") %>%
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
  mutate(geography_id = paste0(id, "-", row_number())) %>%
  ungroup() %>%
  select(geography_id, geography)

# Separate "Type of Parliamentarian" column into multiple rows per period of MP's service
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
  select(id, role_id, role, period_start, period_end) %>%
  left_join(lop_mps_seat_riding_senatorial_division, by = c("role_id" = "geography_id"))

# Explode roles into days!
lop_mps_role_type_of_parliamentarian_by_role_by_day <- lop_mps_role_type_of_parliamentarian_by_role %>%
  gather(period_start, period_end, key = "period_bound", value = "date") %>%
  select(id, role_id, role, date) %>%
  arrange(id, role_id, date) %>%
  group_by(role_id) %>%
  complete(date = full_seq(date, 1), nesting(id, role)) %>%
  ungroup()


# Explode period-bound vars (`political_affiliation`, `role_minister`, `role_critic`)
# into their own tables, like `lop_mps_role_type_of_parliamentarian_by_role`. Then, in
# the per-day table, find the relevant period bounds for each day and assign accordingly.

### `role_minister`
lop_mps_role_minister_by_position <- lop_mps %>%
  select(id, minister = role_minister) %>%
  separate_rows(minister, sep="(?<=[ 0-9])(\\))") %>%
  filter(minister != "") %>%
  separate(minister, c("minister", "period"), sep = " (\\()(?=[0-9])") %>%
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
  mutate(minister_id = paste0(id, "-", row_number())) %>%
  ungroup() %>%
  select(id, minister_id, minister, period_start, period_end)

### `role_critic`
#### Warning messages:
#### 1: Expected 2 pieces. Missing pieces filled with `NA` in 2 rows [1219, 3324]. 
#### 2:  1009 failed to parse. 
#### 3:  11 failed to parse. 
lop_mps_role_critic_by_position <- lop_mps %>%
  select(id, critic = role_critic) %>%
  separate_rows(critic, sep="(?<=[ 0-9])(\\))") %>%
  filter(critic != "") %>%
  separate(critic, c("critic", "period"), sep = " (\\()(?=[0-9])") %>%
  separate(period, c("period_start", "period_end"), sep = " - ") %>%
  mutate(period_end_nchar = nchar(period_end), period_end = case_when(
    period_end_nchar == 0 ~ "FIXME",
    period_end_nchar == 4 ~ paste0(period_end, "/01/01"),
    TRUE ~ period_end
  )) %>%
  mutate(period_start_nchar = nchar(period_start), period_start = case_when(
    period_start_nchar == 0 ~ "FIXME",
    period_start_nchar == 4 ~ paste0(period_start, "/01/01"),
    period_start_nchar == 6 ~ paste0(period_start, "/01"),
    TRUE ~ period_start
  )) %>%
  mutate(
    period_start = ymd(period_start),
    period_end = ymd(period_end)
  ) %>%
  arrange(id, period_start) %>%
  group_by(id) %>%
  mutate(critic_id = paste0(id, "-", row_number())) %>%
  ungroup() %>%
  select(id, critic_id, critic, period_start, period_end)




# Visualizing

lop_mps_role_type_of_parliamentarian_by_role %>%
  left_join(lop_mps_fixed_vars) %>%
  View()

lop_mps_by_day <- lop_mps_role_type_of_parliamentarian_by_role_by_day %>%
  arrange(id) %>%
  left_join(lop_mps_fixed_vars) %>% View()


  
## dates are `nchar`:
### 13 (current parliament, round to today)
### 17 (ends in a four-digit year, round to January 1)
### 23 (complete date)

