library(tidyverse)

# Load data

## Mmm, CSV!
import_lop_mps <- read_csv("data/lop-mps.csv")

# Tidy data

## Separate "Type of Parliamentarian" column into multiple rows per period of MP's service
lop_mps <- import_lop_mps %>%
  separate_rows(`Type of Parliamentarian`, sep="\\)") %>%
  filter(`Type of Parliamentarian` != "") %>%
  mutate(days_of_service = as.numeric(str_extract(`Years of Service`, "(\\d+)"))) %>%
  mutate(years_of_service = days_of_service / 365)

## TODO: extract type of service, service start, service end for each record

# Analyze

## Number of MPs born per year
lop_mps %>%
  transmute(date = substr(`Date of Birth`, 1, 4)) %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  arrange(date)

## Age at election
age_at_election <- import_lop_mps %>%
  filter(`Date of Birth` != "") %>%
  mutate(age_at_first_election = as.integer(substr(as.character(`Date Appointed/Date of First Election`), 1, 4)) - as.integer(substr(`Date of Birth`, 1, 4))) %>%
  mutate(years_of_service = as.numeric(str_extract(`Years of Service`, "(\\d+)")) / 365) %>%
  select(Name, `Date of Birth`, `Political Affiliation`, `Date Appointed/Date of First Election`, age_at_first_election, years_of_service) %>%
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
  geom_histogram(binwidth = 2)

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
