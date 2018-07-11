library(tidyverse)

# Load data

## Mmm, CSV!
import_lop_mps <- read_csv("data/lop-mps.csv")

# Tidy data

## Separate "Type of Parliamentarian" column into multiple rows per period of MP's service
lop_mps <- import_lop_mps %>%
  separate_rows(`Type of Parliamentarian`, sep="\\)") %>%
  filter(`Type of Parliamentarian` != "") %>%
  mutate(`Type of Parliamentarian` = paste(`Type of Parliamentarian`, ")", sep = ""))

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
  select(Name, `Date of Birth`, `Political Affiliation`, `Date Appointed/Date of First Election`, age_at_first_election) %>%
  arrange(age_at_first_election)

## % of MPs first elected at age 65 or above
(age_at_election %>% filter(age_at_first_election >= 65) %>% count() %>% pull(n)) / (age_at_election %>% count() %>% pull(n))

# Summarize

age_at_election %>%
  summarize(
    mean = mean(age_at_first_election, na.rm = TRUE),
    median = median(age_at_first_election, na.rm = TRUE),
    min = min(age_at_first_election, na.rm = TRUE),
    max = max(age_at_first_election, na.rm = TRUE)
  )

# Visualize

age_at_election %>%
  ggplot(aes(x = age_at_first_election)) +
  geom_histogram(binwidth = 2)
