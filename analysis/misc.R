source("load.R")

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

## Number of MP days
import_lop_mps %>%
  mutate(days_of_service = as.numeric(str_extract(years_of_service, "(\\d+)"))) %>%
  summarize(sum(days_of_service))

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


senators <- roles %>%
  filter(NameEn == "Senator") %>%
  group_by(id) %>%
  summarize(
    date_appointed = min(StartDate),
    date_left = max(EndDate)
  ) %>%
  left_join(parliamentarians_backgrounds) %>%
  select(
    id,
    DisplayName,
    dob = DateOfBirth,
    date_appointed,
    date_left,
    dod = Death.DateOfDeath
  ) %>%
  mutate(
    dob = date(dob),
    dod = date(dod),
    age_at_appt = interval(dob, date_appointed) / years(1),
    age_at_leaving = interval(dob, date_left) / years(1),
    length_of_service = interval(date_appointed, date_left) / years(1),
    died_in_office = date_left == dod
  ) %>%
  arrange(age_at_appt)

## expanding by day
roles %>%
  filter(NameEn == "Constituency Member") %>%
  mutate(EndDate = case_when(
    is.na(EndDate) ~ as.character(today()),
    TRUE ~ EndDate
  )) %>%
  mutate(role_id = row_number()) %>%
  gather(StartDate, EndDate, key = "period_bound", value = "date") %>%
  mutate(date = date(date)) %>%
  select(role_id, id, NameEn, OrganizationLongEn, date) %>%
  arrange(id, role_id, NameEn, OrganizationLongEn, date) %>%
  group_by(role_id) %>%
  complete(date = full_seq(date, 1), nesting(id, NameEn, OrganizationLongEn)) %>%
  ungroup()
