source("scripts/load-institutions.R")

## list current DHs
dhs_current <- deputy_heads %>%
  filter(IsCurrent)

dhs_current %>%
  count_group(OrganizationNameEn, PortFolioEn, ToBeStyledAsEn)

dhs_current_ids <- dhs_current %>% pull(PersonId) %>% unique()

deputy_heads %>%
  filter(PersonId %in% dhs_current_ids)

## visualize years in role over time
deputy_heads %>%
  select(StartDate, years_in_role_raw, Gender) %>%
  filter(Gender != "") %>%
  mutate(StartDateDecade = floor_date(StartDate, unit = "10 years")) %>%
  ggplot(aes(x = StartDate, y = years_in_role_raw)) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ Gender)

deputy_heads %>%
  select(PersonId, Gender) %>%
  distinct() %>%
  count_group(Gender) %>%
  mutate(prop = count / sum(count))

deputy_heads %>%
  select(PersonId, Gender, years_in_role) %>%
  filter(years_in_role >= 0) %>%
  group_by(PersonId, Gender) %>%
  summarize(years_as_dh = sum(years_in_role)) %>%
  group_by(Gender) %>%
  filter(Gender != "") %>%
  summarize(count = n(), years = sum(years_as_dh)) %>%
  mutate(count_prop = count / sum(count), years_prop = years / sum(years))

find_closest_date <- function(date_to_place, dates_to_compare) {
  tibble(date = dates_to_compare, source = "base") %>%
    bind_rows(list(date = date_to_place, source = "comparator")) %>%
    arrange(date) %>%
    mutate(previous_date = lag(date)) %>%
    filter(source == "comparator") %>%
    pull(previous_date)
}

find_closest_date_vectorized <- Vectorize(find_closest_date)

deputy_heads %>%
  mutate(closest_ministry_date_to_start = as_date(map_dbl(StartDate, ~ find_closest_date(., ministries$start_date)))) %>%
  mutate(closest_election_date_to_start = as_date(map_dbl(StartDate, ~ find_closest_date(., parliaments$general_election))))

summary(lm(years_in_role_raw ~ Gender, deputy_heads %>% filter(Gender != "")))
