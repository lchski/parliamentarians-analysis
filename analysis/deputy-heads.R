source("scripts/load-institutions.R")



find_closest_date <- function(date_to_place, dates_to_compare) {
  tibble(date = dates_to_compare, source = "base") %>%
    bind_rows(list(date = date_to_place, source = "comparator")) %>%
    arrange(date) %>%
    mutate(previous_date = lag(date)) %>%
    filter(source == "comparator") %>%
    pull(previous_date)
}

find_closest_date_vectorized <- Vectorize(find_closest_date)



deputy_heads_annotated <- deputy_heads %>%
  mutate(closest_ministry_date_to_start = as_date(map_dbl(StartDate, ~ find_closest_date(., ministries$start_date)))) %>%
  mutate(closest_election_date_to_start = as_date(map_dbl(StartDate, ~ find_closest_date(., parliaments$general_election)))) %>%
  mutate(ministries_count = map_int(
    period_in_role_raw,
    function(pirr) ministries %>%
      filter(int_overlaps(period_in_role, pirr)) %>%
      summarize(count = n()) %>%
      pull(count)
  )) %>%
  mutate(parliaments_count = map_int(
    period_in_role_raw,
    function(pirr) parliaments %>%
      filter(int_overlaps(interval_from_returns_to_dissolution, pirr)) %>%
      summarize(count = n()) %>%
      pull(count)
  ))



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



summary(lm(years_in_role_raw ~ Gender, deputy_heads %>% filter(Gender != "")))
summary(lm(
  years_in_role_raw ~ Gender + decade + IsActing,  ## could also be `Gender * decade`
  deputy_heads %>%
    mutate(decade = floor_date(StartDate, "10 years")) %>%
    filter(Gender != "")
))





## deputy heads by number of positions held, first and last dates as DHs
dhs_position_counts <- deputy_heads %>%
  group_by(LastFirstName, PersonId) %>%
  summarize(
    count = n(), 
    years_raw_total = sum(years_in_role_raw),
    years_raw_min = min(years_in_role_raw),
    years_raw_max = max(years_in_role_raw),
    years_raw_avg = mean(years_in_role_raw),
  ) %>%
  left_join(deputy_heads %>%
              group_by(LastFirstName, PersonId) %>%
              top_n(-1, StartDate) %>%
              select(LastFirstName, PersonId, StartDate) %>%
              distinct() %>%
              ungroup()) %>%
  left_join(deputy_heads %>%
              group_by(LastFirstName, PersonId) %>%
              top_n(1, EndDateRaw) %>%
              select(LastFirstName, PersonId, EndDateRaw) %>%
              distinct() %>%
              ungroup())
  


deputy_heads_annotated %>% ggplot(aes(x = StartDate, y = ministries_count)) +
  geom_point() +
  geom_smooth()