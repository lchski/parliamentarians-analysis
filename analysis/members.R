## number of unique people by party
members %>%
  select(PartyEn, Person.PersonId) %>%
  unique() %>%
  count_group(PartyEn) %>%
  left_join(simplified_party_mappings, by = c("PartyEn" = "party"))

members %>%
  filter(PartyEn == "Independent") %>%
  mutate(years_in_role = time_length(period_in_role, "years")) %>% 
  ggplot(aes(x = StartDate, y = years_in_role)) +
  geom_point()

members_by_party_time_in_office <- members %>%
  group_by(Person.PersonId, Person.DisplayName, party_simple) %>%
  mutate(
    dates_in_office_during_period = list(seq_date_vectorized(StartDate, EndDate, by = "days"))
  ) %>%
  summarize(
    days_as_member = length(unique(unlist(dates_in_office_during_period))),
    years_as_member = days_as_member / 365.25
  ) %>%
  ungroup()

## floor crossers! (and/or people who ran under different party banners)
## note: not all reflected accurately; e.g. Scott Brison isn't in here
## to correct this, we could explode to day-by-day, then pull party affiliation
## by comparing to `roles.NameEn`` == "Party Member" (preferred over Caucus Member,
## the dates correspond better to time in the role as MP).
members_by_party_time_in_office %>%
  filter(Person.PersonId %in% (members_by_party_time_in_office %>%
           count_group(Person.PersonId) %>%
           filter(count > 1) %>%
           pull(Person.PersonId))) %>%
  View()
