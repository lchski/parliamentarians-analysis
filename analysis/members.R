party_members <- roles %>%
  filter(NameEn == "Party Member") %>%
  filter(OrganizationTypeEn == "Party") %>% # filter out provincial experience
  remove_extra_columns()

## make some tea
party_members_by_day <- party_members %>% gather(StartDate, EndDate, key = "period_bound", value = "date") %>%
  mutate(date = date(date)) %>%
  select(Person.PersonId, PersonRoleId, OrganizationLongEn, date) %>%
  arrange(Person.PersonId, PersonRoleId, OrganizationLongEn, date) %>%
  group_by(PersonRoleId) %>%
  complete(date = full_seq(date, 1), nesting(Person.PersonId, OrganizationLongEn)) %>%
  ungroup()

party_members_by_day %>%
  mutate(grouping = paste0(Person.PersonId, OrganizationLongEn)) %>%
  group_by(grouping) %>%
  top_n(1, wt = date) %>%
  ungroup()

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



library(fuzzyjoin)

z_members <- tibble(index_date = seq.Date(as_date("1867-07-01"), as_date("1869-07-01"), "1 day")) %>%
  fuzzy_left_join(
    members %>% select(PersonRoleId, StartDate, EndDate),
    by = c(
      "index_date" = "StartDate",
      "index_date" = "EndDate"
    ),
    match_fun = list(`>=`, `<=`)
  )

z_party <- tibble(index_date = seq.Date(as_date("1867-07-01"), as_date("1869-07-01"), "1 day")) %>%
  fuzzy_left_join(
    party_members %>% select(PersonRoleId, StartDate, EndDate),
    by = c(
      "index_date" = "StartDate",
      "index_date" = "EndDate"
    ),
    match_fun = list(`>=`, `<=`)
  )

zzz_members <- tibble(index_date = seq.Date(as_date("1867-07-01"), today(), "1 day")) %>%
  fuzzy_left_join(
    members %>% select(PersonRoleId, StartDate, EndDate),
    by = c(
      "index_date" = "StartDate",
      "index_date" = "EndDate"
    ),
    match_fun = list(`>=`, `<=`)
  )

zzz_party <- tibble(index_date = seq.Date(as_date("1867-07-01"), as_date("1877-07-01"), "1 day")) %>%
  fuzzy_left_join(
    party_members %>% select(PersonRoleId, StartDate, EndDate),
    by = c(
      "index_date" = "StartDate",
      "index_date" = "EndDate"
    ),
    match_fun = list(`>=`, `<=`)
  ) %>%
  select(index_date, PersonRoleId)




zzz2_members <- members %>%
  select(PersonRoleId, StartDate, EndDate) %>%
  mutate(
    index_date = map2(StartDate, EndDate, seq.Date, by = "days")
  ) %>%
  select(PersonRoleId, index_date) %>%
  unnest_longer(c(index_date))

