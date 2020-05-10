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






party_colour_mappings = c(
  "liberal" = "red",
  "conservative" = "navy",
  "ccf/ndp" = "orange",
  "bq" = "skyblue",
  "socred" = "lightgreen",
  "progressive" = "gold",
  "green" = "chartreuse4",
  "independent" = "grey"
)



## to refresh:
# zzz2_members <- members %>%
#   select(PersonRoleId, StartDate, EndDate) %>%
#   mutate(
#     index_date = map2(StartDate, EndDate, seq.Date, by = "days")
#   ) %>%
#   select(PersonRoleId, index_date) %>%
#   unnest_longer(c(index_date))
# 
# zzz2_members %>% write_csv("data/out/zzz2_members.csv.gz")

zzz2_members <- read_csv("data/out/zzz2_members.csv.gz")


zzz2_members %>%
  left_join(members %>% select(PersonRoleId, Person.Gender)) %>%
  count_group(index_date, Person.Gender) %>% ## don't try to plot this directly, crashes R
  ungroup() %>%
  arrange(index_date) %>%
  ggplot(aes(x = index_date, y = count_prop, fill = Person.Gender, colour = Person.Gender)) +
  geom_point()
  





## to refresh:
# zzz2_party <- party_members %>%
#   select(PersonRoleId, StartDate, EndDate) %>%
#   filter(EndDate >= StartDate) %>% ## have one where EndDate < StartDate, which breaks seq.Date
#   mutate(
#     index_date = map2(StartDate, EndDate, seq.Date, by = "days")
#   ) %>%
#   select(PersonRoleId, index_date) %>%
#   unnest_longer(c(index_date))
#
# zzz2_party %>% write_csv("data/out/zzz2_party.csv.gz")

zzz2_party <- read_csv("data/out/zzz2_party.csv.gz")

zzz2_party %>%
  left_join(party_members %>% select(PersonRoleId, Person.PersonId, party_simple)) %>%
  filter(Person.PersonId %in% (members %>% pull(Person.PersonId))) %>% ## filter to just MPs
  count_group(index_date, party_simple) %>%
  ungroup() %>%
  arrange(index_date) %>%
  ggplot(aes(x = index_date, y = count, fill = party_simple, colour = party_simple)) +
  geom_point()

zzz2_party %>%
  left_join(party_members %>% select(PersonRoleId, Person.PersonId, party_simple)) %>%
  filter(Person.PersonId %in% (members %>% pull(Person.PersonId))) %>% ## filter to just MPs
  count_group(index_date, party_simple) %>%
  ungroup() %>%
  arrange(index_date) %>%
  ggplot(aes(x = index_date, y = count_prop, fill = party_simple, colour = party_simple)) +
  geom_area() +
  scale_fill_manual(values = party_colour_mappings, na.value = "white") +
  scale_colour_manual(values = party_colour_mappings, na.value = "white")





## members, get the person; then get the _party_ of that person from zzz2_party with a left_join on Person.PersonId
zzz2_members %>%
  filter(index_date < "1887-07-01") %>%
  left_join(
    members %>% select(PersonRoleId, Person.PersonId)
  ) %>%
  left_join(
    party_members %>% select(Person.PersonId, )
  )



