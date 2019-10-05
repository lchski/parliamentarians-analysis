## Cabinet Size! Replicating: https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/primeMinisters/Cabinet
### Peeps
ministers %>%
  mutate(in_range = date("1997-04-26") %within% period_in_office) %>%
  filter(in_range) %>%
  left_join(parliamentarians) %>%
  select(Person.PersonId, Person.DisplayName, StartDate, EndDate, NameEn, OrganizationLongEn) %>%
  View()

### Count unique peeps
### (good parallel for "Cabinet Size" figure—add 1 to this and you get it,
### except when the PM was counted, e.g., "1993-06-25", when Kim Campbell
### was both PM and Minister responsible for Federal-Provincial Relations.)
ministers %>%
  mutate(in_range = date("1997-04-26") %within% period_in_office) %>%
  filter(in_range) %>%
  distinct(Person.PersonId) %>%
  summarize(count = n())

ministers %>%
  mutate(in_range = date("1993-06-25") %within% period_in_office) %>%
  filter(in_range) %>%
  left_join(parliamentarians) %>%
  select(Person.PersonId, Person.DisplayName, StartDate, EndDate, NameEn, OrganizationLongEn, ToBeStyledAsEn) %>%
  View()

ministers %>%
  mutate(in_range = date("1993-01-03") %within% period_in_office) %>%
  filter(in_range) %>%
  distinct(Person.PersonId) %>%
  summarize(count = n())
