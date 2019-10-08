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



ministers %>%
  mutate(in_range = c(date("2019-06-01")) %within% period_in_office) %>%
  filter(in_range) %>%
  left_join(parliamentarians) %>%
  select(Person.PersonId, Person.DisplayName, StartDate, EndDate, period_in_office, NameEn, OrganizationLongEn, ToBeStyledAsEn) %>%
  View()


ministers %>%
  mutate(in_range = c(date("2019-01-01"), date("2010-01-01")) %within% period_in_office) %>%
  filter(in_range) %>%
  left_join(parliamentarians) %>%
  select(Person.PersonId, Person.DisplayName, StartDate, EndDate, period_in_office, NameEn, OrganizationLongEn, ToBeStyledAsEn) %>%
  View()

ministers %>%
  mutate(in_range = c(date("2019-01-01"), date("2010-01-01")) %within% period_in_office) %>%
  filter(in_range) %>%
  distinct(Person.PersonId) %>%
  summarize(count = n())

full_seq(c(date("1867-09-01"), date("2019-09-01")), period = 365, tol = 100)


full_seq(c(date("1867-06-01"), today()), period = 1) %>%
  as_tibble() %>%
  rename(date = value) %>%
  mutate(cabinet_size = map_dbl(
    date, ~ ministers %>%
      mutate(in_range = . %within% period_in_office) %>%
      filter(in_range) %>%
      distinct(Person.PersonId) %>%
      summarize(count = n()) %>%
      pull(count)
  ))

full_seq(c(date("1867-07-01"), today()), period = 1) %>%
  as_tibble() %>%
  rename(date_to_check = value) %>%
  slice(1) %>%
  mutate(cabinet_size = map_dbl(
    date_to_check, ~ ministers %>%
      mutate(in_range = . %within% period_in_office) %>%
      filter(in_range) %>%
      distinct(Person.PersonId) %>%
      summarize(count = n()) %>%
      pull(count)
  ))

full_seq(c(date("1867-07-01"), today()), period = 1) %>%
  as_tibble() %>%
  rename(date_to_check = value) %>%
  mutate(cabinet_size = map_dbl(
    date_to_check, function(dtc) ministers %>%
      mutate(in_range = dtc %within% period_in_office) %>%
      filter(in_range) %>%
      distinct(Person.PersonId) %>%
      summarize(count = n()) %>%
      pull(count)
  ))

ministers %>%
  mutate(in_range = date("1867-07-01") %within% period_in_office) %>%
  filter(in_range)

cabinet_size_by_day %>%
  ggplot(mapping = aes(x = date_to_check, y = cabinet_size)) +
  geom_point() +
  geom_smooth() +
  xlim(c(date("1867-07-01"), today()))

cabinet_between_dates <- function(ministers, start_date = "1867-07-01", end_date = today(), include_detailed_cabinet = FALSE) {
  cabinets_by_day <- full_seq(c(date(start_date), date(end_date)), period = 1) %>%
    as_tibble() %>%
    rename(date_to_check = value) %>%
    mutate(
      cabinet = map(
        date_to_check, function(dtc) ministers %>%
          mutate(in_range = dtc %within% period_in_office) %>%
          filter(in_range) %>%
          left_join(parliamentarians, by = c("Person.PersonId" = "Person.PersonId"))
      ),
      cabinet_size = map_dbl(
        cabinet, ~ (.) %>%
          pull_count_unique_people()
      ),
      cabinet_size_m = map_dbl(
        cabinet, ~ (.) %>%
          filter(Person.Gender == "M") %>%
          pull_count_unique_people()
      ),
      cabinet_size_f = map_dbl(
        cabinet, ~ (.) %>%
          filter(Person.Gender == "F") %>%
          pull_count_unique_people()
      )
    )
  
  if (! include_detailed_cabinet) {
    cabinets_by_day <- cabinets_by_day %>%
      select(-cabinet)
  }
  
  cabinets_by_day
}

mutate(
  ,
  cabinet_size = map_dbl(
    cabinet, ~ . %>%
      distinct(Person.PersonId) %>%
      pull_count()
  ),
  cabinet_size_m = map_dbl(
    cabinet, ~ . %>%
      filter(Person.Gender == "M") %>%
      distinct(Person.PersonId) %>%
      pull_count()
  ),
  cabinet_size_f = map_dbl(
    cabinet, ~ . %>%
      filter(Person.Gender == "F") %>%
      distinct(Person.PersonId) %>%
      pull_count()
  )
)

ministers %>%
  cabinet_between_dates("2006-02-06") %>%
  ggplot(mapping = aes(x = date_to_check, y = cabinet_size)) +
  geom_point() +
  geom_smooth()

+
  xlim(c(date("1867-07-01"), today()))


