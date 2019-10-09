## Cabinet Size! Replicating: https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/primeMinisters/Cabinet
### Peeps
ministers %>%
  filter(in_ministry) %>%
  mutate(in_range = date("1993-11-04") %within% period_in_office) %>%
  filter(in_range) %>%
  left_join(parliamentarians) %>%
  select(Person.PersonId, Person.DisplayName, StartDate, EndDate, NameEn, OrganizationLongEn, in_ministry, in_cabinet) %>%
  View()

### Count unique peeps
### (good parallel for "Cabinet Size" figure—add 1 to this and you get it,
### except when the PM was counted, e.g., "1993-06-25", when Kim Campbell
### was both PM and Minister responsible for Federal-Provincial Relations.)
ministers %>%
  filter(in_ministry) %>%
  mutate(in_range = date("1993-11-04") %within% period_in_office) %>%
  filter(in_range) %>%
  pull_count_unique_people()



cabinet_between_dates <- function(
  ministers_to_analyze,
  start_date = "1867-07-01",
  end_date = today(),
  include_detailed_cabinet = FALSE,
  ...
) {
  cabinets_by_day <- full_seq(c(date(start_date), date(end_date)), period = 1) %>%
    as_tibble() %>%
    rename(date_to_check = value) %>%
    mutate(
      cabinet = map(
        date_to_check, function(dtc) ministers_to_analyze %>%
          mutate(in_range = dtc %within% period_in_office) %>%
          filter(in_range) %>%
          left_join(
            (parliamentarians %>% select(Person.PersonId, Person.Gender, ...)),
            by = c("Person.PersonId" = "Person.PersonId")
          )
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
      ),
      cabinet_size = cabinet_size_m + cabinet_size_f
    )
  
  if (! include_detailed_cabinet) {
    cabinets_by_day <- cabinets_by_day %>%
      select(-cabinet)
  }
  
  cabinets_by_day
}

system.time(
  ministers %>%
    filter(in_ministry) %>%
    cabinet_between_dates_mod(start_date = "2010-03-26", end_date = "2019-10-08")
)

system.time(
  cabinet_size_by_day <- ministers %>%
    filter(in_cabinet) %>%
    cabinet_between_dates()
)

cabinet_size_by_day <- ministers %>%
  filter(in_cabinet) %>%
  cabinet_between_dates(include_detailed_cabinet = TRUE)

ministry_size_by_day <- ministers %>%
  filter(in_ministry) %>%
  cabinet_between_dates(include_detailed_cabinet = TRUE)

cabinet_size_by_day %>%
  ggplot(mapping = aes(x = date_to_check, y = cabinet_size)) +
  geom_point() +
  geom_smooth() +
  xlim(c(date("1867-07-01"), today()))

ministers %>%
  cabinet_between_dates("2006-02-06") %>%
  ggplot(mapping = aes(x = date_to_check, y = cabinet_size)) +
  geom_point() +
  geom_smooth()


cabinet_size_by_day %>%
  pivot_longer(c(cabinet_size_m, cabinet_size_f)) %>%
  select(date_to_check, cabinet_size_key = name, cabinet_size_value = value) %>%
  ggplot(mapping = aes(x = date_to_check, y = cabinet_size_value, colour = cabinet_size_key)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(c(date("1867-07-01"), today()))

library(digest)

cabinet_size_by_day %>% filter(date_to_check > date("2019-01-01")) %>% mutate(previous = list(lag((.) %>% select(-date_to_check) %>% pull())))
cabinet_size_by_day %>% filter(date_to_check > date("2019-01-01")) %>% mutate(previous = lag((.) %>% select(-date_to_check) %>% digest(.)))

vectorize_digest = Vectorize(digest)

cabinet_size_by_day %>%
  slice(1:10) %>%
  rowwise() %>%
  mutate(hash = vectorize_digest(lag(.), serialize = TRUE))

cabinet_size_by_day %>%
  slice(1:10) %>%
  rowwise() %>%
  do(data.frame(., hash = digest(. %>% select(-date_to_check) %>% pull(), serialize = TRUE)))

cabinet_size_by_day %>%
  slice(1:10) %>%
  rowwise() %>%
  mutate(hash = digest(.))

## works, but needs integrating
cabinet_size_by_day_hashes <- cabinet_size_by_day %>%
  select(-date_to_check) %>%
  pull() %>%
  vectorize_digest()

cabinet_size_by_day <- cabinet_size_by_day %>%
  cbind(cabinet_size_by_day_hashes) %>%
  as_tibble() %>%
  rename(hash = cabinet_size_by_day_hashes) %>%
  mutate(is_same_as_previous = hash == lag(hash)) %>%
  mutate(
    is_same_as_previous = if_else(
      is.na(is_same_as_previous),
      FALSE,
      is_same_as_previous
    )
  )

cabinet_size_by_day %>%
  slice(1:10) %>%
  cbind(hashes)

cabinet_size_by_day %>%
  slice(1:10) %>%
  rowwise() %>%
  do(data.frame(., hash = . %>% select(-date_to_check) %>% pull() %>% paste0(collapse = "")))

