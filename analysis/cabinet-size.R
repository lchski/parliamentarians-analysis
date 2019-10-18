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
    tibble::enframe(name = NULL) %>%
    rename(date_to_check = value) %>%
    mutate(
      cabinet = map(
        date_to_check, function(dtc) ministers_to_analyze %>%
          mutate(in_range = dtc %within% period_in_office) %>%
          filter(in_range) %>%
          left_join(
            (parliamentarians %>% select(Person.PersonId, ...)),
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


cabinet_details_by_lop_shuffle <- cabinet_size_by_lop_shuffle %>%
  mutate(
    cabinet_details = map(
      shuffle_date,
      function(dtc) cabinet_between_dates(
        ministers_to_analyze = ministers %>%
          filter(in_cabinet),
        start_date = dtc,
        end_date = dtc,
        include_detailed_cabinet = TRUE
      )
    ),
    ministry_details = map(
      shuffle_date,
      function(dtc) cabinet_between_dates(
        ministers_to_analyze = ministers %>%
          filter(in_ministry),
        start_date = dtc,
        end_date = dtc,
        include_detailed_cabinet = TRUE
      )
    )
  ) %>%
  unnest(cols = c(cabinet_details, ministry_details), names_sep = ".")

## TODO QA on this stuff, y'know
cabinet_details_by_lop_shuffle_qc <- cabinet_details_by_lop_shuffle %>%
  mutate(
    cabinet_size_diff = cabinet_size - cabinet_details.cabinet_size,
    ministry_size_diff = ministry_size - ministry_details.cabinet_size
  )
cabinet_details_by_lop_shuffle_qc %>%
  select(
    shuffle_date,
    cabinet_details.cabinet,
    ministry_details.cabinet,
    cabinet_size_diff,
    ministry_size_diff
  ) %>%
  filter(cabinet_size_diff > 0)



library(digest)

toJSON_vectorized = Vectorize(toJSON)
digest_vectorized = Vectorize(digest)

cabinet_size_by_day <- ministers %>%
  filter(in_cabinet) %>%
  cabinet_between_dates(include_detailed_cabinet = TRUE)

cabinet_size_by_day <- cabinet_size_by_day %>%
  mutate(
    cabinet_json = toJSON_vectorized(cabinet),
    cabinet_digest = digest_vectorized(cabinet_json)
  )

distinct_cabinets <- cabinet_size_by_day %>%
  group_by(cabinet_digest) %>%
  top_n(1, wt = date_to_check)



ministry_size_by_day <- ministers %>%
  filter(in_ministry) %>%
  cabinet_between_dates(include_detailed_cabinet = TRUE)

cabinet_size_by_day %>%
  ggplot(mapping = aes(x = date_to_check, y = cabinet_size)) +
  geom_point() +
  geom_smooth() +
  xlim(c(date("1867-07-01"), today()))

ministers %>%
  filter(in_cabinet) %>%
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

## cabinet size proportions!
p_cabinet_size_day_gender_prop <- cabinet_size_by_day %>%
  rename(M = cabinet_size_m, F = cabinet_size_f) %>%
  pivot_longer(cols = c(M, F), names_to = "gender") %>%
  ggplot(aes(x = date_to_check, y = value, fill = gender)) +
    geom_col(alpha = 0.5, position = "fill") +
    scale_x_date(limits = c(date("1867-07-01"), today())) +
    colour_block_by_party(party_bg_alpha = 0.2)


# comparing (to integrate, to accommodate tibble)

library(digest)

vectorize_digest = Vectorize(digest)

csbd <- ministers %>%
  filter(in_cabinet) %>%
  cabinet_between_dates("2017-01-01", include_detailed_cabinet = TRUE)

csbd_hashes <- csbd %>%
  select(-date_to_check) %>%
  pull()

## worksih (just takes last col), but needs integrating
cabinet_size_by_day_hashes <- cabinet_size_by_day %>%
  select(-date_to_check) %>%
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



csbd %>% slice(1:2) %>% mutate(hash = map_chr(., digest))

csbd %>% slice(1:2) %>% as.list() %>% digest()

