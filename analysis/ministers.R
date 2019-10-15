cabinet_ministers <- ministers %>%
  filter(in_cabinet)

summarize_roles_by_category <- function(dataset, ...) {
  dataset %>%
    group_by(...) %>%
    mutate(dates_in_office_during_period = list(seq_date_vectorized(StartDate, EndDate, by = "days"))) %>%
    summarize(
      first_appearance = min(StartDate, na.rm = FALSE),
      last_appearance = max(EndDate, na.rm = FALSE),
      count = n(),
      category_occupied_yrs = sum(time_length(period_in_office, "years")),
      dates_off = length(unlist(dates_in_office_during_period)) / n(),
      dates_on = length(unique(unlist(dates_in_office_during_period))),
      category_occupied_prop = dates_on / dates_off,
      category_occupied_yrs_prop = category_occupied_yrs * category_occupied_prop,
      category_lifespan_yrs = time_length(interval(first_appearance, last_appearance), "years"),
      avg_length_mos =
        mean(
          time_length(period_in_office, unit = "months")
        ),
      min_length_mos = 
        min(
          time_length(period_in_office, unit = "months")
        ),
      max_length_mos = 
        max(
          time_length(period_in_office, unit = "months")
        )
    ) %>%
    select(-category_occupied_yrs:-category_occupied_yrs_prop)
}

cabinet_ministers_by_org_party_gender <- cabinet_ministers %>%
  summarize_roles_by_category(OrganizationLongEn, party_simple, Person.Gender)

cabinet_ministers %>%
  mutate(yrs_in_office = time_length(period_in_office, "years")) %>%
  ggplot() +
  geom_point(aes(x = StartDate, y = yrs_in_office)) +
  geom_smooth(aes(x = StartDate, y = yrs_in_office)) +
  colour_block_by_party()


## calculate number of years a position is actually occupied
## (or # of days, if we drop the division at the end)
## doesn't work for roles where n = 1 (lists!)
cabinet_ministers %>%
  filter(OrganizationLongEn == "Agriculture") %>%
  select(StartDate, EndDate, period_in_office) %>%
  mutate(dates = list(seq_date_vectorized(StartDate, EndDate, by = "days"))) %>%
  summarize(
    dates_off = length(unlist(dates)) / n(),
    dates_on = length(unique(unlist(dates))),
    dates_on_proportion = dates_on / dates_off
  )


