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
      category_occupied_yrs_sum = sum(time_length(period_in_office, "years")),
      dates_off = length(unlist(dates_in_office_during_period)) / n(),
      dates_on = length(unique(unlist(dates_in_office_during_period))),
      category_occupied_prop = dates_on / dates_off,
      category_occupied_yrs_prop = category_occupied_yrs_sum * category_occupied_prop,
      category_lifespan_yrs = time_length(interval(first_appearance, last_appearance), "years"),
      category_occupied_yrs = dates_on / 365.25,
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
    select(-category_occupied_yrs_sum:-category_occupied_yrs_prop)
}

cabinet_ministers %>%
  mutate(yrs_in_office = time_length(period_in_office, "years")) %>%
  ggplot() +
  geom_point(aes(x = StartDate, y = yrs_in_office)) +
  geom_smooth(aes(x = StartDate, y = yrs_in_office)) +
  colour_block_by_party()



cabinet_minister_gender_by_portfolio <- cabinet_ministers %>%
  select(PortFolioEn) %>%
  unique() %>%
  arrange(PortFolioEn) %>%
  mutate(
    M = NA,
    F = NA
  ) %>%
  pivot_longer(
    -PortFolioEn,
    names_to = "Person.Gender"
  ) %>%
  select(-value) %>%
  left_join(cabinet_ministers %>%
              summarize_roles_by_category(PortFolioEn, Person.Gender)) %>%
  mutate(count = ifelse(is.na(count), 0, count))

cabinet_minister_gender_by_organization <- cabinet_ministers %>%
  select(OrganizationLongEn) %>%
  unique() %>%
  arrange(OrganizationLongEn) %>%
  mutate(
    M = NA,
    F = NA
  ) %>%
  pivot_longer(
    -OrganizationLongEn,
    names_to = "Person.Gender"
  ) %>%
  select(-value) %>%
  left_join(cabinet_ministers %>%
              summarize_roles_by_category(OrganizationLongEn, Person.Gender)) %>%
  mutate(count = ifelse(is.na(count), 0, count))


portfolios_without_ministers_of_each_gender <- cabinet_minister_gender_by_portfolio %>%
  filter(
    PortFolioEn %in%
      (cabinet_minister_gender_by_portfolio %>%
         filter(count == 0) %>%
         pull(PortFolioEn) %>%
         unique()
      )
  )

organizations_without_ministers_of_each_gender <- cabinet_minister_gender_by_organization %>%
  filter(
    OrganizationLongEn %in%
      (cabinet_minister_gender_by_organization %>%
         filter(count == 0) %>%
         pull(OrganizationLongEn) %>%
         unique()
       )
  )



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


