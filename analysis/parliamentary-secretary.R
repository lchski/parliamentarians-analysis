roles %>%
  filter(GroupingTitleEn %in% c(NA, "House of Commons Roles"))%>%
  filter(str_detect(NameEn, "Parliamentary Secretary|Parliamentary Assistant")) %>%
  count_group()
  View()
  

## Find the secretaries
## Could also do `ministers %>% filter(str_detect(NameEn, "Parliamentary Secretary|Parliamentary Assistant"))`
## But there are a few extra entries (~21) that that misses.
parliamentary_secretaries <- roles %>%
  filter(GroupingTitleEn %in% c(NA, "House of Commons Roles"))%>%
  filter(str_detect(NameEn, "Parliamentary Secretary|Parliamentary Assistant")) %>%
  remove_extra_columns(.) %>%
  mutate(
    period_in_office = period_in_role,
    yrs_in_office = time_length(period_in_office, "years")
  ) %>%
  left_join(
    parliamentarians %>%
      select(Person.PersonId, Person.DisplayName, Person.Gender)
  )


parl_secs_gender_by_portfolio <- parliamentary_secretaries %>%
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
  left_join(parliamentary_secretaries %>%
              summarize_roles_by_category(PortFolioEn, Person.Gender)) %>%
  mutate(count = ifelse(is.na(count), 0, count))

parl_secs_gender_by_portfolio %>%
  mutate(
    avg_length_yrs = avg_length_mos / 12,
    median_length_yrs = median_length_mos / 12,
    min_length_yrs = min_length_mos / 12,
    max_length_yrs = max_length_mos / 12
  ) %>%
  mutate_at(
    c("category_lifespan_yrs", "category_occupied_yrs", "avg_length_yrs", "avg_length_mos", "median_length_yrs", "median_length_mos", "min_length_mos", "min_length_yrs", "max_length_mos", "max_length_yrs"),
    ~ round(., digits = 2)
  ) %>% write_csv("data/out/parl_secs_gender_by_portfolio.csv")

parl_secs_gender_by_organization <- parliamentary_secretaries %>%
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
  left_join(parliamentary_secretaries %>%
              summarize_roles_by_category(OrganizationLongEn, Person.Gender)) %>%
  mutate(count = ifelse(is.na(count), 0, count))

parl_secs_gender_by_organization %>%
  mutate(
    avg_length_yrs = avg_length_mos / 12,
    median_length_yrs = median_length_mos / 12,
    min_length_yrs = min_length_mos / 12,
    max_length_yrs = max_length_mos / 12
  ) %>%
  mutate_at(
    c("category_lifespan_yrs", "category_occupied_yrs", "avg_length_yrs", "avg_length_mos", "median_length_yrs", "median_length_mos", "min_length_mos", "min_length_yrs", "max_length_mos", "max_length_yrs"),
    ~ round(., digits = 2)
  ) %>% write_csv("data/out/parl_secs_gender_by_organization.csv")
