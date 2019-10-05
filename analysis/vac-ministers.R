## how many Ministers of Veterans Affairs had military experience?
vac_ministers <- parliamentarians %>%
  mutate(
    minister_of_va = map_lgl(
      Person.Roles,
      ~ as_tibble(.) %>%
        filter(NameEn == "Minister") %>%
        filter(OrganizationLongEn %in% c("Veterans Affairs", "Soldiers' Civil Re-establishment")) %>%
        summarize(count = n()) %>%
        mutate(had_role = count > 0) %>%
        pull(had_role)
    ),
    had_military_experience = map_lgl(
      MilitaryExperience,
      ~ as_tibble(.) %>%
        summarize(count = n()) %>%
        mutate(had_role = count > 0) %>%
        pull(had_role)
    )
  ) %>%
  filter(minister_of_va)

vac_ministers %>%
  mutate(earliest_date_as_minister = map(
    Person.Roles,
    ~ as_tibble(.) %>%
      filter(NameEn == "Minister") %>%
      filter(OrganizationLongEn %in% c("Veterans Affairs", "Soldiers' Civil Re-establishment")) %>%
      summarize(date = max(date(StartDate))) %>%
      pull(date)
  ) %>% reduce(c)) %>%
  select(
    Person.PersonId,
    Person.DisplayName,
    Person.DateOfBirth,
    earliest_date_as_minister,
    had_military_experience,
    MilitaryExperience
  ) %>%
  arrange(earliest_date_as_minister)