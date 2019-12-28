departmental_roles <- fromJSON("data/institutions/departments.json", simplifyDataFrame = TRUE) %>%
  as_tibble() %>%
  remove_extra_columns()

departmental_roles_extra_roles <- departmental_roles %>%
  select(ExtraRoles) %>% ## probably want an ID variable or something to be able to connect back to `departmental_roles`
  unnest(cols = c(ExtraRoles))

departmental_roles <- departmental_roles %>%
  select(-ExtraRoles) %>%
  mutate_at(
    c(
      "OrganizationStartDate",
      "OrganizationEndDate",
      "StartDate",
      "EndDate"
    ),
    date
  ) %>%
  mutate(
    IsCurrent = is.na(EndDate),
    EndDate = case_when(
      IsCurrent ~ today(),
      TRUE ~ EndDate
    ),
    period_in_role = interval(StartDate, EndDate),
    years_in_role = time_length(period_in_role, unit = "years")
  ) %>%
  mutate_at(
    c(
      "PortFolioEn"
    ),
    trimws
  )

remove_deputy_head_duplicates <- function(dhs) {
  dhs %>%
    select(-Parliament:-ParliamentEndDate) %>%
    distinct()
}

## for use if you want to analyse DHs surviving parliaments
deputy_heads_raw <- departmental_roles %>%
  filter(! NameEn %in% c(
    "Minister",
    "Parliamentary Secretary",
    "Critic",
    "Assistant Critic",
    "Minister of State",
    "Parliamentary Assistant",
    "Associate Minister",
    "Secretary of State"
  )) %>%
  remove_extra_columns()

deputy_heads <- deputy_heads_raw %>%
  remove_deputy_head_duplicates() %>%
  arrange(OrganizationId, PortFolioEn, StartDate) %>%
  group_by(OrganizationId, PortFolioEn, NameEn) %>%
  mutate(EndDateRaw = EndDate) %>%
  mutate(EndDate = case_when(
    EndDateIsApproximate ~ lead(StartDate) - days(1),
    TRUE ~ EndDate
  )) %>%
  ungroup() %>%
  select(OrganizationId:EndDate, EndDateRaw, EndDateIsApproximate:years_in_role) %>%
  rename(
    period_in_role_raw = period_in_role,
    years_in_role_raw = years_in_role
  ) %>%
  mutate(
    period_in_role = interval(StartDate, EndDate),
    years_in_role = time_length(period_in_role, unit = "years")
  )


