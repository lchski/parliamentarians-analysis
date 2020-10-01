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

find_closest_date <- function(date_to_place, dates_to_compare) {
  tibble(date = dates_to_compare, source = "base") %>%
    bind_rows(list(date = date_to_place, source = "comparator")) %>%
    arrange(date) %>%
    mutate(previous_date = lag(date)) %>%
    filter(source == "comparator") %>%
    pull(previous_date)
}

find_closest_date_vectorized <- Vectorize(find_closest_date)


identify_quarter_in_range <- function(dtc, date_range) {
  if(! is.interval(date_range)) {
    break("oops, not an interval")
  }
  
  if(! is.Date(dtc)) {
    break("oops, not a date")
  }
  
  d25 <- int_end((date_range / 4) * 1)
  d50 <- int_end((date_range / 4) * 2)
  d75 <- int_end((date_range / 4) * 3)
  d100 <- int_end(date_range)
  
  quarters_to_check <- tribble(
    ~quarter, ~quarter_end_date,
    1, d25,
    2, d50,
    3, d75,
    4, d100
  ) %>%
    mutate(quarter_end_date = as_date(quarter_end_date)) %>%
    bind_rows(list(quarter = NA_real_, quarter_end_date = today() + days(1)))
  
  quarters_to_check %>%
    mutate(dist = time_length(dtc %--% quarter_end_date)) %>%
    filter(dist >= 0) %>%
    top_n(-1, dist) %>%
    pull(quarter)
}



dhs_annotated <- deputy_heads %>%
  mutate(closest_ministry_date_to_start = as_date(map_dbl(StartDate, ~ find_closest_date(., ministries$start_date)))) %>%
  mutate(closest_election_date_to_start = as_date(map_dbl(StartDate, ~ find_closest_date(., parliaments$general_election)))) %>%
  mutate(ministries_count = map_int(
    period_in_role_raw,
    function(pirr) ministries %>%
      filter(int_overlaps(period_in_role, pirr)) %>%
      summarize(count = n()) %>%
      pull(count)
  )) %>%
  mutate(parliaments_count = map_int(
    period_in_role_raw,
    function(pirr) parliaments %>%
      filter(int_overlaps(interval_from_returns_to_dissolution, pirr)) %>%
      summarize(count = n()) %>%
      pull(count)
  )) %>%
  mutate(
    ministry_quarter_at_appointment = map_dbl(StartDate, function(dtc) {
      ministry_interval <- ministries %>% 
        filter(dtc %within% period_in_role) %>%
        top_n(1, start_date) %>%
        pull(period_in_role)
      
      ## handle edge case where no ministry interval is identified.
      ## only one case fits this description, `StartDate` == "1867-01-01" (before ministries)
      if(is_empty(ministry_interval)) {
        return(1)
      }
      
      identify_quarter_in_range(dtc, ministry_interval)
    })
  )
