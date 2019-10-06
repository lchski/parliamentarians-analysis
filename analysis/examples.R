
## Pull a summary statistic from the roles column
## (note the parentheses around `.` in the `map` call)
parliamentarians %>%
  select(Person.PersonId, Person.Roles) %>%
  mutate(max_start_date = map(
    Person.Roles,
    ~ (.) %>%
      summarize(max_start_date = max(StartDate)) %>%
      pull(max_start_date)
    )
  ) %>%
  unnest(cols = c(max_start_date))