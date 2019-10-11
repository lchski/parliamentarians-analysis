parliamentarians %>%
  slice(1:5) %>%
  mutate(
    date_of_first_election = map(
      Person.ElectionCandidates,
      ~ (.) %>%
        filter(IsWin) %>%
        summarize(earliest = min(ElectionDate)) %>%
        pull(earliest)
    )
  ) %>%
  select(Person.PersonId, date_of_first_election) %>%
  unnest()

parliamentarians %>%
  slice(9) %>%
  mutate(
    Person.ElectionCandidatesIsEmpty = map_lgl(
      Person.ElectionCandidates,
      ~ (.) %>%
        is_empty()
    ),
    date_of_first_election = if_else(
      Person.ElectionCandidatesIsEmpty,
      list(NA),
      map(
        Person.ElectionCandidates,
        function (ecs) ecs %>%
          filter(IsWin) %>%
          summarize(earliest = min(ElectionDate)) %>%
          pull(earliest)
      )
    )
  ) %>%
  select(Person.PersonId, Person.ElectionCandidatesIsEmpty, date_of_first_election) %>%
  unnest(cols = date_of_first_election)
