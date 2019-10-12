parliamentarians %>%
  left_join(election_candidates %>%
              group_by(Person.PersonId) %>%
              filter(IsWin) %>%
              summarize(date_of_first_election = min(ElectionDate))) %>%
  mutate(age_at_first_election = interval(Person.DateOfBirth, date_of_first_election) / years(1))
