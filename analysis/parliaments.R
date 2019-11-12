parliaments %>%
  select(parliament, first_sitting, first_budget, dissolution) %>%
  mutate(
    p25 = dissolution - (((dissolution - first_sitting) / 4) * 3),
    p50 = dissolution - ((dissolution - first_sitting) / 2),
    p75 = dissolution - ((dissolution - first_sitting) / 4)
  )
  

members_by_parliament <- parliaments %>%
  select(parliament, first_sitting, first_budget, dissolution) %>%
  mutate(
    p25 = dissolution - (((dissolution - first_sitting) / 4) * 3),
    p50 = dissolution - ((dissolution - first_sitting) / 2),
    p75 = dissolution - ((dissolution - first_sitting) / 4)
  ) %>%
  pivot_longer(
    c(first_sitting, first_budget, p25, p50, p75, dissolution),
    names_to = "milestone",
    values_to = "date"
  ) %>%
  mutate(members = map(date, function(dtc) members %>%
                         filter(date(dtc) %within% period_in_role) %>%
                         left_join(
                           (party_members %>%
                              filter(date(dtc) %within% period_in_role)),
                           by = c("Person.PersonId" = "Person.PersonId")
                         ) %>%
                         group_by(party_simple) %>%
                         summarize(count = n()))) %>%
  unnest(members) %>%
  group_by(parliament, milestone) %>%
  mutate(percent = count / sum(count))

## for each milestone, get a row for each party present within a Parliament
members_by_parliament %>%
  ungroup() %>%
  group_by(parliament) %>%
  complete(milestone, nesting(party_simple)) %>%
  arrange(parliament, party_simple, date) %>%
  group_by(parliament, milestone) %>%
  fill(date) %>%
  replace_na(list(count = 0, percent = 0)) %>%
  ungroup() %>%
  arrange(parliament, party_simple, date) %>%
  group_by(parliament, party_simple) %>%
  mutate(swing = count - lag(count))
