by_age_at_first_election <- parliamentarians %>%
  left_join(election_candidates %>%
              group_by(Person.PersonId) %>%
              filter(IsWin) %>%
              summarize(date_of_first_election = min(ElectionDate))) %>%
  mutate(age_at_first_election = interval(Person.DateOfBirth, date_of_first_election) / years(1)) %>%
  select(
    Person.PersonId,
    Person.Gender,
    date_of_first_election,
    age_at_first_election
  )

## Histogram of age of first election
by_age_at_first_election %>%
  ggplot(aes(x = age_at_first_election)) +
  geom_histogram(binwidth = 2) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    title = "Age at first election to the House of Commons",
    x = "Age at first election",
    y = "Count",
    caption = "By @lchski with data from Library of Parliament."
  )

## Frequency polygram of age at first election by gender (freqpoly allows comparison between categorical data, i.e. gender)
by_age_at_first_election %>%
  ggplot(aes(x = age_at_first_election, stat(density), colour = Person.Gender)) +
  geom_freqpoly(binwidth = 2) +
  labs(
    title = "Age at first election to the House of Commons by gender",
    x = "Age at first election",
    y = "Density",
    caption = "By @lchski with data from Library of Parliament."
  )

by_age_at_first_election %>%
  ggplot(aes(x = date_of_first_election, y = age_at_first_election)) +
  geom_point() +
  geom_smooth()

by_age_at_first_election %>%
  ggplot(aes(x = date_of_first_election, y = age_at_first_election)) +
  geom_hex() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(vars(Person.Gender)) +
  labs(
    title = "Date of first election to the House of Commons by age at first election",
    x = "Date of first election",
    y = "Age at first election",
    caption = "By @lchski with data from Library of Parliament."
  )

election_candidates %>%
  filter(IsWin) %>%
  group_by(Person.PersonId) %>%
  top_n(1, ElectionDate) %>%
  top_n(1) %>% filter(Person.PersonId %in% tids)
