by_profession <- professions %>%
  select(Person.PersonId, NameEn, ProfessionTypeEn) %>%
  left_join(by_age_at_first_election)

by_profession %>%
  filter(ProfessionTypeEn %in% (by_profession %>%
           count_group(ProfessionTypeEn) %>%
           slice(1:8) %>%
           pull(ProfessionTypeEn))) %>%
  mutate(decade_of_first_election = floor_date(date_of_first_election, years(10))) %>%
  ggplot(aes(x = decade_of_first_election, stat(density), color = Person.Gender)) +
  geom_freqpoly(bins = 15) +
  facet_wrap(vars(ProfessionTypeEn), ncol = 2, labeller = label_wrap_gen()) +
  theme(strip.text.x = element_text(hjust = 0)) +
  xlim(as.Date(c("1860-01-01", "2020-01-01")))

by_profession %>%
  filter(NameEn %in% (by_profession %>%
                                  count_group(NameEn) %>%
                                  slice(1:12) %>%
                                  pull(NameEn))) %>%
  mutate(decade_of_first_election = floor_date(date_of_first_election, years(10))) %>%
  ggplot(aes(x = decade_of_first_election, stat(density), color = Person.Gender)) +
  geom_freqpoly(bins = 15) +
  facet_wrap(vars(NameEn), ncol = 2, labeller = label_wrap_gen()) +
  theme(strip.text.x = element_text(hjust = 0)) +
  xlim(as.Date(c("1860-01-01", "2020-01-01")))



by_profession %>%
  filter(ProfessionTypeEn %in% (by_profession %>%
                        count_group(ProfessionTypeEn) %>%
                        slice(1:8) %>%
                        pull(ProfessionTypeEn))) %>%
  mutate(decade_of_first_election = floor_date(date_of_first_election, years(10))) %>%
  ggplot(aes(x = decade_of_first_election)) + ## TODO figure out why the `fill` with `Person.Gender` changes the %s in `y`
  geom_bar(aes(y = ..prop.., group = 1)) +
  facet_wrap(vars(ProfessionTypeEn), ncol = 2, labeller = label_wrap_gen()) +
  theme(strip.text.x = element_text(hjust = 0)) +
  xlim(as.Date(c("1860-01-01", "2020-01-01")))

by_profession %>%
  filter(NameEn %in% (by_profession %>%
                                  count_group(NameEn) %>%
                                  slice(1:8) %>%
                                  pull(NameEn))) %>%
  mutate(decade_of_first_election = floor_date(date_of_first_election, years(10))) %>%
  ggplot(aes(x = decade_of_first_election)) + ## TODO figure out why the `fill` with `Person.Gender` changes the %s in `y`
  geom_bar(aes(y = ..prop.., group = 1)) +
  facet_wrap(vars(NameEn), ncol = 2, labeller = label_wrap_gen()) +
  theme(strip.text.x = element_text(hjust = 0)) +
  xlim(as.Date(c("1860-01-01", "2020-01-01")))
