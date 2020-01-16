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


